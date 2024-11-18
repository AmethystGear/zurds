use std::{sync::MutexGuard, time::Duration};

use rand::{thread_rng, Rng};
use tokio::{
    io::{self, AsyncWriteExt},
    sync::{mpsc, oneshot},
    time::timeout,
};

use crate::{
    id::PlayerId,
    playerstate::{ArcMutex, Players, Table},
};

/// Wrapper around a `TcpStream` that relays messages to the `TcpStream` via channels.
/// This lets us use the `TcpStream` from multiple `tokio::spawn`'s without locks
/// (we can clone the Messenger across threads and it will still send to the same `TcpStream`).
/// It communicates with the JS browser client via the SSE protocol.
#[derive(Clone)]
pub struct Messenger(mpsc::UnboundedSender<(Message, oneshot::Sender<Result<(), io::Error>>)>);

impl Messenger {
    pub fn new(table: ArcMutex<Table>, player_id: PlayerId, stream: tokio::net::TcpStream) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        message_loop(table, player_id, stream, rx);
        Self(tx)
    }

    /// Messages will be queued in an unbounded channel, and sent in that order on the provided stream.
    pub async fn send(&self, message: Message) -> Result<(), MessagingError> {
        let (tx, rx) = oneshot::channel();
        self.0
            .send((message, tx))
            .map_err(|_| MessagingError::MessageLoopUnavailable)?;
        match rx.await {
            Ok(Ok(())) => Ok(()),
            Ok(Err(e)) => Err(MessagingError::Io(e)),
            Err(_) => Err(MessagingError::MessageLoopUnavailable),
        }
    }
}

#[derive(Debug)]
pub enum MessagingError {
    Io(std::io::Error),
    MessageLoopUnavailable
}

fn message_loop(
    table: ArcMutex<Table>,
    player_id: PlayerId,
    mut stream: tokio::net::TcpStream,
    mut rx: mpsc::UnboundedReceiver<(Message, oneshot::Sender<Result<(), io::Error>>)>,
) {
    const MIN_TIMEOUT: u64 = 30;
    const MAX_TIMEOUT: u64 = 60;
    tokio::spawn(async move {
        let mut id: u64 = 0;
        loop {
            // randomized timeout, this should lower contention by spreading out
            // when we (may) need to ping the player and/or take the table lock.
            let duration = Duration::from_secs(thread_rng().gen_range(MIN_TIMEOUT..=MAX_TIMEOUT));
            let (content, tx) = match timeout(duration, rx.recv()).await {
                Ok(Some((message, tx))) => {
                    // convert the message to SSE format
                    (
                        format!(
                            "id: {}\nevent: {}\ndata: {}\n\n",
                            id,
                            message.title,
                            message.content.to_string()
                        ),
                        Some(tx),
                    )
                }
                Ok(None) => {
                    // mpsc sender closed, that means all `Messenger` objects have been dropped, which means
                    // the application has no need to talk to this player anymore. 
                    // Delete their metadata/any pairings from the table, and end the message loop.
                    println!("All `Messenger` objects for player {} have been dropped. Deleting player metadata...", player_id);
                    delete_player_metadata(table.lock(), &player_id);
                    return;
                },
                Err(_) => {
                    // timed out, which means there's nothing the application wants to send the player right now.
                    // let's ping the player to see if they're still alive...
                    (format!("id: {}\nevent: ping\ndata: {{}}\n\n", id), None)
                }
            };
            let border = "_________________________________________";
            println!(
                "{border}\nsending\n{}to player: {}\n{border}",
                content, player_id
            );
            let res = stream.write_all(content.as_bytes()).await;
            let err = res.is_err();
            if let Some(tx) = tx {
                if let Err(_) = tx.send(res) {
                    // The application was trying to send a message to the player, and it sent the message,
                    // but then the reciever was dropped. This means we have a bug in `Messenger` send()
                    panic!("bug: oneshot reciever was dropped before we could return the result of writing to the tcp stream!")
                }
            }
            if err {
                // We couldn't send the message to the player, so assume that they have disconnected.
                // Delete their metadata/any pairings from the table, and end the message loop.
                println!("Could not send message to player {}. Deleting player metadata...", player_id);
                delete_player_metadata(table.lock(), &player_id);
                return;
            }
            id += 1;
        }
    });
}

fn delete_player_metadata(mut table: MutexGuard<'_, Table>, player_id: &PlayerId) {
    table.messengers.remove(player_id);
    if let Some(token) = table.player_to_token.remove(player_id) {
        if let Some(Players::Pair(a, b)) = table.token_to_players.remove(&token) {
            if &a == player_id {
                table.player_to_token.remove(&b);
            } else {
                table.player_to_token.remove(&a);
            }
        }
    }
}

/// represents an SSE (Server Side Event)
pub struct Message {
    title: String,
    content: serde_json::Value,
}

impl Message {
    /// constructs an SSE given the title and content. Any newlines in `title` are removed
    /// (SSE uses newlines as delimiters).
    pub fn new(title: &'static str, content: serde_json::Value) -> Self {
        Self {
            title: title.chars().filter(|c| c != &'\n').collect(),
            content,
        }
    }
}
