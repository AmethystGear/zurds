use std::time::Duration;

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
    pub async fn send(
        &self,
        message: Message,
    ) -> Result<(), MessagingError> {
        let (tx, rx) = oneshot::channel();
        if let Err(e) = self.0.send((message, tx)) {
            return Err(MessagingError::Send(e));
        }
        match rx.await {
            Ok(Ok(())) => Ok(()),
            Ok(Err(e)) => Err(MessagingError::Io(e)),
            Err(e) => Err(MessagingError::Recv(e)),
        }
    }
}

pub enum MessagingError {
    Io(std::io::Error),
    Send(mpsc::error::SendError<(Message, oneshot::Sender<Result<(), std::io::Error>>)>),
    Recv(tokio::sync::oneshot::error::RecvError)
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
        let mut id = 0;
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
                Ok(None) => panic!("mpsc sender closed"),
                Err(_) => {
                    // timed out, which means there's nothing we need to send the player right now.
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
                tx.send(res).expect("oneshot reciever closed");
            }
            if err {
                // we couldn't send to the player, so let's get rid of their data from the table.
                println!("deleting player: {}", player_id);
                let mut table = table.lock();
                table.messengers.remove(&player_id);
                if let Some(token) = table.player_to_token.remove(&player_id) {
                    if let Some(Players::Pair(a, b)) = table.token_to_players.remove(&token) {
                        if a == player_id {
                            table.player_to_token.remove(&b);
                        } else {
                            table.player_to_token.remove(&a);
                        }
                    }
                }
                break;
            }
            id += 1;
        }
    });
}

/// represents an SSE
pub struct Message {
    title: String,
    content: serde_json::Value,
}

impl Message {
    /// constructs an SSE given the title and content. Any newlines in `title` are removed
    /// (SSE uses newlines as delimiters).
    pub fn new(title: &str, content: serde_json::Value) -> Self {
        Self {
            title: title.chars().filter(|c| c != &'\n').collect(),
            content,
        }
    }
}
