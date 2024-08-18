use tokio::{
    io::{self, AsyncWriteExt},
    sync::{
        mpsc,
        oneshot,
    },
    time::timeout,
};

use crate::{
    id::PlayerId,
    playerstate::{ArcMutex, Table},
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
    ) -> Result<
        Result<Result<(), std::io::Error>, tokio::sync::oneshot::error::RecvError>,
        mpsc::error::SendError<(Message, oneshot::Sender<Result<(), std::io::Error>>)>,
    > {
        let (tx, rx) = oneshot::channel();
        self.0.send((message, tx))?;
        Ok(rx.await)
    }
}

fn message_loop(
    table: ArcMutex<Table>,
    player_id: PlayerId,
    mut stream: tokio::net::TcpStream,
    mut rx: mpsc::UnboundedReceiver<(Message, oneshot::Sender<Result<(), io::Error>>)>,
) {
    tokio::spawn(async move {
        let mut id = 0;
        loop {
            let duration = tokio::time::Duration::from_secs(60);
            if let Some((message, tx)) = timeout(duration, rx.recv())
                .await
                .expect("bug: mpsc sender closed")
            {
                // convert the message to SSE format
                let content = format!(
                    "id: {}\nevent: {}\ndata: {}\n\n",
                    id,
                    message.title,
                    message.content.to_string()
                );
                tx.send(stream.write_all(content.as_bytes()).await)
                    .expect("bug: oneshot reciever closed");
            } else {
                // timed out, let's ping the player to see if they're still alive...
                let content = format!("id: {}\nevent: ping\ndata: \n\n", id);
                if let Err(_) = stream.write_all(content.as_bytes()).await {
                    // we couldn't ping the player, so let's get rid of their data from the table.
                    let mut table = table.lock();
                    table.messengers.remove(&player_id);
                    if let Some(token) = table.player_to_token.remove(&player_id) {
                        table.token_to_players.remove(&token);
                    }
                    break;
                }
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
