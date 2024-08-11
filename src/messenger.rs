use futures::{future::Map, FutureExt};
use tokio::{
    io::{self, AsyncWriteExt},
    sync::{mpsc, oneshot},
};

/// Wrapper around a `TcpStream` that relays messages to the `TcpStream` via channels.
/// This lets us use the `TcpStream` from multiple `tokio::spawn`'s without locks
/// (we can clone the Messenger across threads and it will still send to the same `TcpStream`).
/// It communicates with the JS browser client via the SSE protocol.
#[derive(Clone)]
pub struct Messenger(mpsc::UnboundedSender<(Message, oneshot::Sender<Result<(), io::Error>>)>);

impl Messenger {
    pub fn new(stream: tokio::net::TcpStream) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        message_loop(stream, rx);
        Self(tx)
    }

    /// Messages will be queued in an unbounded channel, which allows `send` to be synchronous.
    /// The caller is responsible for awaiting the returned future.
    pub fn send(
        &self,
        message: Message,
    ) -> Map<
        oneshot::Receiver<Result<(), io::Error>>,
        impl FnOnce(Result<Result<(), io::Error>, oneshot::error::RecvError>) -> Result<(), io::Error>,
    > {
        let (tx, rx) = oneshot::channel();
        self.0
            .send((message, tx))
            .expect("bug: mpsc reciever closed");
        rx.map(|x| x.expect("bug: oneshot sender closed"))
    }
}

// 
fn message_loop(
    mut stream: tokio::net::TcpStream,
    mut rx: mpsc::UnboundedReceiver<(Message, oneshot::Sender<Result<(), io::Error>>)>,
) {
    tokio::spawn(async move {
        let mut id = 0;
        loop {
            let (message, tx) = rx.recv().await.expect("bug: mpsc reciever closed");
            // convert the message to SSE format
            let content = format!(
                "id: {}\nevent: {}\ndata: {}\n\n",
                id,
                message.name,
                message.content.to_string()
            );
            tx.send(stream.write_all(content.as_bytes()).await)
                .expect("bug: oneshot reciever closed");

            id += 1;
        }
    });
}

/// represents an SSE
pub struct Message {
    name: String,
    content: serde_json::Value,
}

impl Message {
    /// constructs an SSE given the name and content. Any newlines in `name` are removed
    /// (SSE uses newlines as delimiters).
    pub fn new(name: &str, content: serde_json::Value) -> Self {
        Self {
            name: name.chars().filter(|c| c != &'\n').collect(),
            content,
        }
    }
}
