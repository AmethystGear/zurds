use std::{collections::HashMap};

use err::http_err;
use id::{PairId, PlayerId};
use itertools::Itertools;
use messenger::{Message, Messenger};
use phf::phf_map;
use players::{ConnectingState, Pair, PlayerData, PlayerInfo, PlayerState};
use playerstate::Table;
use request::{parse_http_request, HttpRequest, HttpRequestParsingError};
use response::{write_http_response, Body, Headers, HttpResponse};
use serde::Deserialize;
use serde_json::json;
use tokio::sync::mpsc::{self, error::TrySendError, Sender};

mod err;
mod id;
mod messenger;
mod players;
mod request;
mod response;
mod playerstate;

const INDEX: &str = "/index.html";
const RESOURCES: phf::Map<&'static str, &'static [u8]> = incdir::include_dir!("res");

#[tokio::main]
async fn main() -> Result<(), tokio::io::Error> {
    println!("starting server on http://localhost:7878");
    let table = Table::new();
    let listener = tokio::net::TcpListener::bind("localhost:7878").await?;
    loop {
        let (stream, _) = listener.accept().await?;
        let mut table = table.clone();
        tokio::spawn(async move { handle_connection(stream, &mut table).await });
    }
}

async fn handle_connection(
    mut stream: tokio::net::TcpStream,
    table: &mut Table,
) -> Result<(), tokio::io::Error> {
    loop {
        let mut buf_reader = tokio::io::BufReader::new(&mut stream);
        let request = parse_http_request(&mut buf_reader).await;
        let mut pid: Option<PlayerId> = None;
        let response = match request {
            Ok(request) => {
                // always route "/" to the index file
                let path = if request.path == "/" {
                    INDEX
                } else {
                    &request.path
                };
                let mut split = path.split('/').filter(|elem| elem != &"").collect_vec();
                split.insert(0, &request.method);
                let response = match split[..] {
                    ["GET", "join", player_name] => {
                        return join(table, stream, player_name).await;
                    }
                    ["GET", ..] => get(&path[1..], split),
                    ["POST", function] => {
                        (|| {
                            let body: serde_json::Value = expect_json_body(&request)?;
                            match body.get("player_id").clone() {
                                Some(serde_json::Value::String(player_id)) => {
                                    pid = Some(player_id.clone().into());
                                    match function {

                                        _ => Err(http_err!(404))
                                    }
                                }
                                _ => Err(http_err!(400))
                            }
                        })()
                    }
                    _ => Err(http_err!(404)), // catch all 404
                };
                match response {
                    Ok(response) => response,
                    Err(response) => response,
                }
            }
            // error while reading from TcpStream
            Err(HttpRequestParsingError::Io(err)) => return Err(err),
            // couldn't parse the request
            Err(_) => http_err!(400),
        };
        match write_http_response(&response, &mut stream).await {
            Err(e) => {
                if let Some(player_id) = pid {
                    table.delete_player(&player_id);
                }
                return Err(e);
            }
            _ => {}
        }
    }
}

/// sends SSE response + sends player id on SSE stream.
/// returns messenger through which we can send more messages to that player
async fn init_sse(mut stream : tokio::net::TcpStream, player_id: &PlayerId) -> Result<Messenger, tokio::io::Error> {
    const SSE_HEADERS: phf::Map<&str, &str> = phf_map!(
        "Content-Type" => "text/event-stream",
        "Cache-Control" => "no-cache",
        "Connection" => "keep-alive"
    );
    let response = HttpResponse::new(200, Some(Headers::Static(SSE_HEADERS)), None);
    write_http_response(&response, &mut stream).await?;
    let messenger = Messenger::new(stream);
    messenger
        .send(Message::new("player_id", json!(player_id)))
        .await?;
    Ok(messenger)
}

/// reserves a specific player name and sets up an SSE event stream on the provided `TcpStream`,
/// and sends the player it's id via that SSE stream.
async fn join(
    table: &mut Table,
    mut stream: tokio::net::TcpStream,
    player_name: &str,
) -> Result<(), tokio::io::Error> {
    let name_reserved = {
        let name_to_id = table.name_to_id.read();
        let mut reservations = table.temp_reservations.write();
        if name_to_id.contains_left(player_name) || reservations.contains(player_name) {
            true
        } else {
            // temporarily reserving the name so that parallel requests don't cause us to 
            // overwrite player info in the table.
            reservations.insert(player_name.into());
            false
        }
    };
    if name_reserved {
        return write_http_response(&http_err!(400), &mut stream).await;
    }
    let player_id = PlayerId::new();
    let messenger = init_sse(stream, &player_id).await;
    {
        let mut name_to_id = table.name_to_id.write();
        let mut messengers = table.player_id_to_messenger.write();
        let mut reservations = table.temp_reservations.write();
        reservations.remove(player_name);
        let messenger = messenger?;
        messengers.insert(player_id.clone(), messenger);
        name_to_id.insert(player_name.into(), player_id);
    }
    Ok(())
}

// simple get for getting files from `RESOURCES`
fn get(path: &str, split: Vec<&str>) -> Result<HttpResponse, HttpResponse> {
    match RESOURCES.get(path) {
        Some(response) => {
            let mut headers = HashMap::new();
            let content_type = match split[split.len() - 1].split('.').collect_vec()[..] {
                [_, "html"] => Ok("text/html"),
                [_, "css"] => Ok("text/css"),
                [_, "txt"] => Ok("text/plain"),
                [_, "js"] => Ok("text/javascript"),
                [_, "json"] => Ok("application/json"),
                [_, "jpg" | "jpeg"] => Ok("image/jpeg"),
                [_, "png"] => Ok("image/png"),
                [_, "webp"] => Ok("image/webp"),
                _ => Err(()),
            };

            match content_type {
                Ok(content_type) => {
                    headers.insert("Content-Type", content_type.into());
                    Ok(HttpResponse {
                        code: 200,
                        headers: Some(Headers::Dynamic(headers)),
                        body: Some(Body::StaticBytes(response)),
                    })
                }
                Err(_) => Err(http_err!(404)),
            }
        }
        None => Err(http_err!(404)),
    }
}

/// expect json body, or return err 400
fn expect_json_body<'a, T: Deserialize<'a>>(request: &'a HttpRequest) -> Result<T, HttpResponse> {
    let body = request
        .body
        .as_ref()
        .map(|body| std::str::from_utf8(body).map(|body| serde_json::from_str(body)));
    match body {
        Some(Ok(Ok(body))) => Ok(body),
        _ => Err(http_err!(400)),
    }
}
