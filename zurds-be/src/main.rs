use std::collections::HashMap;

use err::http_err;
use id::{PlayerId, Token};
use messenger::{Message, Messenger};
use phf::phf_map;
use playerstate::{ArcMutex, Players, Table};
use request::{parse_http_request, HttpRequest, HttpRequestParsingError};
use response::{write_http_response, Body, Headers, HttpResponse};
use serde::{de::DeserializeOwned, Deserialize};
use serde_json::json;
use tokio::net::{TcpListener, TcpStream};

mod err;
mod id;
mod messenger;
mod playerstate;
mod request;
mod response;

const INDEX: &str = "/index.html";
const PKG: phf::Map<&'static str, &'static [u8]> = incdir::include_dir!("../zurds-fe/pkg");

#[tokio::main]
async fn main() -> Result<(), tokio::io::Error> {
    // propogate panics in tokio tasks so that whole application crashes if we get panics anywhere.
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        default_panic(info);
        std::process::exit(1);
    }));

    println!("starting server on http://localhost:7878");
    let table = ArcMutex::new(Table::new());
    let listener = TcpListener::bind("localhost:7878").await?;
    loop {
        let table = table.clone();
        let (stream, _) = listener.accept().await?;
        tokio::spawn(async move {
            println!("{:?}", handle_connection(stream, table).await);
        });
    }
}

async fn handle_connection(
    mut stream: TcpStream,
    table: ArcMutex<Table>,
) -> Result<(), tokio::io::Error> {
    loop {
        let table = table.clone();
        let mut buf_reader = tokio::io::BufReader::new(&mut stream);
        let request = parse_http_request(&mut buf_reader).await;
        match request {
            Ok(request) => {
                // always route "/" to the index file
                let path = if request.path == "/" {
                    INDEX
                } else {
                    &request.path
                };
                let mut split: Vec<_> = path.split('/').filter(|elem| elem != &"").collect();
                split.insert(0, &request.method);
                match &split[..] {
                    ["GET", "player"] => {
                        return player(table, stream).await;
                    }
                    ["GET", path @ ..] => get(path, &mut stream).await?,
                    ["POST", path @ ..] => post(table, path, &request, &mut stream).await?,
                    _ => write_http_response(&http_err!(404), &mut stream).await?, // catch all 404
                };
            }
            // error while reading from TcpStream
            Err(HttpRequestParsingError::Io(e)) => return Err(e),
            // couldn't parse the request
            Err(HttpRequestParsingError::InvalidRequestFormat) => {
                write_http_response(&http_err!(400), &mut stream).await?
            }
        };
    }
}

/// sends SSE response + sends player id on SSE stream.
async fn player(table: ArcMutex<Table>, mut stream: TcpStream) -> Result<(), tokio::io::Error> {
    let player_id = PlayerId::new();
    const SSE_HEADERS: phf::Map<&str, &str> = phf_map!(
        "Content-Type" => "text/event-stream",
        "Cache-Control" => "no-cache",
        "Connection" => "keep-alive"
    );
    let response = HttpResponse::new(200, Some(Headers::Static(SSE_HEADERS)), None);
    write_http_response(&response, &mut stream).await?;
    let messenger = Messenger::new(table.clone(), player_id.clone(), stream);
    match messenger
        .send(Message::new("playerId", json!(player_id)))
        .await
    {
        Ok(()) => {} // successfully sent message
        Err(messenger::MessagingError::Io(e)) => return Err(e), // error while writing to TcpStream
        Err(_) => return Ok(()) // couldn't send message to message loop
    }
    {
        let mut table = table.lock();
        table.messengers.insert(player_id.clone(), messenger);
    }
    Ok(())
}

// Simple get for getting files `PKG` directory.
async fn get(path: &[&str], stream: &mut TcpStream) -> Result<(), tokio::io::Error> {
    let response = match PKG.get(&path.join("/")) {
        Some(response) => {
            let mut headers = HashMap::new();
            let content_type = match path.last().map(|x| x.split('.').last()).flatten() {
                Some(ext) => match ext {
                    "html" => Ok("text/html"),
                    "css" => Ok("text/css"),
                    "txt" => Ok("text/plain"),
                    "js" => Ok("text/javascript"),
                    "json" => Ok("application/json"),
                    "jpg" | "jpeg" => Ok("image/jpeg"),
                    "png" => Ok("image/png"),
                    "webp" => Ok("image/webp"),
                    "wasm" => Ok("application/wasm"),
                    "ico" => Ok("image/x-icon"),
                    _ => Err(()),
                },
                _ => Err(()),
            };

            match content_type {
                Ok(content_type) => {
                    headers.insert("Content-Type", content_type.into());
                    HttpResponse {
                        code: 200,
                        headers: Some(Headers::Dynamic(headers)),
                        body: Some(Body::Static(response)),
                    }
                }
                Err(_) => http_err!(404),
            }
        }
        _ => http_err!(404),
    };
    write_http_response(&response, stream).await
}

/// expect json body, or return err 400
fn expect_json_body<'a, T: Deserialize<'a>>(request: &'a HttpRequest) -> Result<T, HttpResponse> {
    let body = request
        .body
        .as_ref()
        .map(|body| std::str::from_utf8(body).map(|body| serde_json::from_str(body)));
    match body {
        Some(Ok(Ok(body))) => Ok(body),
        _ => Err(HttpResponse::json(
            400,
            json!({"err" : "invalid request: body is not valid json"}),
        )),
    }
}

fn parse_body<'a, T: DeserializeOwned>(body: serde_json::Value) -> Result<T, HttpResponse> {
    match serde_json::from_value(body) {
        Ok(body) => Ok(body),
        _ => Err(HttpResponse::json(
            400,
            json!({"err" : "invalid request: request does not match expected fields"}),
        )),
    }
}

fn vend(table: ArcMutex<Table>, body: serde_json::Value) -> Result<HttpResponse, HttpResponse> {
    #[derive(Deserialize)]
    struct Vend {
        player_id: PlayerId,
    }
    let body: Vend = parse_body(body)?;
    let mut table = table.lock();
    if table.messengers.get(&body.player_id).is_none() {
        Err(HttpResponse::json(
            400,
            json!({"err" : "provided player id is invalid"}),
        ))?
    }
    if let Some(token) = table.player_to_token.get(&body.player_id).cloned() {
        table.token_to_players.remove(&token);
    }
    let token = Token::new();
    table
        .player_to_token
        .insert(body.player_id.clone(), token.clone());
    table
        .token_to_players
        .insert(token.clone(), Players::Single(body.player_id));
    Ok(HttpResponse::json(200, json!({"token" : token})))
}

fn accept(table: ArcMutex<Table>, body: serde_json::Value) -> Result<HttpResponse, HttpResponse> {
    #[derive(Deserialize)]
    struct Accept {
        player_id: PlayerId,
        token: Token,
    }
    let body: Accept = parse_body(body)?;
    let mut table = table.lock();
    if table.messengers.get(&body.player_id).is_none() {
        Err(HttpResponse::json(
            400,
            json!({"err" : "provided player id is invalid"}),
        ))?
    }
    if let Some(Players::Single(challenger)) = table.token_to_players.get(&body.token).cloned() {
        if challenger != body.player_id {
            table
                .player_to_token
                .insert(body.player_id.clone(), body.token.clone());
            table
                .token_to_players
                .insert(body.token, Players::Pair(challenger, body.player_id));
            Ok(HttpResponse::json(200, json!({})))
        } else {
            Err(HttpResponse::json(
                400,
                json!({"err" : "you can't fight yourself"}),
            ))
        }
    } else {
        Err(HttpResponse::json(
            400,
            json!({"err" : "either the token is invalid, the opponent has disconnected, or the opponent has joined a different game"}),
        ))
    }
}

fn reduce<T>(res: Result<T, T>) -> T {
    match res {
        Ok(t) => t,
        Err(t) => t,
    }
}

async fn msg(
    table: ArcMutex<Table>,
    body: serde_json::Value,
    stream: &mut TcpStream,
) -> Result<(), std::io::Error> {
    #[derive(Deserialize)]
    struct Msg {
        player_id: PlayerId,
        content: serde_json::Value,
    }
    let response = (|| {
        let body: Msg = parse_body(body)?;
        let table = table.lock();
        if table.messengers.get(&body.player_id).is_none() {
            Err(HttpResponse::json(
                400,
                json!({"err" : "invalid player id"}),
            ))?
        }
        let messenger = match table
            .player_to_token
            .get(&body.player_id)
            .map(|token| table.token_to_players.get(token))
        {
            Some(Some(Players::Pair(challenger, acceptor))) => if acceptor == &body.player_id {
                table.messengers.get(&challenger)
            } else {
                table.messengers.get(&acceptor)
            }
            .cloned()
            .expect("table in invalid state: token_to_players map contains an invalid player id."),
            _ => Err(HttpResponse::json(
                400,
                json!({"err" : "player is not paired to anyone"}),
            ))?,
        };
        Ok((body, messenger))
    })();
    write_http_response(
        &match response {
            Err(e) => e,
            Ok((body, messenger)) => {
                let send = messenger.send(Message::new("msg", body.content)).await;
                match send {
                    Ok(()) => HttpResponse::json(200, json!({})),
                    err => HttpResponse::json(
                        400,
                        json!({"err" : format!("failed to send message to opponent {:?}", err)}),
                    ),
                }
            }
        },
        stream,
    )
    .await
}

async fn respond(
    table: ArcMutex<Table>,
    body: serde_json::Value,
    stream: &mut TcpStream,
    response_fn: fn(ArcMutex<Table>, serde_json::Value) -> Result<HttpResponse, HttpResponse>,
) -> Result<(), std::io::Error> {
    let response = reduce(response_fn(table, body));
    write_http_response(&response, stream).await
}

async fn post(
    table: ArcMutex<Table>,
    path: &[&str],
    request: &HttpRequest,
    stream: &mut TcpStream,
) -> Result<(), tokio::io::Error> {
    let body = expect_json_body(request);
    match body {
        Ok(body) => match path {
            ["vend"] => respond(table, body, stream, vend).await,
            ["accept"] => respond(table, body, stream, accept).await,
            ["msg"] => msg(table, body, stream).await,
            _ => write_http_response(&http_err!(404), stream).await,
        },
        Err(e) => write_http_response(&e, stream).await,
    }
}
