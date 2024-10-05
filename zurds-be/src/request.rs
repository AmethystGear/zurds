use std::collections::HashMap;

use tokio::io::{AsyncBufReadExt, AsyncReadExt};

pub struct HttpRequest {
    pub method: String,
    pub path: String,
    pub http_version: String,
    pub headers: HashMap<String, String>,
    pub body: Option<Vec<u8>>,
}

pub enum HttpRequestParsingError {
    Io(tokio::io::Error),
    InvalidRequestFormat
}

impl From<tokio::io::Error> for HttpRequestParsingError {
    fn from(err: tokio::io::Error) -> Self {
        Self::Io(err)
    }
}

pub async fn parse_http_request(
    buf_reader: &mut tokio::io::BufReader<&mut tokio::net::TcpStream>,
) -> Result<HttpRequest, HttpRequestParsingError> {
    fn ok<T>(val: Option<T>) -> Result<T, HttpRequestParsingError> {
        val.ok_or(HttpRequestParsingError::InvalidRequestFormat)
    }

    // Split the request into lines
    let mut lines = buf_reader.lines();

    // extract method, path, HTTP version
    let request_line = ok(lines.next_line().await?)?;
    let mut parts = request_line.split_whitespace();
    let method = ok(parts.next())?.to_string();
    let path = ok(parts.next())?.to_string();
    let http_version = ok(parts.next())?.to_string();

    // extract headers
    let mut headers = HashMap::new();
    loop {
        let header_line = ok(lines.next_line().await?)?;
        if header_line.is_empty() {
            break;
        }
        let mut parts = header_line.splitn(2, ':');
        let k = ok(parts.next())?.trim().to_string();
        let v = ok(parts.next())?.trim().to_string();
        headers.insert(k, v);
    }

    // extract body, assume no body if there is no `Content-Length` header
    let body = if let Some(len) = headers.get("Content-Length") {
        let len = ok(len.parse().ok())?;
        let mut buf = vec![0; len];
        buf_reader.read_exact(&mut buf).await?;
        Some(buf)
    } else {
        None
    };

    // Return the parsed request as a struct
    Ok(HttpRequest {
        method,
        path,
        http_version,
        headers,
        body,
    })
}
