use phf::phf_map;
use std::collections::HashMap;
use tokio::io::AsyncWriteExt;

pub const OK: HttpResponse = HttpResponse {
    code: 200,
    headers: None,
    body: None,
};

pub enum Body {
    StaticBytes(&'static [u8]),
    StaticString(&'static str),
}

impl Body {
    fn len(&self) -> usize {
        match &self {
            Body::StaticBytes(b) => b.len(),
            Body::StaticString(s) => s.bytes().len(),
        }
    }
}

pub enum Headers {
    Static(phf::Map<&'static str, &'static str>),
    Dynamic(HashMap<&'static str, String>),
}

impl Headers {
    fn iter(&self) -> Box<dyn Iterator<Item = (&'_ str, &'_ str)> + '_> {
        match &self {
            Headers::Static(map) => Box::new(map.entries().map(|(k, v)| (*k, *v))),
            Headers::Dynamic(map) => Box::new(map.iter().map(|(k, v)| (*k, v.as_str()))),
        }
    }
}

pub struct HttpResponse {
    pub code: u16,
    pub headers: Option<Headers>,
    pub body: Option<Body>,
}

impl HttpResponse {
    pub fn new(code: u16, headers: Option<Headers>, body: Option<Body>) -> Self {
        Self {
            code,
            headers,
            body,
        }
    }
}

fn add_http_header(bytes: &mut Vec<u8>, k: &str, v: &str) {
    bytes.extend_from_slice(k.as_bytes());
    bytes.extend_from_slice(b": ");
    bytes.extend_from_slice(v.as_bytes());
    bytes.extend_from_slice(b"\r\n");
}

const STATUS: phf::Map<u16, &'static str> = phf_map! {
    200u16 => "OK",
    400u16 => "BAD REQUEST",
    404u16 => "NOT FOUND",
    500u16 => "SERVER INTERNAL ERROR",
};

pub async fn write_http_response(
    response: &HttpResponse,
    stream: &mut tokio::net::TcpStream,
) -> Result<(), tokio::io::Error> {
    let mut bytes = vec![];
    let status = STATUS
        .get(&response.code)
        .expect("unsupported response code");
    let status_line = format!("HTTP/1.1 {} {}\r\n", response.code, status);
    bytes.extend_from_slice(status_line.as_bytes());
    if let Some(body) = &response.body {
        add_http_header(&mut bytes, "Content-Length", &format!("{}", body.len()));
    }
    if let Some(headers) = &response.headers {
        for (k, v) in headers.iter() {
            add_http_header(&mut bytes, k, v);
        }
    }
    bytes.extend_from_slice(b"\r\n");
    if let Some(body) = &response.body {
        bytes.extend_from_slice(match body {
            Body::StaticBytes(bytes) => bytes,
            Body::StaticString(str) => str.as_bytes()
        });
    }
    stream.write_all(&bytes).await
}
