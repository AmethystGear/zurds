macro_rules! http_err {
    ( $x:literal ) => {
        {
            use phf::phf_map;
            use crate::response::{Body, Headers, HttpResponse};
            HttpResponse {
                code: $x,
                headers: Some(Headers::Static(phf_map!("Content-Type" => "text/html"))),
                body: Some(Body::StaticString(include_str!(concat!("../res/", stringify!($x), ".html")))),
            }
        }
    };
}

pub(crate) use http_err;