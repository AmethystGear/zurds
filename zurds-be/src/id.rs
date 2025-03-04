use std::{fmt::Display, iter};

use rand::Rng;
//use regex::Regex;
use serde::{Deserialize, Deserializer,Serialize};
use std::sync::Arc;

/// macro for making an `Id` type, basically a wrapper around Arc<str> with a constructor that generates a unique id.
/// implements hash, eq, clone, debug, ord, serialize, and has a custom deserialize implementation to ensure that the string is a valid id.
macro_rules! id {
    ( $x:ident ) => {
        #[derive(Hash, PartialEq, Eq, Clone, Debug, PartialOrd, Ord, Serialize)]
        pub struct $x(Arc<str>);

        impl $x {
            pub fn new() -> Self {
                Self(unique_id(stringify!($x)).into())
            }
        }

        impl Display for $x {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl<T: Into<Arc<str>>> From<T> for $x {
            fn from(value: T) -> Self {
                Self(value.into())
            }
        }

        impl<'de> Deserialize<'de> for $x {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                let id_name = camel_to_kebab(stringify!($x)) + "-";
                let s: String = Deserialize::deserialize(deserializer)?;
                if s.len() > id_name.len() {
                    let split = s[id_name.len()..].split('-').collect::<Vec<_>>();
                    if s.starts_with(&id_name) && split.len() == 4 && split.iter().all(|s| s.len() == 4 && s.chars().all(|c| c.is_ascii_digit())) {
                        return Ok(s.into())
                    }
                }
                Err(serde::de::Error::custom(format!("Invalid {}: '{}'", id_name, s)))
            }
        }        
    };
}

id!(Token);
id!(PlayerId);

// generates a random string in the form xxxx-xxxx-xxxx-xxxx, where x is 0-9.
fn unique_id(id: &str) -> String {
    const CHARS: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    iter::once(camel_to_kebab(id))
        .chain((0..4).map(|_| {
            (0..4)
                .map(|_| CHARS[rand::thread_rng().gen_range(0..CHARS.len())])
                .collect::<String>()
        })).collect::<Vec<_>>().join("-")
}

// convert ascii camelCase string to ascii kebab-case string
fn camel_to_kebab(s: &str) -> String {
    s.chars()
        .enumerate()
        .flat_map(|(i, c)| {
            if c.is_ascii_uppercase() && i != 0 {
                Some('-')
            } else {
                None
            }
            .into_iter()
            .chain(iter::once(c.to_ascii_lowercase()))
        })
        .collect()
}
