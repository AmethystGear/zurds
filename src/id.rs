use std::fmt::Display;

use itertools::Itertools;
use rand::Rng;
use serde::Serialize;
use std::sync::Arc;

/// macro for making an `Id` type, basically a wrapper around Arc<str> with a constructor that generates a unique id.
macro_rules! id {
    ( $x:ident ) => {
        #[derive(Hash, PartialEq, Eq, Clone, Debug)]
        pub struct $x(Arc<str>);

        impl $x {
            pub fn new() -> Self {
                Self(unique_id().into())
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

        impl Serialize for $x {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                serializer.serialize_str(&self.0)
            }
        }
    };
}

id!(PairId);
id!(PlayerId);

/// generates a random string in the form xxxx-xxxx-xxxx-xxxx, where x is a lowercase alphanumeric character.
fn unique_id() -> String {
    const CHARS: [char; 36] = [
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    ];
    (0..4)
        .map(|_| {
            (0..4)
                .map(|_| CHARS[rand::thread_rng().gen_range(0..CHARS.len())])
                .collect::<String>()
        })
        .join("-")
}
