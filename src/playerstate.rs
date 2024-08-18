use crate::{
    id::{Token, PlayerId},
    messenger::Messenger,
};
use std::{
    collections::HashMap, sync::{Arc, Mutex, MutexGuard},
};

pub enum Players {
    Single(PlayerId),
    Pair(PlayerId, PlayerId)
}

pub struct Table {
    pub messengers: HashMap<PlayerId, Messenger>,
    pub player_to_token: HashMap<PlayerId, Token>,
    pub token_to_players: HashMap<Token, Players>,
}

impl Table {
    pub fn new() -> Self {
        Self {
            messengers: HashMap::new(),
            player_to_token: HashMap::new(),
            token_to_players: HashMap::new(),
        }
    }
}

pub struct ArcMutex<T>(Arc<Mutex<T>>);

impl<T> Clone for ArcMutex<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> ArcMutex<T> {
    pub fn new(val: T) -> Self {
        Self(Arc::new(Mutex::new(val)))
    }

    pub fn lock(&self) -> MutexGuard<T> {
        self.0.lock().expect("lock poisoned")
    }
}