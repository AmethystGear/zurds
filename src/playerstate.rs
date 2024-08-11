use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use bimap::BiMap;

use crate::{
    id::{PairId, PlayerId},
    messenger::Messenger,
};

pub struct Pair {
    pub answerer: PlayerId,
    pub offerer: PlayerId,
}

#[derive(Clone)]
pub struct Table {
    pub player_id_to_messenger: ArcRwLock<HashMap<PlayerId, Messenger>>,
    pub name_to_id: ArcRwLock<BiMap<String, PlayerId>>,
    pub pair_id_to_pair: ArcRwLock<HashMap<PairId, Pair>>,
    pub player_id_to_pair_id: ArcRwLock<HashMap<PlayerId, PairId>>,
    pub temp_reservations: ArcRwLock<HashSet<String>>,
}

impl Table {
    pub fn new() -> Self {
        Self {
            player_id_to_messenger: ArcRwLock::new(HashMap::new()),
            name_to_id: ArcRwLock::new(BiMap::new()),
            pair_id_to_pair: ArcRwLock::new(HashMap::new()),
            player_id_to_pair_id: ArcRwLock::new(HashMap::new()),
            temp_reservations: ArcRwLock::new(HashSet::new()),
        }
    }

    pub fn delete_player(&self, player: &PlayerId) {
        let mut player_id_to_messenger = self.player_id_to_messenger.write();
        let mut name_to_id = self.name_to_id.write();
        let mut pair_id_to_pair = self.pair_id_to_pair.write();
        let mut player_id_to_pair_id = self.player_id_to_pair_id.write();
        player_id_to_messenger.remove(player);
        name_to_id.remove_by_right(player);
        if let Some(pair_id) = player_id_to_pair_id.remove(player) {
            let pair = pair_id_to_pair
                .remove(&pair_id)
                .expect("bug: no pair for valid pair id");
            let opponent = if player == &pair.answerer {
                pair.offerer
            } else {
                pair.answerer
            };

            
        }
    }
}

pub struct ArcRwLock<T>(Arc<RwLock<T>>);

impl<T> Clone for ArcRwLock<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> ArcRwLock<T> {
    pub fn new(val: T) -> Self {
        Self(Arc::new(RwLock::new(val)))
    }

    pub fn read(&self) -> RwLockReadGuard<T> {
        self.0.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<T> {
        self.0.write().unwrap()
    }
}
