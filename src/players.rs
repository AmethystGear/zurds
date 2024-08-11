use std::{
    collections::HashMap,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use serde_json::json;
use tokio::{
    io,
    sync::oneshot,
};

use crate::{
    id::{PairId, PlayerId},
    messenger::{Message, Messenger},
};

#[derive(PartialEq, Eq)]
pub enum ConnectingState {
    Offerer,
    Answerer,
}

#[derive(PartialEq, Eq)]
pub enum PlayerState {
    Connecting(ConnectingState),
    Pairing,
    Joined,
}

pub struct PlayerInfo {
    pub name: String,
    pub state: PlayerState,
    pub messenger: Messenger,
}

#[derive(Clone)]
pub enum PairState {
    Init,
    SetupAnswerer,
    SetupOfferer,
}

#[derive(Clone)]
pub struct Pair {
    pub answerer: PlayerId,
    pub offerer: PlayerId,
    pub state: PairState,
}

pub struct PlayerData {
    pub info: HashMap<PlayerId, PlayerInfo>,
    pub names_to_player_ids: HashMap<String, PlayerId>,
    pub pairs: HashMap<PairId, Pair>,
    pub player_ids_to_pairs: HashMap<PlayerId, PairId>,
}

impl PlayerData {
    /// Removes a player from `PlayerData`, and notifies the other player in the pair that they have disconnected,
    /// if the player was paired to another player.
    pub fn delete(
        &mut self,
        player_id: &PlayerId,
    ) -> Option<
        futures::future::Map<
            oneshot::Receiver<Result<(), io::Error>>,
            impl FnOnce(
                Result<Result<(), io::Error>, oneshot::error::RecvError>,
            ) -> Result<(), io::Error>,
        >,
    > {
        let player = (|| {
            let player = self.info.remove(player_id)?;
            self.names_to_player_ids.remove(&player.name);
            let pair = self.player_ids_to_pairs.get(player_id)?;
            let pair = self.pairs.remove(pair)?;
            for player in [&pair.answerer, &pair.offerer] {
                self.player_ids_to_pairs.remove(player);
            }
            match &pair.state {
                PairState::Init => None,
                PairState::SetupAnswerer => {
                    if player_id == &pair.offerer {
                        Some(pair.answerer)
                    } else {
                        None
                    }
                }
                PairState::SetupOfferer => {
                    if player_id == &pair.offerer {
                        Some(pair.answerer)
                    } else {
                        Some(pair.offerer)
                    }
                }
            }
        })();
        player
            .and_then(|player| self.info.get(&player))
            .map(|player| player.messenger.send(Message::new("disconnected", json!({}))))
    }
}

#[derive(Clone)]
pub struct ArcRwLock<T>(pub Arc<RwLock<T>>);

impl <T> ArcRwLock<T> {
    pub fn read(&self) -> RwLockReadGuard<T> {
        self.0.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<T> {
        self.0.write().unwrap()
    }
}
