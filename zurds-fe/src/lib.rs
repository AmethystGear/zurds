use itertools::Itertools;
use rune::{
    runtime::{Object, Shared},
    Context, FromValue, Source, SourceId, Sources, Value, Vm,
};
use serde::Deserialize;
use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc, sync::Arc, thread::LocalKey};
use wasm_bindgen::prelude::*;
use web_sys::window;

struct Code {
    vm: Vm,
    sources: Sources,
    scripts: HashMap<String, String>,
}

impl Code {
    pub fn update_source(&mut self, name: &str, code: &str) {
        let context = Context::with_default_modules().unwrap();
        let runtime = Arc::new(context.runtime().unwrap());
        self.sources
            .insert(Source::new(name, code).unwrap())
            .unwrap();
        self.scripts.insert(name.to_string(), code.to_string());
        let unit = rune::prepare(&mut self.sources).build().unwrap();
        self.vm = Vm::new(runtime, Arc::new(unit));
    }
}

thread_local! {
    static CODE: RefCell<Option<Code>> = RefCell::new(None);
    static STATE: RefCell<rune::Value> = RefCell::new(rune::Value::EmptyTuple)
}

fn init_code() -> Code {
    let context = Context::with_default_modules().unwrap();
    let runtime = Arc::new(context.runtime().unwrap());

    let mut scripts = HashMap::new();
    scripts.insert(
        "lib".to_string(),
        include_str!("../res/rune/lib.rune").to_string(),
    );
    scripts.insert(
        "king".to_string(),
        include_str!("../res/rune/pieces/king.rune").to_string(),
    );
    scripts.insert(
        "vizir".to_string(),
        include_str!("../res/rune/pieces/vizir.rune").to_string(),
    );
    scripts.insert(
        "magician".to_string(),
        include_str!("../res/rune/pieces/magician.rune").to_string(),
    );
    scripts.insert(
        "martyr".to_string(),
        include_str!("../res/rune/pieces/martyr.rune").to_string(),
    );
    scripts.insert(
        "chariot".to_string(),
        include_str!("../res/rune/pieces/chariot.rune").to_string(),
    );
    scripts.insert(
        "mercenary".to_string(),
        include_str!("../res/rune/pieces/mercenary.rune").to_string(),
    );

    let mut sources = Sources::new();
    for (name, code) in &scripts {
        sources.insert(Source::new(name, code).unwrap());
    }

    let unit = rune::prepare(&mut sources).build().unwrap();
    let vm = Vm::new(runtime, Arc::new(unit));
    Code {
        vm,
        sources,
        scripts,
    }
}

// Called when the Wasm module is instantiated
#[wasm_bindgen(start)]
fn main() -> Result<(), JsValue> {
    Ok(())
}

/// update the board state
#[wasm_bindgen]
pub fn push_board_state(state: JsValue) {
    let json_data: serde_json::Value = serde_wasm_bindgen::from_value(state).unwrap();
    STATE.set(json_to_rune(&json_data))
}

/// update a piece's code
#[wasm_bindgen]
pub fn bind_code(name: &str, code: &str) {
    let c = CODE.take();
    let c = if let Some(mut c) = c {
        c.update_source(name, code);
        Some(c)
    } else {
        None
    };
    CODE.set(c);
}

/// get a piece's code
#[wasm_bindgen]
pub fn get_code(name: &str) -> String {
    let name = apply(&CODE, |code| {
        code.as_ref().unwrap().scripts.get(name).cloned()
    });
    name.unwrap()
}

/// given a board state, get all the possible moves.
/// if this returns no moves, then it's checkmate.
#[wasm_bindgen]
pub fn get_moves() -> JsValue {
    let moves = _get_moves();
    serde_wasm_bindgen::to_value(&Val::List(moves).to_json()).unwrap()
}

pub fn _get_moves() -> Vec<Rc<RefCell<Val>>> {
    //let now = time();
    let state = get_clone(&STATE);
    let moves = moves(&state);

    // remove all the moves which would put us in check
    let moves = moves
        .into_iter()
        .filter(|mov| {
            match mov {
                Value::Object(mov) => {
                    let mov = mov.borrow_ref().unwrap();
                    mov.get("state").expect("bug: invalid move escaped validation in `moves`: no ")
                    !in_check(&state)
                },
                _ => panic!("bug: invalid move escaped validation in `moves`")
            }
        })
        .collect_vec();

    //web_sys::console::log_1(&format!("sss{}", time() - now).into());
    moves
}

fn time() -> f64 {
    window()
        .and_then(|w| w.performance())
        .map(|p| p.now())
        .unwrap_or(0.0)
}

fn moves(state: &rune::Value) -> Vec<rune::Value> {
    let mut moves = vec![];
    let board = parse_state(state);

    let piece_names = apply(&CODE, |code| {
        code.as_ref().unwrap().scripts.keys().cloned().collect_vec()
    });
    for piece_name in piece_names {
        for piece in &board.pieces {
            if &piece.color == &board.turn && &piece.name == &piece_name {
                let piece_moves = apply(&CODE, |code| {
                    let code = code.as_mut().unwrap();
                    let moves_fn = format!("{}_moves", piece_name);
                    code.vm.call([moves_fn.as_str()], (piece.id, state.clone()))
                })
                .expect("error while evaluating");
                parse_moves(&piece_moves);
                match piece_moves {
                    Value::Vec(shared) => {
                        let mut vec = shared.clone().take().unwrap().into_iter().collect_vec();
                        moves.append(&mut vec);
                    }
                    _ => panic!("bug: parse_moves should have validated that this is a Vec"),
                }
            }
        }
        // TODO: add the second phase (rules fn to discard moves)
    }
    moves
}

fn in_check(state: &rune::Value) -> bool {
    let mut state_with_turn_swapped = state.clone();
    let current_turn = match &mut state_with_turn_swapped {
        rune::Value::Object(state) => {
            let turn = {
                let state = state.borrow_ref().unwrap();
                let turn = state
                    .get("turn")
                    .expect("invalid state: state is missing 'turn' field");
                match turn {
                    rune::Value::String(turn) => turn.borrow_ref().unwrap().as_str().to_string(),
                    _ => panic!("invalid state: expected 'turn' to be a string"),
                }
            };
            let mut state = state.borrow_mut().unwrap();
            state.insert(
                rune::alloc::String::try_from("turn").unwrap(),
                if turn == "white" {
                    rune::to_value("black")
                } else if turn == "black" {
                    rune::to_value("white")
                } else {
                    panic!("invalid state: turn should be 'white' or 'black'")
                }
                .unwrap(),
            );
            turn
        }
        _ => panic!("invalid state: expected state to be a dict"),
    };
    for mov in moves(&state_with_turn_swapped) {
        let mov: Move = serde_json::from_value(rune_to_json(&mov)).expect("invalid move");
        for piece in mov.state.pieces {
            if piece.name == "king" && piece.color == current_turn && piece.captured {
                // if it was our opponent's turn right now, they could capture our king!
                return true;
            }
        }
    }
    return false;
}

#[derive(Deserialize, Debug)]
struct Move {
    piece_id: usize,
    state: BoardState,
    name: String,
    loc: (usize, usize),
}

#[derive(Deserialize, Debug)]
struct BoardState {
    pub turn: String,
    pub pieces: Vec<Piece>,
}

#[derive(Deserialize, Debug)]
struct Piece {
    pub id: usize,
    pub name: String,
    pub color: String,
    pub loc: (usize, usize),
    pub captured: bool,
}

/// parse out the fields that we care about from the state of the game, and validate them.
/// this way pieces can track additional state that we don't care about,
/// but we still validate the basic game state.
fn parse_state(state: &rune::Value) -> BoardState {
    let state = rune_to_json(state);
    let state: BoardState = serde_json::from_value(state).expect("invalid board state");
    for a in &state.pieces {
        // off of the board
        if a.loc.0 > 7 || a.loc.1 > 7 {
            panic!(
                "invalid board state, there is a piece that is off the board {:?}",
                a
            )
        }
        for b in &state.pieces {
            if a.id == b.id {
                continue;
            }
            // pieces are on top of each other
            if a.loc.0 == b.loc.0 && a.loc.1 == b.loc.1 {
                if a.captured || b.captured {
                    continue;
                }
                panic!(
                    "invalid board state, there are pieces on top of each other {:?} {:?}",
                    a, b
                )
            }
        }
    }
    if state.turn != "white" && state.turn != "black" {
        panic!("invalid board state, turn should be 'white' or 'black'");
    }
    state
}

fn parse_moves(moves: &rune::Value) -> Vec<Move> {
    let moves = rune_to_json(moves);
    let moves: Vec<Move> = serde_json::from_value(moves).expect("invalid moves");
    for (i, a) in moves.iter().enumerate() {
        if a.loc.0 > 7 || a.loc.1 > 7 {
            panic!("invalid move {:?}, out of bounds", a);
        }
        for (j, b) in moves.iter().enumerate() {
            if i == j {
                continue;
            }
            if a.loc.0 == b.loc.0
                && a.loc.1 == b.loc.1
                && a.piece_id == b.piece_id
                && a.name == b.name
            {
                panic!(
                    "there are moves with the same name, location, and piece {:?}, {:?}",
                    a, b
                );
            }
        }
    }
    moves
}


fn get_clone<T: Clone + Default>(val: &'static LocalKey<RefCell<T>>) -> T {
    let v = val.take();
    let v_clone = v.clone();
    val.set(v);
    v_clone
}

fn apply<T: Default, V>(val: &'static LocalKey<RefCell<T>>, get: impl FnOnce(&mut T) -> V) -> V {
    let mut t = val.take();
    let v = get(&mut t);
    val.set(t);
    v
}

fn json_to_rune(json_value: &serde_json::Value) -> rune::Value {
    match json_value {
        serde_json::Value::Null => panic!("cannot translate nulls to rune"),
        serde_json::Value::Bool(b) => rune::Value::Bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                rune::Value::Integer(i)
            } else if let Some(f) = n.as_f64() {
                rune::Value::Float(f)
            } else {
                unreachable!()
            }
        }
        serde_json::Value::String(s) => {
            rune::to_value(s.clone()).expect("failed to convert string")
        }
        serde_json::Value::Array(arr) => {
            let rune_arr: Vec<rune::Value> = arr.into_iter().map(json_to_rune).collect();
            rune::to_value(rune_arr).unwrap()
        }
        serde_json::Value::Object(obj) => {
            let rune_obj: std::collections::HashMap<String, rune::Value> = obj
                .into_iter()
                .map(|(k, v)| (k.clone(), json_to_rune(v)))
                .collect();
            rune::to_value(rune_obj).unwrap()
        }
    }
}

fn rune_to_json(rune_value: &rune::Value) -> serde_json::Value {
    match rune_value {
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Integer(i) => serde_json::Value::from(*i),
        Value::Float(f) => serde_json::Value::from(*f),
        Value::String(s) => serde_json::Value::String(s.clone().take().unwrap().into()),
        Value::Vec(vec) => {
            let json_arr: Vec<serde_json::Value> = vec
                .clone()
                .take()
                .unwrap()
                .iter()
                .map(rune_to_json)
                .collect();
            serde_json::Value::Array(json_arr)
        }
        Value::Object(obj) => {
            let json_map: serde_json::Map<String, serde_json::Value> = obj
                .clone()
                .take()
                .unwrap()
                .into_iter()
                .map(|(k, v)| (k.into(), rune_to_json(&v)))
                .collect();
            serde_json::Value::Object(json_map)
        }
        _ => panic!("can't support converting other rune types to json"),
    }
}
