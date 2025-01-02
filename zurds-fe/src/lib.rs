use std::collections::HashMap;
use wasm_bindgen::prelude::*;
mod lang;

// Called when the Wasm module is instantiated
#[wasm_bindgen(start)]
fn main() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    let program = include_str!("../test-spells/example.spell");
    let tokens = lang::lexer::tokenize(program).unwrap();
    let program = lang::parser::parse(&mut tokens.iter()).unwrap();
    let mut ctx = HashMap::new();
    lang::eval::eval(&program, &mut ctx).unwrap();
    Ok(())
}