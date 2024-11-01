use std::collections::HashMap;

use wasm_bindgen::prelude::*;

mod lang;

// Called when the Wasm module is instantiated
#[wasm_bindgen(start)]
fn main() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    // Use `web_sys`'s global `window` function to get a handle on the global
    // window object.
    web_sys::console::log_1(&"".into());
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let body = document.body().expect("document should have a body");
    
    let program = include_str!("../example.spell");
    let tokens = lang::lexer::tokenize(program).unwrap();
    let (program, _) = lang::parser::parse(0, &mut tokens.iter()).unwrap();
    let mut ctx = HashMap::new();
    lang::eval::eval(&program, &mut ctx).unwrap();

    // Manufacture the element we're gonna append
    let val = document.create_element("p")?;
    val.set_inner_html("testing testing");

    body.append_child(&val)?;

    Ok(())
}

#[wasm_bindgen]
pub fn add(a: u32, b: u32) -> u32 {
    a + b
}
