use wasm_bindgen::prelude::*;

mod lang;

// Called when the Wasm module is instantiated
#[wasm_bindgen(start)]
fn main() -> Result<(), JsValue> {
    // Use `web_sys`'s global `window` function to get a handle on the global
    // window object.
    web_sys::console::log_1(&"".into());
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let body = document.body().expect("document should have a body");
    
    let stuff = lang::lexer::tokenize("        5.95                 8 some_identifier:");

    // Manufacture the element we're gonna append
    let val = document.create_element("p")?;
    val.set_inner_html(&format!("{:?}", stuff));

    body.append_child(&val)?;

    Ok(())
}

#[wasm_bindgen]
pub fn add(a: u32, b: u32) -> u32 {
    a + b
}
