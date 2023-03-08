mod format;
mod store;

//use core::ops::{Deref, DerefMut};
//use wasm_bindgen::convert::{RefMutFromWasmAbi, WasmAbi};
//use wasm_bindgen::describe::WasmDescribe;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn i32_null() -> i32 {
    return i32::MIN;
}

#[wasm_bindgen]
pub fn i64_null() -> i64 {
    return i64::MIN;
}

#[wasm_bindgen]
pub fn f32_null() -> f32 {
    return f32::NAN;
}

#[wasm_bindgen]
pub fn f64_null() -> f64 {
    return f64::NAN;
}

#[wasm_bindgen]
pub fn sin(val: f64) -> f64 {
    val.sin()
}

#[wasm_bindgen]
pub fn cos(val: f64) -> f64 {
    val.cos()
}

#[wasm_bindgen]
pub fn tan(val: f64) -> f64 {
    val.tan()
}

#[wasm_bindgen]
pub fn asin(val: f64) -> f64 {
    val.asin()
}

#[wasm_bindgen]
pub fn acos(val: f64) -> f64 {
    val.acos()
}

#[wasm_bindgen]
pub fn atan(val: f64) -> f64 {
    val.atan()
}

#[wasm_bindgen]
pub fn pi() -> f64 {
    std::f64::consts::PI
}

#[wasm_bindgen]
pub fn __force_string_functions(val: &str) -> String {
    format::force_string_functions(val)
}

const

#[wasm_bindgen]
pub fn format_text(db: &store::Store, db_ref: u32, val: &str) {
    // nothing yet
}

#[wasm_bindgen]
pub fn __split(val: u64) -> u32 {
    val as u32 + (val >> 32) as u32
}
