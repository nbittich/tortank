use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use tortank::turtle::turtle_doc::{Node, RdfJsonNodeResult};
use wasm_bindgen::prelude::*;

thread_local! {
    static JS_UUID_FN: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
    static JS_UUID_ERR: RefCell<Option<JsValue>> = const { RefCell::new(None) };
}

fn js_uuid_gen() -> String {
    let f = JS_UUID_FN.with(|f| f.borrow().clone());
    let res = match f {
        Some(f) => f.call0(&JsValue::NULL).and_then(|v| {
            v.as_string()
                .ok_or_else(|| js_err("uuid function must return a string"))
        }),
        None => Err(js_err("uuid function not registered")),
    };
    res.unwrap_or_else(|e| {
        JS_UUID_ERR.with(|slot| {
            slot.borrow_mut().get_or_insert(e);
        });
        String::new()
    })
}

pub fn take_uuid_err() -> Result<(), JsValue> {
    match JS_UUID_ERR.with(|slot| slot.borrow_mut().take()) {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

/// Registers the JS function for the lifetime of the guard (cleared on drop).
pub struct UuidFnGuard;

impl Drop for UuidFnGuard {
    fn drop(&mut self) {
        JS_UUID_FN.with(|f| *f.borrow_mut() = None);
        JS_UUID_ERR.with(|e| *e.borrow_mut() = None);
    }
}

pub fn install_uuid_fn(
    f: Option<js_sys::Function>,
) -> (Option<fn() -> String>, Option<UuidFnGuard>) {
    match f {
        Some(f) => {
            JS_UUID_FN.with(|slot| *slot.borrow_mut() = Some(f));
            (Some(js_uuid_gen as fn() -> String), Some(UuidFnGuard))
        }
        None => (None, None),
    }
}

pub fn js_err<E: std::fmt::Display>(err: E) -> JsValue {
    js_sys::Error::new(&err.to_string()).into()
}

pub fn to_js<T: Serialize>(value: &T) -> Result<JsValue, JsValue> {
    serde_wasm_bindgen::to_value(value).map_err(js_err)
}

pub fn from_js<T>(value: JsValue) -> Result<T, JsValue>
where
    T: for<'de> Deserialize<'de>,
{
    serde_wasm_bindgen::from_value(value).map_err(js_err)
}

pub fn to_node(n: &Option<RdfJsonNodeResult>) -> Result<Option<Node<'_>>, JsValue> {
    n.as_ref().map(Node::try_from).transpose().map_err(js_err)
}
