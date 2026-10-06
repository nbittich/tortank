#![cfg(target_arch = "wasm32")]

use lol_alloc::{AssumeSingleThreaded, FreeListAllocator};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use tortank::turtle::turtle_doc::{
    Node, RdfJsonNode, RdfJsonNodeResult, RdfJsonTriple, Statement, TurtleDoc as NativeTurtleDoc,
};
use wasm_bindgen::prelude::*;

// SAFETY: wasm32 without threads is single threaded.
#[global_allocator]
static ALLOCATOR: AssumeSingleThreaded<FreeListAllocator> =
    unsafe { AssumeSingleThreaded::new(FreeListAllocator::new()) };

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

fn take_uuid_err() -> Result<(), JsValue> {
    match JS_UUID_ERR.with(|slot| slot.borrow_mut().take()) {
        Some(e) => Err(e),
        None => Ok(()),
    }
}
/// Registers the JS function for the lifetime of the guard (cleared on drop).
struct UuidFnGuard;

impl Drop for UuidFnGuard {
    fn drop(&mut self) {
        JS_UUID_FN.with(|f| *f.borrow_mut() = None);
        JS_UUID_ERR.with(|e| *e.borrow_mut() = None);
    }
}

fn install_uuid_fn(f: Option<js_sys::Function>) -> (Option<fn() -> String>, Option<UuidFnGuard>) {
    match f {
        Some(f) => {
            JS_UUID_FN.with(|slot| *slot.borrow_mut() = Some(f));
            (Some(js_uuid_gen as fn() -> String), Some(UuidFnGuard))
        }
        None => (None, None),
    }
}

fn js_err<E: std::fmt::Display>(err: E) -> JsValue {
    js_sys::Error::new(&err.to_string()).into()
}

fn to_js<T: Serialize>(value: &T) -> Result<JsValue, JsValue> {
    serde_wasm_bindgen::to_value(value).map_err(js_err)
}

fn from_js<T>(value: JsValue) -> Result<T, JsValue>
where
    T: for<'de> Deserialize<'de>,
{
    serde_wasm_bindgen::from_value(value).map_err(js_err)
}

fn opt_from_js(v: Option<JsValue>) -> Result<Option<RdfJsonNodeResult>, JsValue> {
    match v {
        Some(v) if !v.is_null() && !v.is_undefined() => Ok(Some(from_js(v)?)),
        _ => Ok(None),
    }
}

fn to_node(n: &Option<RdfJsonNodeResult>) -> Result<Option<Node<'_>>, JsValue> {
    n.as_ref().map(Node::try_from).transpose().map_err(js_err)
}

fn stmts_to_js(stmts: Vec<&Statement<'_>>) -> Result<JsValue, JsValue> {
    let triples: Vec<RdfJsonTriple> = stmts.into_iter().map(RdfJsonTriple::from).collect();
    to_js(&triples)
}

/// Owns a fully-owned TurtleDoc; nothing is re-built per call.
#[wasm_bindgen(js_name = TurtleDoc)]
pub struct WasmTurtleDoc {
    doc: NativeTurtleDoc<'static>,
}

#[wasm_bindgen(js_class = TurtleDoc)]
impl WasmTurtleDoc {
    #[wasm_bindgen(js_name = parse)]
    pub fn parse(
        input: &str,
        well_known_prefix: Option<String>,
        uuid_fn: Option<js_sys::Function>,
    ) -> Result<WasmTurtleDoc, JsValue> {
        let (uuid_fn, _guard) = install_uuid_fn(uuid_fn);
        let res = NativeTurtleDoc::try_from((input, well_known_prefix, uuid_fn));
        take_uuid_err()?;
        let doc = res.map_err(js_err)?;
        Ok(Self {
            doc: doc.into_owned(),
        })
    }

    #[wasm_bindgen(js_name = toTurtle)]
    pub fn to_turtle(&self) -> Result<String, JsValue> {
        self.doc.as_turtle().map_err(js_err)
    }

    #[wasm_bindgen(js_name = fromJSON)]
    pub fn from_json(value: JsValue) -> Result<WasmTurtleDoc, JsValue> {
        let triples: Vec<RdfJsonTriple> = from_js(value)?;
        let doc = NativeTurtleDoc::try_from(&triples)
            .map_err(js_err)?
            .into_owned();
        Ok(Self { doc })
    }

    #[wasm_bindgen(js_name = fromJSONString)]
    pub fn from_json_string(value: &str) -> Result<WasmTurtleDoc, JsValue> {
        let triples = RdfJsonTriple::from_json(value).map_err(js_err)?;
        let doc = NativeTurtleDoc::try_from(&triples)
            .map_err(js_err)?
            .into_owned();
        Ok(Self { doc })
    }

    #[wasm_bindgen(getter)]
    pub fn length(&self) -> usize {
        self.doc.len()
    }

    #[wasm_bindgen(js_name = len)]
    pub fn len(&self) -> usize {
        self.doc.len()
    }

    #[wasm_bindgen(js_name = isEmpty)]
    pub fn is_empty(&self) -> bool {
        self.doc.is_empty()
    }

    #[wasm_bindgen(js_name = statements)]
    pub fn statements(&self) -> Result<JsValue, JsValue> {
        to_js(&Vec::<RdfJsonTriple>::from(&self.doc))
    }

    #[wasm_bindgen(js_name = toJSON)]
    pub fn to_json(&self) -> Result<JsValue, JsValue> {
        self.statements()
    }

    #[wasm_bindgen(js_name = toJSONString)]
    pub fn to_json_string(&self) -> Result<String, JsValue> {
        serde_json::to_string(&Vec::<RdfJsonTriple>::from(&self.doc)).map_err(js_err)
    }

    #[wasm_bindgen(js_name = toString)]
    pub fn to_string(&self) -> Result<String, JsValue> {
        self.to_turtle()
    }

    /// Turtle-syntax query. Prefixes/base declared in the parsed document
    /// are available, since the real document is kept.
    #[wasm_bindgen(js_name = match)]
    pub fn match_statements(
        &self,
        subject: Option<String>,
        predicate: Option<String>,
        object: Option<String>,
    ) -> Result<JsValue, JsValue> {
        let stmts = self
            .doc
            .parse_and_list_statements(subject, predicate, object)
            .map_err(js_err)?;
        stmts_to_js(stmts)
    }

    /// Exact RDF-JSON node matching (null/undefined = wildcard).
    #[wasm_bindgen(js_name = matchNodes)]
    pub fn match_nodes(
        &self,
        subject: Option<JsValue>,
        predicate: Option<JsValue>,
        object: Option<JsValue>,
    ) -> Result<JsValue, JsValue> {
        let (s, p, o) = (
            opt_from_js(subject)?,
            opt_from_js(predicate)?,
            opt_from_js(object)?,
        );
        let (sn, pn, on) = (to_node(&s)?, to_node(&p)?, to_node(&o)?);
        stmts_to_js(
            self.doc
                .list_statements(sn.as_ref(), pn.as_ref(), on.as_ref()),
        )
    }

    #[wasm_bindgen(js_name = add)]
    pub fn add(
        &mut self,
        subject: JsValue,
        predicate: JsValue,
        object: JsValue,
    ) -> Result<bool, JsValue> {
        self.add_triple_inner(RdfJsonTriple {
            subject: from_js(subject)?,
            predicate: from_js(predicate)?,
            object: from_js(object)?,
        })
    }

    #[wasm_bindgen(js_name = addTriple)]
    pub fn add_triple(&mut self, triple: JsValue) -> Result<bool, JsValue> {
        self.add_triple_inner(from_js(triple)?)
    }

    #[wasm_bindgen(js_name = remove)]
    pub fn remove(
        &mut self,
        subject: Option<JsValue>,
        predicate: Option<JsValue>,
        object: Option<JsValue>,
    ) -> Result<usize, JsValue> {
        let (s, p, o) = (
            opt_from_js(subject)?,
            opt_from_js(predicate)?,
            opt_from_js(object)?,
        );
        let (sn, pn, on) = (to_node(&s)?, to_node(&p)?, to_node(&o)?);
        Ok(self
            .doc
            .remove_statements(sn.as_ref(), pn.as_ref(), on.as_ref()))
    }

    #[wasm_bindgen(js_name = clear)]
    pub fn clear(&mut self) {
        self.doc.clear();
    }

    #[wasm_bindgen(js_name = contains)]
    pub fn contains(&self, triple: JsValue) -> Result<bool, JsValue> {
        let triple: RdfJsonTriple = from_js(triple)?;
        let stmt = Statement::try_from(&triple).map_err(js_err)?;
        Ok(!self
            .doc
            .list_statements(
                Some(&stmt.subject),
                Some(&stmt.predicate),
                Some(&stmt.object),
            )
            .is_empty())
    }

    #[wasm_bindgen(js_name = difference)]
    pub fn difference(&self, other: &WasmTurtleDoc) -> Result<WasmTurtleDoc, JsValue> {
        let doc = self
            .doc
            .difference(&other.doc)
            .map_err(js_err)?
            .into_owned();
        Ok(Self { doc })
    }

    #[wasm_bindgen(js_name = intersection)]
    pub fn intersection(&self, other: &WasmTurtleDoc) -> Result<WasmTurtleDoc, JsValue> {
        let doc = self
            .doc
            .intersection(&other.doc)
            .map_err(js_err)?
            .into_owned();
        Ok(Self { doc })
    }

    #[wasm_bindgen(js_name = allSubjects)]
    pub fn all_subjects(&self) -> Result<JsValue, JsValue> {
        let mut subjects: Vec<RdfJsonNodeResult> = Vec::new();
        for s in self.doc.all_subjects() {
            let s = RdfJsonNodeResult::from(&s);
            if !subjects.contains(&s) {
                subjects.push(s);
            }
        }
        to_js(&subjects)
    }

    #[wasm_bindgen(js_name = clone)]
    pub fn clone_doc(&self) -> WasmTurtleDoc {
        Self {
            doc: self.doc.clone(),
        }
    }
}

impl WasmTurtleDoc {
    fn add_triple_inner(&mut self, triple: RdfJsonTriple) -> Result<bool, JsValue> {
        let stmt = Statement::try_from(&triple).map_err(js_err)?.into_owned();
        let before = self.doc.len();
        self.doc
            .add_statement(stmt.subject, stmt.predicate, stmt.object);
        Ok(self.doc.len() != before)
    }
}

#[wasm_bindgen(js_name = parseNTriplesStatement)]
pub fn parse_ntriples_statement(
    input: &str,
    uuid_fn: Option<js_sys::Function>,
) -> Result<JsValue, JsValue> {
    let (uuid_fn, _guard) = install_uuid_fn(uuid_fn);
    let parsed = NativeTurtleDoc::parse_ntriples_statement(input, uuid_fn);
    take_uuid_err()?;
    let Some((rest, statement)) = parsed.map_err(js_err)? else {
        return Ok(JsValue::NULL);
    };

    #[derive(Serialize)]
    struct ResultValue<'a> {
        rest: &'a str,
        statement: RdfJsonTriple,
    }
    to_js(&ResultValue {
        rest,
        statement: RdfJsonTriple::from(&statement),
    })
}

fn node(
    typ: &str,
    value: &str,
    datatype: Option<String>,
    lang: Option<String>,
) -> Result<JsValue, JsValue> {
    to_js(&RdfJsonNodeResult::SingleNode(RdfJsonNode {
        typ: typ.into(),
        datatype,
        lang,
        value: value.into(),
    }))
}

#[wasm_bindgen(js_name = uri)]
pub fn uri(value: &str) -> Result<JsValue, JsValue> {
    node("uri", value, None, None)
}

#[wasm_bindgen(js_name = blankNode)]
pub fn blank_node(value: &str) -> Result<JsValue, JsValue> {
    node("bnode", value, None, None)
}

#[wasm_bindgen(js_name = literal)]
pub fn literal(
    value: &str,
    datatype: Option<String>,
    lang: Option<String>,
) -> Result<JsValue, JsValue> {
    node("literal", value, datatype, lang)
}
