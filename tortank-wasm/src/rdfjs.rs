use crate::common::{from_js, install_uuid_fn, js_err, take_uuid_err, to_js, to_node};
use serde::Deserialize;
use tortank::turtle::turtle_doc::{
    Node, RdfJsonNode, RdfJsonNodeResult, RdfJsonTriple, Statement, TurtleDoc as NativeTurtleDoc,
};
use wasm_bindgen::prelude::*;

const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";

// ---------------------------------------------------------------------------
// JS glue: RDF/JS term/quad objects (with `equals`), input readers, and the
// extra Dataset methods that need `this` / callbacks (done in JS on purpose).
// ---------------------------------------------------------------------------
#[wasm_bindgen(inline_js = r#"
const XSD_STRING = 'http://www.w3.org/2001/XMLSchema#string';
const RDF_LANG_STRING = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString';

function termEquals(o) {
  if (!o || o.termType !== this.termType || o.value !== this.value) return false;
  if (this.termType === 'Literal') {
    return o.language === this.language && !!o.datatype && o.datatype.value === this.datatype.value;
  }
  return true;
}
const NamedNodeProto = { equals: termEquals };
const BlankNodeProto = { equals: termEquals };
const LiteralProto = { equals: termEquals };
const DefaultGraphProto = { equals: termEquals };
const QuadProto = {
  equals(o) {
    return !!o
      && this.subject.equals(o.subject) && this.predicate.equals(o.predicate)
      && this.object.equals(o.object) && this.graph.equals(o.graph);
  },
};
const make = (proto, props) => Object.assign(Object.create(proto), props);
const DEFAULT_GRAPH = make(DefaultGraphProto, { termType: 'DefaultGraph', value: '' });
let bnodeCounter = 0;

export function js_named_node(value) {
  return make(NamedNodeProto, { termType: 'NamedNode', value });
}
export function js_blank_node(value) {
  return make(BlankNodeProto, { termType: 'BlankNode', value: value ?? `b${bnodeCounter++}` });
}
export function js_literal(value, language, datatype) {
  const lang = language || '';
  return make(LiteralProto, {
    termType: 'Literal',
    value,
    language: lang,
    datatype: js_named_node(datatype || (lang ? RDF_LANG_STRING : XSD_STRING)),
  });
}
export function js_default_graph() { return DEFAULT_GRAPH; }
export function js_quad(subject, predicate, object, graph) {
  return make(QuadProto, {
    termType: 'Quad', value: '',
    subject, predicate, object, graph: graph ?? DEFAULT_GRAPH,
  });
}

// Normalises any RDF/JS-shaped input (class instances, getters, plain objects).
export function js_read_term(t) {
  if (!t || typeof t.termType !== 'string') throw new TypeError('expected an RDF/JS term');
  return {
    termType: t.termType,
    value: String(t.value ?? ''),
    language: t.language || null,
    datatype: t.datatype ? t.datatype.value : null,
  };
}
export function js_read_quad(q) {
  if (!q || !q.subject || !q.predicate || !q.object) throw new TypeError('expected an RDF/JS quad');
  if (q.graph && q.graph.termType !== 'DefaultGraph') {
    throw new TypeError('named graphs are not supported');
  }
  return {
    subject: js_read_term(q.subject),
    predicate: js_read_term(q.predicate),
    object: js_read_term(q.object),
  };
}

export function js_install(proto) {
  if (Object.prototype.hasOwnProperty.call(proto, '__rdfjsInstalled')) return;
  Object.defineProperty(proto, '__rdfjsInstalled', { value: true });
  proto[Symbol.iterator] = function () { return this.toArray()[Symbol.iterator](); };

  // RDF/JS: mutators return the dataset itself for chaining.
  for (const name of ['add', 'delete', 'addAll', 'deleteMatches']) {
    const orig = proto[name];
    proto[name] = function (...args) { orig.apply(this, args); return this; };
  }

  proto.includes = function (quad) { return this.has(quad); };
  proto.contains = function (other) { return other.toArray().every((q) => this.has(q)); };
  proto.equals = function (other) {
    return !!other && this.size === other.size && this.contains(other);
  };
  proto.union = function (other) {
    return new this.constructor([...this.toArray(), ...other.toArray()]);
  };
  proto.forEach = function (cb) { for (const q of this.toArray()) cb(q, this); };
  proto.some = function (cb) { return this.toArray().some((q) => cb(q, this)); };
  proto.every = function (cb) { return this.toArray().every((q) => cb(q, this)); };
  proto.filter = function (cb) {
    return new this.constructor(this.toArray().filter((q) => cb(q, this)));
  };
  proto.map = function (cb) {
    return new this.constructor(this.toArray().map((q) => cb(q, this)));
  };
  proto.reduce = function (cb, init) {
    const arr = this.toArray();
    return arguments.length > 1
      ? arr.reduce((acc, q) => cb(acc, q, this), init)
      : arr.reduce((acc, q) => cb(acc, q, this));
  };
}
"#)]
extern "C" {
    fn js_named_node(value: &str) -> JsValue;
    fn js_blank_node(value: Option<String>) -> JsValue;
    fn js_literal(value: &str, language: Option<String>, datatype: Option<String>) -> JsValue;
    fn js_default_graph() -> JsValue;
    fn js_quad(s: &JsValue, p: &JsValue, o: &JsValue, g: &JsValue) -> JsValue;
    #[wasm_bindgen(catch)]
    fn js_read_term(t: &JsValue) -> Result<JsValue, JsValue>;
    #[wasm_bindgen(catch)]
    fn js_read_quad(q: &JsValue) -> Result<JsValue, JsValue>;
    fn js_install(proto: &JsValue);
}

/// Installs the JS-side methods (iterator, chaining, filter, ...).
/// Idempotent; runs automatically on module load, exposed for tests.
pub fn install_js_methods() {
    if let Ok(ds) = WasmDataset::new(None) {
        let proto = js_sys::Object::get_prototype_of(&JsValue::from(ds));
        js_install(&proto);
    }
}

#[wasm_bindgen(start)]
fn start() {
    if let Ok(ds) = WasmDataset::new(None) {
        let proto = js_sys::Object::get_prototype_of(&JsValue::from(ds));
        js_install(&proto);
    }
}

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------
fn is_nullish(v: &JsValue) -> bool {
    v.is_null() || v.is_undefined()
}

#[derive(Deserialize)]
struct JsTerm {
    #[serde(rename = "termType")]
    term_type: String,
    value: String,
    language: Option<String>,
    datatype: Option<String>,
}

#[derive(Deserialize)]
struct JsQuad {
    subject: JsTerm,
    predicate: JsTerm,
    object: JsTerm,
}

fn json_node(
    typ: &str,
    value: String,
    datatype: Option<String>,
    lang: Option<String>,
) -> RdfJsonNodeResult {
    RdfJsonNodeResult::SingleNode(RdfJsonNode {
        typ: typ.into(),
        datatype,
        lang,
        value: value.into(),
    })
}

fn rdfjs_to_json(t: JsTerm) -> Result<RdfJsonNodeResult, JsValue> {
    match t.term_type.as_str() {
        "NamedNode" => Ok(json_node("uri", t.value, None, None)),
        "BlankNode" => {
            let value = if t.value.starts_with("_:") {
                t.value
            } else {
                format!("_:{}", t.value)
            };
            Ok(json_node("bnode", value, None, None))
        }
        "Literal" => {
            let lang = t.language.filter(|l| !l.is_empty());
            // xsd:string / rdf:langString are implicit in RDF-JSON
            let datatype = if lang.is_some() {
                None
            } else {
                t.datatype.filter(|d| d.as_str() != XSD_STRING)
            };
            Ok(json_node("literal", t.value, datatype, lang))
        }
        other => Err(js_err(format!("unsupported term type: {other}"))),
    }
}

#[allow(irrefutable_let_patterns)]
fn json_to_term(node: &RdfJsonNodeResult) -> Result<JsValue, JsValue> {
    let RdfJsonNodeResult::SingleNode(n) = node else {
        return Err(js_err("unsupported RDF-JSON node"));
    };
    match &*n.typ {
        "uri" => Ok(js_named_node(&n.value)),
        "bnode" => {
            let v: &str = &n.value;
            Ok(js_blank_node(Some(
                v.strip_prefix("_:").unwrap_or(v).to_string(),
            )))
        }
        "literal" => Ok(js_literal(&n.value, n.lang.clone(), n.datatype.clone())),
        other => Err(js_err(format!("unsupported RDF-JSON node type: {other}"))),
    }
}

fn triple_to_quad(t: &RdfJsonTriple) -> Result<JsValue, JsValue> {
    Ok(js_quad(
        &json_to_term(&t.subject)?,
        &json_to_term(&t.predicate)?,
        &json_to_term(&t.object)?,
        &js_default_graph(),
    ))
}

fn triples_to_quads(triples: &[RdfJsonTriple]) -> Result<JsValue, JsValue> {
    let arr = js_sys::Array::new();
    for t in triples {
        arr.push(&triple_to_quad(t)?);
    }
    Ok(arr.into())
}

fn quad_to_triple(q: &JsValue) -> Result<RdfJsonTriple, JsValue> {
    let q: JsQuad = from_js(js_read_quad(q)?)?;
    Ok(RdfJsonTriple {
        subject: rdfjs_to_json(q.subject)?,
        predicate: rdfjs_to_json(q.predicate)?,
        object: rdfjs_to_json(q.object)?,
    })
}

/// `null` / `undefined` / Variable => wildcard.
fn opt_term(v: Option<JsValue>) -> Result<Option<RdfJsonNodeResult>, JsValue> {
    match v {
        Some(v) if !is_nullish(&v) => {
            let t: JsTerm = from_js(js_read_term(&v)?)?;
            if t.term_type == "Variable" {
                Ok(None)
            } else {
                rdfjs_to_json(t).map(Some)
            }
        }
        _ => Ok(None),
    }
}

/// Only the default graph exists in this store.
fn graph_is_default(g: Option<JsValue>) -> Result<bool, JsValue> {
    match g {
        Some(g) if !is_nullish(&g) => {
            let t: JsTerm = from_js(js_read_term(&g)?)?;
            Ok(matches!(t.term_type.as_str(), "DefaultGraph" | "Variable"))
        }
        _ => Ok(true),
    }
}

fn doc_from_triples(triples: &Vec<RdfJsonTriple>) -> Result<NativeTurtleDoc<'static>, JsValue> {
    Ok(NativeTurtleDoc::try_from(triples)
        .map_err(js_err)?
        .into_owned())
}

// ---------------------------------------------------------------------------
// DataFactory (RDF/JS)
// ---------------------------------------------------------------------------
#[wasm_bindgen(js_name = DataFactory)]
pub struct WasmDataFactory;

#[wasm_bindgen(js_class = DataFactory)]
impl WasmDataFactory {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmDataFactory {
        WasmDataFactory
    }

    #[wasm_bindgen(js_name = namedNode)]
    pub fn named_node(&self, value: &str) -> JsValue {
        js_named_node(value)
    }

    #[wasm_bindgen(js_name = blankNode)]
    pub fn blank_node(&self, value: Option<String>) -> JsValue {
        js_blank_node(value)
    }

    /// `languageOrDatatype`: string => language tag, NamedNode => datatype.
    #[wasm_bindgen(js_name = literal)]
    pub fn literal(&self, value: &str, language_or_datatype: Option<JsValue>) -> JsValue {
        let (lang, dt) = match language_or_datatype {
            Some(v) if !is_nullish(&v) => match v.as_string() {
                Some(lang) => (Some(lang), None),
                None => (
                    None,
                    js_sys::Reflect::get(&v, &JsValue::from_str("value"))
                        .ok()
                        .and_then(|x| x.as_string()),
                ),
            },
            _ => (None, None),
        };
        js_literal(value, lang, dt)
    }

    #[wasm_bindgen(js_name = defaultGraph)]
    pub fn default_graph(&self) -> JsValue {
        js_default_graph()
    }

    #[wasm_bindgen(js_name = quad)]
    pub fn quad(
        &self,
        subject: JsValue,
        predicate: JsValue,
        object: JsValue,
        graph: Option<JsValue>,
    ) -> JsValue {
        let g = graph
            .filter(|g| !is_nullish(g))
            .unwrap_or_else(js_default_graph);
        js_quad(&subject, &predicate, &object, &g)
    }

    #[wasm_bindgen(js_name = triple)]
    pub fn triple(&self, subject: JsValue, predicate: JsValue, object: JsValue) -> JsValue {
        js_quad(&subject, &predicate, &object, &js_default_graph())
    }
}

impl Default for WasmDataFactory {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Dataset (RDF/JS DatasetCore + Dataset)
// ---------------------------------------------------------------------------
/// Owns a fully-owned TurtleDoc; nothing is re-built per call.
#[wasm_bindgen(js_name = Dataset)]
pub struct WasmDataset {
    doc: NativeTurtleDoc<'static>,
}

#[wasm_bindgen(js_class = Dataset)]
impl WasmDataset {
    /// `new Dataset(quads?)` — `quads` is any iterable of RDF/JS quads.
    #[wasm_bindgen(constructor)]
    pub fn new(quads: Option<JsValue>) -> Result<WasmDataset, JsValue> {
        let mut ds = Self {
            doc: NativeTurtleDoc::empty_doc().map_err(js_err)?.into_owned(),
        };
        if let Some(q) = quads.filter(|q| !is_nullish(q)) {
            ds.add_all(q)?;
        }
        Ok(ds)
    }

    // ---- DatasetCore ------------------------------------------------------
    #[wasm_bindgen(getter)]
    pub fn size(&self) -> usize {
        self.doc.len()
    }

    /// (wrapped in JS to return `this`)
    #[wasm_bindgen(js_name = add)]
    pub fn add(&mut self, quad: JsValue) -> Result<(), JsValue> {
        self.add_triple_inner(quad_to_triple(&quad)?)?;
        Ok(())
    }

    /// (wrapped in JS to return `this`)
    #[wasm_bindgen(js_name = delete)]
    pub fn delete(&mut self, quad: JsValue) -> Result<(), JsValue> {
        let t = quad_to_triple(&quad)?;
        let s = Node::try_from(&t.subject).map_err(js_err)?;
        let p = Node::try_from(&t.predicate).map_err(js_err)?;
        let o = Node::try_from(&t.object).map_err(js_err)?;
        self.doc.remove_statements(Some(&s), Some(&p), Some(&o));
        Ok(())
    }

    #[wasm_bindgen(js_name = has)]
    pub fn has(&self, quad: JsValue) -> Result<bool, JsValue> {
        let triple = quad_to_triple(&quad)?;
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

    /// `null` / `undefined` / Variable = wildcard. Returns a new Dataset.
    #[wasm_bindgen(js_name = match)]
    pub fn match_quads(
        &self,
        subject: Option<JsValue>,
        predicate: Option<JsValue>,
        object: Option<JsValue>,
        graph: Option<JsValue>,
    ) -> Result<WasmDataset, JsValue> {
        let triples = if graph_is_default(graph)? {
            self.collect_matches(subject, predicate, object)?
        } else {
            Vec::new()
        };
        Ok(Self {
            doc: doc_from_triples(&triples)?,
        })
    }

    // [Symbol.iterator] is installed from JS (see `js_install`).
    #[wasm_bindgen(js_name = toArray)]
    pub fn to_array(&self) -> Result<JsValue, JsValue> {
        triples_to_quads(&Vec::<RdfJsonTriple>::from(&self.doc))
    }

    // ---- Dataset ----------------------------------------------------------
    /// (wrapped in JS to return `this`)
    #[wasm_bindgen(js_name = addAll)]
    pub fn add_all(&mut self, quads: JsValue) -> Result<(), JsValue> {
        let iter = js_sys::try_iter(&quads)?
            .ok_or_else(|| js_err("addAll expects an iterable of quads"))?;
        for q in iter {
            self.add_triple_inner(quad_to_triple(&q?)?)?;
        }
        Ok(())
    }

    /// (wrapped in JS to return `this`)
    #[wasm_bindgen(js_name = deleteMatches)]
    pub fn delete_matches(
        &mut self,
        subject: Option<JsValue>,
        predicate: Option<JsValue>,
        object: Option<JsValue>,
        graph: Option<JsValue>,
    ) -> Result<(), JsValue> {
        if !graph_is_default(graph)? {
            return Ok(());
        }
        let (s, p, o) = (opt_term(subject)?, opt_term(predicate)?, opt_term(object)?);
        let (sn, pn, on) = (to_node(&s)?, to_node(&p)?, to_node(&o)?);
        self.doc
            .remove_statements(sn.as_ref(), pn.as_ref(), on.as_ref());
        Ok(())
    }

    #[wasm_bindgen(js_name = difference)]
    pub fn difference(&self, other: &WasmDataset) -> Result<WasmDataset, JsValue> {
        let doc = self
            .doc
            .difference(&other.doc)
            .map_err(js_err)?
            .into_owned();
        Ok(Self { doc })
    }

    #[wasm_bindgen(js_name = intersection)]
    pub fn intersection(&self, other: &WasmDataset) -> Result<WasmDataset, JsValue> {
        let doc = self
            .doc
            .intersection(&other.doc)
            .map_err(js_err)?
            .into_owned();
        Ok(Self { doc })
    }

    // Provided from JS: includes, contains, equals, union, forEach, some,
    // every, filter, map, reduce, [Symbol.iterator].

    // ---- extras (not in RDF/JS) ------------------------------------------
    #[wasm_bindgen(js_name = clear)]
    pub fn clear(&mut self) {
        self.doc.clear();
    }

    #[wasm_bindgen(js_name = clone)]
    pub fn clone_doc(&self) -> WasmDataset {
        Self {
            doc: self.doc.clone(),
        }
    }

    #[wasm_bindgen(js_name = parse)]
    pub fn parse(
        input: &str,
        well_known_prefix: Option<String>,
        uuid_fn: Option<js_sys::Function>,
    ) -> Result<WasmDataset, JsValue> {
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

    #[wasm_bindgen(js_name = toString)]
    pub fn to_string(&self) -> Result<String, JsValue> {
        self.to_turtle()
    }

    /// Turtle-syntax pattern query (strings, e.g. `"ex:foo"`, `"a"`, `"\"lit\""`).
    /// Prefixes/base from the parsed document are available.
    #[wasm_bindgen(js_name = matchTurtle)]
    pub fn match_turtle(
        &self,
        subject: Option<String>,
        predicate: Option<String>,
        object: Option<String>,
    ) -> Result<WasmDataset, JsValue> {
        let stmts = self
            .doc
            .parse_and_list_statements(subject, predicate, object)
            .map_err(js_err)?;
        let triples: Vec<RdfJsonTriple> = stmts.into_iter().map(RdfJsonTriple::from).collect();
        Ok(Self {
            doc: doc_from_triples(&triples)?,
        })
    }

    // ---- RDF-JSON interop -------------------------------------------------
    #[wasm_bindgen(js_name = fromRdfJson)]
    pub fn from_rdf_json(value: JsValue) -> Result<WasmDataset, JsValue> {
        let triples: Vec<RdfJsonTriple> = from_js(value)?;
        Ok(Self {
            doc: doc_from_triples(&triples)?,
        })
    }

    #[wasm_bindgen(js_name = fromRdfJsonString)]
    pub fn from_rdf_json_string(value: &str) -> Result<WasmDataset, JsValue> {
        let triples = RdfJsonTriple::from_json(value).map_err(js_err)?;
        Ok(Self {
            doc: doc_from_triples(&triples)?,
        })
    }

    #[wasm_bindgen(js_name = toRdfJson)]
    pub fn to_rdf_json(&self) -> Result<JsValue, JsValue> {
        to_js(&Vec::<RdfJsonTriple>::from(&self.doc))
    }

    #[wasm_bindgen(js_name = toRdfJsonString)]
    pub fn to_rdf_json_string(&self) -> Result<String, JsValue> {
        serde_json::to_string(&Vec::<RdfJsonTriple>::from(&self.doc)).map_err(js_err)
    }
}

impl WasmDataset {
    fn add_triple_inner(&mut self, triple: RdfJsonTriple) -> Result<bool, JsValue> {
        let stmt = Statement::try_from(&triple).map_err(js_err)?.into_owned();
        let before = self.doc.len();
        self.doc
            .add_statement(stmt.subject, stmt.predicate, stmt.object);
        Ok(self.doc.len() != before)
    }

    fn collect_matches(
        &self,
        subject: Option<JsValue>,
        predicate: Option<JsValue>,
        object: Option<JsValue>,
    ) -> Result<Vec<RdfJsonTriple>, JsValue> {
        let (s, p, o) = (opt_term(subject)?, opt_term(predicate)?, opt_term(object)?);
        let (sn, pn, on) = (to_node(&s)?, to_node(&p)?, to_node(&o)?);
        let out: Vec<RdfJsonTriple> = self
            .doc
            .list_statements(sn.as_ref(), pn.as_ref(), on.as_ref())
            .into_iter()
            .map(RdfJsonTriple::from)
            .collect();
        Ok(out)
    }
}

// ---------------------------------------------------------------------------
// N-Triples single-statement parsing -> `{ rest, quad }`
// ---------------------------------------------------------------------------
#[wasm_bindgen(js_name = parseNTriplesQuad)]
pub fn parse_ntriples_quad(
    input: &str,
    uuid_fn: Option<js_sys::Function>,
) -> Result<JsValue, JsValue> {
    let (uuid_fn, _guard) = install_uuid_fn(uuid_fn);
    let parsed = NativeTurtleDoc::parse_ntriples_statement(input, uuid_fn);
    take_uuid_err()?;
    let Some((rest, statement)) = parsed.map_err(js_err)? else {
        return Ok(JsValue::NULL);
    };

    let out = js_sys::Object::new();
    js_sys::Reflect::set(&out, &"rest".into(), &JsValue::from_str(rest))?;
    js_sys::Reflect::set(
        &out,
        &"quad".into(),
        &triple_to_quad(&RdfJsonTriple::from(&statement))?,
    )?;
    Ok(out.into())
}
