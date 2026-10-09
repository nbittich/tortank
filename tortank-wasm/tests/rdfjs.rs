#![cfg(target_arch = "wasm32")]

use js_sys::{Array, Function, Reflect};
use tortank_wasm::rdfjs::{WasmDataFactory, WasmDataset, install_js_methods, parse_ntriples_quad};
use wasm_bindgen::prelude::*;
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

// private in the crate, so redefine it here
const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";

const EX: &str = "http://example.org/";
const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
const RDF_LANG_STRING: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";
fn iri(s: &str) -> String {
    format!("{EX}{s}")
}
fn get(v: &JsValue, k: &str) -> JsValue {
    Reflect::get(v, &JsValue::from_str(k)).unwrap()
}
fn gs(v: &JsValue, k: &str) -> String {
    get(v, k).as_string().unwrap()
}
fn equals(a: &JsValue, b: &JsValue) -> bool {
    let f: Function = get(a, "equals").into();
    f.call1(a, b).unwrap().as_bool().unwrap()
}
fn df() -> WasmDataFactory {
    WasmDataFactory::new()
}
fn nn(s: &str) -> JsValue {
    df().named_node(&iri(s))
}
fn lit(s: &str) -> JsValue {
    df().literal(s, None)
}
fn q(s: &str, p: &str, o: JsValue) -> JsValue {
    df().quad(nn(s), nn(p), o, None)
}
fn len(v: &JsValue) -> u32 {
    Array::from(v).length()
}
fn ds(quads: &[JsValue]) -> WasmDataset {
    let mut d = WasmDataset::new(None).unwrap();
    for quad in quads {
        d.add(quad.clone()).unwrap();
    }
    d
}

// ---- DataFactory ------------------------------------------------------

#[wasm_bindgen_test]
fn named_node_shape() {
    let n = nn("a");
    assert_eq!(gs(&n, "termType"), "NamedNode");
    assert_eq!(gs(&n, "value"), iri("a"));
    assert!(equals(&n, &nn("a")));
    assert!(!equals(&n, &nn("b")));
}

#[wasm_bindgen_test]
fn blank_node_labels() {
    let named = df().blank_node(Some("x".into()));
    assert_eq!(gs(&named, "termType"), "BlankNode");
    assert_eq!(gs(&named, "value"), "x");
    let a = df().blank_node(None);
    let b = df().blank_node(None);
    assert_ne!(gs(&a, "value"), gs(&b, "value"));
}

#[wasm_bindgen_test]
fn literal_defaults_and_variants() {
    let plain = lit("hi");
    assert_eq!(gs(&plain, "language"), "");
    assert_eq!(gs(&get(&plain, "datatype"), "value"), XSD_STRING);

    let tagged = df().literal("hi", Some("en".into()));
    assert_eq!(gs(&tagged, "language"), "en");
    assert_eq!(gs(&get(&tagged, "datatype"), "value"), RDF_LANG_STRING);

    let typed = df().literal("1", Some(df().named_node(XSD_INTEGER)));
    assert_eq!(gs(&get(&typed, "datatype"), "value"), XSD_INTEGER);
    assert!(!equals(&plain, &typed));
    assert!(equals(
        &typed,
        &df().literal("1", Some(df().named_node(XSD_INTEGER)))
    ));
}

#[wasm_bindgen_test]
fn quad_defaults_to_default_graph_and_equals() {
    let a = q("s", "p", lit("o"));
    assert_eq!(gs(&get(&a, "graph"), "termType"), "DefaultGraph");
    assert!(equals(&a, &q("s", "p", lit("o"))));
    assert!(!equals(&a, &q("s", "p", lit("other"))));
    let t = df().triple(nn("s"), nn("p"), lit("o"));
    assert!(equals(&a, &t));
}

// ---- Dataset core -----------------------------------------------------

#[wasm_bindgen_test]
fn add_has_size_and_dedup() {
    let mut d = WasmDataset::new(None).unwrap();
    assert_eq!(d.size(), 0);
    d.add(q("s", "p", lit("o"))).unwrap();
    d.add(q("s", "p", lit("o"))).unwrap();
    assert_eq!(d.size(), 1);
    assert!(d.has(q("s", "p", lit("o"))).unwrap());
    assert!(!d.has(q("s", "p", lit("nope"))).unwrap());
}

#[wasm_bindgen_test]
fn delete_removes_only_that_quad() {
    let mut d = ds(&[q("s", "p", lit("1")), q("s", "p", lit("2"))]);
    d.delete(q("s", "p", lit("1"))).unwrap();
    assert_eq!(d.size(), 1);
    assert!(d.has(q("s", "p", lit("2"))).unwrap());
}

#[wasm_bindgen_test]
fn blank_node_and_lang_roundtrip() {
    let b = df().blank_node(Some("b1".into()));
    let tagged = df().literal("hi", Some("en".into()));
    let d = ds(&[df().quad(b.clone(), nn("p"), tagged.clone(), None)]);
    let arr = d.to_array().unwrap();
    assert_eq!(len(&arr), 1);
    let out = Array::from(&arr).get(0);
    assert!(equals(&get(&out, "subject"), &b));
    assert!(equals(&get(&out, "object"), &tagged));
}

#[wasm_bindgen_test]
fn typed_literal_roundtrip() {
    let typed = df().literal("1", Some(df().named_node(XSD_INTEGER)));
    let d = ds(&[q("s", "p", typed.clone())]);
    assert!(d.has(q("s", "p", typed.clone())).unwrap());
    let out = Array::from(&d.to_array().unwrap()).get(0);
    assert!(equals(&get(&out, "object"), &typed));
}

#[wasm_bindgen_test]
fn match_wildcards() {
    let d = ds(&[
        q("a", "p", lit("1")),
        q("a", "q", lit("2")),
        q("b", "p", lit("3")),
    ]);
    let by_s = d.match_quads(Some(nn("a")), None, None, None).unwrap();
    assert_eq!(by_s.size(), 2);
    let by_p = d.match_quads(None, Some(nn("p")), None, None).unwrap();
    assert_eq!(by_p.size(), 2);
    let by_sp = d
        .match_quads(Some(nn("a")), Some(nn("p")), None, None)
        .unwrap();
    assert_eq!(by_sp.size(), 1);
    let all = d.match_quads(None, None, None, None).unwrap();
    assert_eq!(all.size(), 3);
    // JS null behaves like None
    let nulls = d
        .match_quads(Some(JsValue::NULL), Some(JsValue::UNDEFINED), None, None)
        .unwrap();
    assert_eq!(nulls.size(), 3);
}

#[wasm_bindgen_test]
fn match_named_graph_is_empty() {
    let d = ds(&[q("a", "p", lit("1"))]);
    let g = nn("g");
    let m = d.match_quads(None, None, None, Some(g)).unwrap();
    assert_eq!(m.size(), 0);
}

#[wasm_bindgen_test]
fn delete_matches() {
    let mut d = ds(&[
        q("a", "p", lit("1")),
        q("a", "q", lit("2")),
        q("b", "p", lit("3")),
    ]);
    d.delete_matches(Some(nn("a")), None, None, None).unwrap();
    assert_eq!(d.size(), 1);
    // named graph: no-op
    d.delete_matches(None, None, None, Some(nn("g"))).unwrap();
    assert_eq!(d.size(), 1);
}

#[wasm_bindgen_test]
fn add_all_accepts_any_iterable() {
    let arr = Array::of2(&q("a", "p", lit("1")), &q("b", "p", lit("2")));
    let mut d = WasmDataset::new(None).unwrap();
    d.add_all(arr.into()).unwrap();
    assert_eq!(d.size(), 2);
    assert!(d.add_all(JsValue::from_f64(1.0)).is_err());
}

#[wasm_bindgen_test]
fn constructor_with_quads() {
    let arr = Array::of1(&q("a", "p", lit("1")));
    let d = WasmDataset::new(Some(arr.into())).unwrap();
    assert_eq!(d.size(), 1);
}

#[wasm_bindgen_test]
fn set_operations() {
    let a = ds(&[q("s", "p", lit("1")), q("s", "p", lit("2"))]);
    let b = ds(&[q("s", "p", lit("2")), q("s", "p", lit("3"))]);
    let diff = a.difference(&b).unwrap();
    assert_eq!(diff.size(), 1);
    assert!(diff.has(q("s", "p", lit("1"))).unwrap());
    let inter = a.intersection(&b).unwrap();
    assert_eq!(inter.size(), 1);
    assert!(inter.has(q("s", "p", lit("2"))).unwrap());
}

// ---- input validation -------------------------------------------------

#[wasm_bindgen_test]
fn rejects_non_quads_and_named_graphs() {
    let mut d = WasmDataset::new(None).unwrap();
    assert!(d.add(JsValue::from_str("nope")).is_err());
    assert!(d.add(JsValue::NULL).is_err());
    let in_graph = df().quad(nn("s"), nn("p"), lit("o"), Some(nn("g")));
    assert!(d.add(in_graph).is_err());
    assert_eq!(d.size(), 0);
}

#[wasm_bindgen_test]
fn accepts_plain_object_terms() {
    let obj = js_sys::Object::new();
    let term = |ty: &str, v: &str| {
        let o = js_sys::Object::new();
        Reflect::set(&o, &"termType".into(), &ty.into()).unwrap();
        Reflect::set(&o, &"value".into(), &v.into()).unwrap();
        JsValue::from(o)
    };
    Reflect::set(&obj, &"subject".into(), &term("NamedNode", &iri("s"))).unwrap();
    Reflect::set(&obj, &"predicate".into(), &term("NamedNode", &iri("p"))).unwrap();
    Reflect::set(&obj, &"object".into(), &term("Literal", "x")).unwrap();
    let mut d = WasmDataset::new(None).unwrap();
    d.add(obj.into()).unwrap();
    assert!(d.has(q("s", "p", lit("x"))).unwrap());
}

// ---- Turtle / RDF-JSON extras -----------------------------------------

#[wasm_bindgen_test]
fn parse_and_match_turtle() {
    let ttl = "@prefix ex: <http://example.org/> .\nex:a ex:p \"x\" , \"y\" .\nex:b ex:p \"z\" .\n";
    let d = WasmDataset::parse(ttl, None, None).unwrap();
    assert_eq!(d.size(), 3);
    let m = d.match_turtle(Some("ex:a".into()), None, None).unwrap();
    assert_eq!(m.size(), 2);
}

#[wasm_bindgen_test]
fn to_turtle_roundtrip() {
    let d = ds(&[q("a", "p", lit("x")), q("b", "p", lit("y"))]);
    let back = WasmDataset::parse(&d.to_turtle().unwrap(), None, None).unwrap();
    assert_eq!(back.size(), 2);
    assert!(back.has(q("a", "p", lit("x"))).unwrap());
}

#[wasm_bindgen_test]
fn rdf_json_roundtrip() {
    let d = ds(&[
        q("a", "p", lit("x")),
        q("a", "p", df().literal("hi", Some("en".into()))),
    ]);
    let json = d.to_rdf_json_string().unwrap();
    let back = WasmDataset::from_rdf_json_string(&json).unwrap();
    assert_eq!(back.size(), 2);
    let via_value = WasmDataset::from_rdf_json(d.to_rdf_json().unwrap()).unwrap();
    assert_eq!(via_value.size(), 2);
}

#[wasm_bindgen_test]
fn clear_and_clone_are_independent() {
    let mut d = ds(&[q("a", "p", lit("x"))]);
    let copy = d.clone_doc();
    d.clear();
    assert_eq!(d.size(), 0);
    assert_eq!(copy.size(), 1);
}

// ---- N-Triples --------------------------------------------------------

#[wasm_bindgen_test]
fn parse_ntriples_quad_ok() {
    let nt = "<http://example.org/a> <http://example.org/p> \"x\" .\n";
    let out = parse_ntriples_quad(nt, None).unwrap();
    assert!(get(&out, "rest").is_string());
    let quad = get(&out, "quad");
    assert_eq!(gs(&get(&quad, "subject"), "value"), iri("a"));
    assert_eq!(gs(&get(&quad, "object"), "value"), "x");
    assert_eq!(gs(&get(&quad, "graph"), "termType"), "DefaultGraph");
}

#[wasm_bindgen_test]
fn parse_ntriples_quad_invalid() {
    // either an Err or a null, depending on how tortank reports it
    match parse_ntriples_quad("not ntriples", None) {
        Ok(v) => assert!(v.is_null()),
        Err(_) => {}
    }
}

// ---- JS-installed methods ---------------------------------------------

#[wasm_bindgen_test]
fn mutators_return_this_after_install() {
    install_js_methods(); // idempotent
    let d = JsValue::from(WasmDataset::new(None).unwrap());
    let add: Function = get(&d, "add").into();
    let ret = add.call1(&d, &q("a", "p", lit("x"))).unwrap();
    assert!(ret.is_object());
    assert_eq!(get(&ret, "size").as_f64().unwrap(), 1.0);
}
