#![cfg(target_arch = "wasm32")]

use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

use tortank_wasm::{WasmTurtleDoc, literal, parse_ntriples_statement, uri};

wasm_bindgen_test_configure!(run_in_browser);

const TTL: &str = r#"
@prefix ex: <https://example.com/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice
    a foaf:Person ;
    foaf:name "Alice" ;
    foaf:age 42 .

ex:bob
    a foaf:Person ;
    foaf:name "Bob"@en .
"#;

#[wasm_bindgen_test]
fn serializes_as_turtle() {
    let doc = WasmTurtleDoc::parse(TTL, None, None).unwrap();

    let turtle = doc.to_turtle().unwrap();

    assert!(!turtle.is_empty());
    assert!(turtle.contains("Alice"));
    assert!(turtle.contains("Bob"));
}

#[wasm_bindgen_test]
fn parses_turtle() {
    let doc = WasmTurtleDoc::parse(TTL, None, None).unwrap();

    assert!(!doc.is_empty());
    assert_eq!(doc.len(), 5);
    assert_eq!(doc.length(), 5);
}

#[wasm_bindgen_test]
fn empty_document() {
    let doc = WasmTurtleDoc::parse("", None, None).unwrap();

    assert!(doc.is_empty());
    assert_eq!(doc.len(), 0);
}

#[wasm_bindgen_test]
fn rdf_json_roundtrip() {
    let doc = WasmTurtleDoc::parse(TTL, None, None).unwrap();

    let json = doc.to_json().unwrap();
    let restored = WasmTurtleDoc::from_json(json).unwrap();

    assert_eq!(doc.len(), restored.len());

    let diff = doc.difference(&restored).unwrap();
    assert!(diff.is_empty());

    let diff = restored.difference(&doc).unwrap();
    assert!(diff.is_empty());
}
#[wasm_bindgen_test]
fn finds_difference() {
    let a = WasmTurtleDoc::parse(
        r#"
        <https://example.com/a>
            <https://example.com/name>
            "Alice" .

        <https://example.com/b>
            <https://example.com/name>
            "Bob" .
        "#,
        None,
        None,
    )
    .unwrap();

    let b = WasmTurtleDoc::parse(
        r#"
        <https://example.com/a>
            <https://example.com/name>
            "Alice" .
        "#,
        None,
        None,
    )
    .unwrap();

    let diff = a.difference(&b).unwrap();

    assert_eq!(diff.len(), 1);
}
#[wasm_bindgen_test]
fn match_uses_document_prefixes() {
    let doc = WasmTurtleDoc::parse(TTL, None, None).unwrap();

    // `ex:` is declared in TTL; this only works because the parsed doc is kept.
    let result = doc
        .match_statements(Some("ex:alice".into()), None, None)
        .unwrap();

    let triples: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
    assert_eq!(triples.len(), 3);
}
#[wasm_bindgen_test]
fn match_nodes_and_contains() {
    let doc = WasmTurtleDoc::parse(TTL, None, None).unwrap();

    let result = doc
        .match_nodes(Some(uri("https://example.com/bob").unwrap()), None, None)
        .unwrap();

    let triples: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();

    assert_eq!(triples.len(), 2);

    let triple = js_sys::Object::new();

    js_sys::Reflect::set(
        &triple,
        &JsValue::from_str("subject"),
        &uri("https://example.com/bob").unwrap(),
    )
    .unwrap();

    js_sys::Reflect::set(
        &triple,
        &JsValue::from_str("predicate"),
        &uri("http://xmlns.com/foaf/0.1/name").unwrap(),
    )
    .unwrap();

    js_sys::Reflect::set(
        &triple,
        &JsValue::from_str("object"),
        &literal("Bob", None, Some("en".into())).unwrap(),
    )
    .unwrap();

    assert!(doc.contains(triple.into()).unwrap());
}
#[wasm_bindgen_test]
fn clear_empties_document() {
    let mut doc = WasmTurtleDoc::parse(TTL, None, None).unwrap();
    doc.clear();
    assert!(doc.is_empty());
}
#[wasm_bindgen_test]
fn json_string_roundtrip() {
    let doc = WasmTurtleDoc::parse(TTL, None, None).unwrap();

    let json = doc.to_json_string().unwrap();
    let restored = WasmTurtleDoc::from_json_string(&json).unwrap();

    assert_eq!(doc.len(), restored.len());
}

#[wasm_bindgen_test]
fn finds_intersection() {
    let a = WasmTurtleDoc::parse(
        r#"
        <https://example.com/a>
            <https://example.com/name>
            "Alice" .
        "#,
        None,
        None,
    )
    .unwrap();

    let b = WasmTurtleDoc::parse(
        r#"
        <https://example.com/a>
            <https://example.com/name>
            "Alice" .

        <https://example.com/b>
            <https://example.com/name>
            "Bob" .
        "#,
        None,
        None,
    )
    .unwrap();

    let intersection = a.intersection(&b).unwrap();

    assert_eq!(intersection.len(), 1);
}
#[wasm_bindgen_test]
fn duplicate_add_is_ignored() {
    let mut doc = WasmTurtleDoc::parse("", None, None).unwrap();

    let s = uri("https://example.com/alice").unwrap();
    let p = uri("https://example.com/name").unwrap();
    let o = literal("Alice", None, None).unwrap();

    assert!(doc.add(s.clone(), p.clone(), o.clone()).unwrap());

    assert!(!doc.add(s, p, o).unwrap());

    assert_eq!(doc.len(), 1);
}

#[wasm_bindgen_test]
fn remove_statement() {
    let mut doc = WasmTurtleDoc::parse(
        r#"
        <https://example.com/a>
            <https://example.com/name>
            "Alice" .
        "#,
        None,
        None,
    )
    .unwrap();

    let subject = uri("https://example.com/a").unwrap();

    let removed = doc.remove(Some(subject), None, None).unwrap();

    assert_eq!(removed, 1);
    assert!(doc.is_empty());
}

#[wasm_bindgen_test]
fn all_subjects_are_unique() {
    let doc = WasmTurtleDoc::parse(
        r#"
        <https://example.com/a>
            <https://example.com/name>
            "Alice" ;
            <https://example.com/age>
            42 .
        "#,
        None,
        None,
    )
    .unwrap();

    let subjects = doc.all_subjects().unwrap();

    let subjects: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(subjects).unwrap();

    assert_eq!(subjects.len(), 1);
}

#[wasm_bindgen_test]
fn parse_ntriples() {
    let result = parse_ntriples_statement(
        r#"<https://example.com/a> <https://example.com/name> "Alice" ."#,
        None,
    )
    .unwrap();

    assert!(!result.is_null());
}

#[wasm_bindgen_test]
fn invalid_turtle_throws() {
    let result = WasmTurtleDoc::parse("this definitely isn't turtle {", None, None);

    assert!(result.is_err());
}

#[wasm_bindgen_test]
fn custom_uuid_fn_names_blank_nodes() {
    let uuid_fn = js_sys::Function::new_no_args("return '1'");

    let doc = WasmTurtleDoc::parse(
        r#"<https://example.com/a> <https://example.com/knows> [ <https://example.com/name> "Bob" ] ."#,
        None,
        Some(uuid_fn),
    )
    .unwrap();

    let triples: Vec<serde_json::Value> =
        serde_wasm_bindgen::from_value(doc.to_json().unwrap()).unwrap();

    let dump = serde_json::to_string_pretty(&triples).unwrap();

    assert!(
        triples.iter().any(|t| t["object"]["type"] == "bnode"),
        "no bnode object found, triples were:\n{dump}"
    );

    assert!(
        triples.iter().any(|t| t["object"]["type"] == "bnode"
            && t["object"]["value"]
                .as_str()
                .is_some_and(|v| v.ends_with('1'))),
        "bnode value doesn't end with 1, triples were:\n{dump}"
    );
    // `_:1 name "Bob"`
    assert!(
        triples
            .iter()
            .any(|t| t["subject"]["type"] == "bnode" && t["subject"]["value"] == "1")
    );

    // and it shows up as `_:1` in the turtle output
    assert!(doc.to_turtle().unwrap().contains("_:1"));
}

#[wasm_bindgen_test]
fn uuid_fn_returning_non_string_throws() {
    let uuid_fn = js_sys::Function::new_no_args("return 1");

    let result = WasmTurtleDoc::parse(
        r#"<https://example.com/a> <https://example.com/knows> [ <https://example.com/name> "Bob" ] ."#,
        None,
        Some(uuid_fn),
    );

    assert!(result.is_err());
}
