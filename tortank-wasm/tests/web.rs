#![cfg(target_arch = "wasm32")]

use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

use tortank_wasm::{
    literal,
    parse_ntriples_statement,
    uri,
    WasmTurtleDoc,
};

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
fn parses_turtle() {
    let doc = WasmTurtleDoc::parse(TTL, None).unwrap();

    assert!(!doc.is_empty());
    assert_eq!(doc.len(), 5);
    assert_eq!(doc.length(), 5);
}

#[wasm_bindgen_test]
fn empty_document() {
    let doc = WasmTurtleDoc::parse("", None).unwrap();

    assert!(doc.is_empty());
    assert_eq!(doc.len(), 0);
}

#[wasm_bindgen_test]
fn rdf_json_roundtrip() {
    let doc = WasmTurtleDoc::parse(TTL, None).unwrap();

    let json = doc.to_json().unwrap();
    let restored = WasmTurtleDoc::from_json(json).unwrap();

    assert_eq!(doc.len(), restored.len());

    let diff = doc.difference(&restored);
    assert!(diff.is_empty());
}

#[wasm_bindgen_test]
fn json_string_roundtrip() {
    let doc = WasmTurtleDoc::parse(TTL, None).unwrap();

    let json = doc.to_json_string().unwrap();
    let restored =
        WasmTurtleDoc::from_json_string(&json).unwrap();

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
    )
    .unwrap();

    let intersection = a.intersection(&b);

    assert_eq!(intersection.len(), 1);
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
    )
    .unwrap();

    let b = WasmTurtleDoc::parse(
        r#"
        <https://example.com/a>
            <https://example.com/name>
            "Alice" .
        "#,
        None,
    )
    .unwrap();

    let diff = a.difference(&b);

    assert_eq!(diff.len(), 1);
}

#[wasm_bindgen_test]
fn duplicate_add_is_ignored() {
    let mut doc = WasmTurtleDoc::parse("", None).unwrap();

    let s = uri("https://example.com/alice").unwrap();
    let p = uri("https://example.com/name").unwrap();
    let o = literal("Alice", None, None).unwrap();

    assert!(doc
        .add(s.clone(), p.clone(), o.clone())
        .unwrap());

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
    )
    .unwrap();

    let subject =
        uri("https://example.com/a").unwrap();

    let removed = doc
        .remove(
            Some(subject),
            None,
            None,
        )
        .unwrap();

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
    )
    .unwrap();

    let subjects = doc.all_subjects().unwrap();

    let subjects: Vec<serde_json::Value> =
        serde_wasm_bindgen::from_value(subjects).unwrap();

    assert_eq!(subjects.len(), 1);
}

#[wasm_bindgen_test]
fn parse_ntriples() {
    let result = parse_ntriples_statement(
        r#"<https://example.com/a> <https://example.com/name> "Alice" ."#,
    )
    .unwrap();

    assert!(!result.is_null());
}

#[wasm_bindgen_test]
fn invalid_turtle_throws() {
    let result =
        WasmTurtleDoc::parse("this definitely isn't turtle {", None);

    assert!(result.is_err());
}
