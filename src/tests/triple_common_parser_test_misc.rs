use std::borrow::Cow;

use crate::triple_common_parser::{iri, literal::string_literal, Iri, Literal};

#[test]
fn test_parse_iri_escaped() {
    let iri_str = "<http://a.example/%66oo-bar>";
    let (_, res) = iri::iri(iri_str).unwrap();
    assert_eq!(Iri::Enclosed("http://a.example/%66oo-bar"), res);

    let iri_prefix = "ex:%66oo-bar";
    let (_, res) = iri::iri(iri_prefix).unwrap();
    assert_eq!(
        Iri::Prefixed {
            prefix: "ex",
            local_name: "%66oo-bar"
        },
        res
    );
    let iri_prefix = "ex:\\%66oo-bar";
    let (_, res) = iri::iri(iri_prefix).unwrap();
    assert_eq!(
        Iri::Prefixed {
            prefix: "ex",
            local_name: "\\%66oo-bar"
        },
        res
    );
    println!("{res:?}");
}
#[test]
fn test_empty_string() {
    let empty_str = r#""""#;
    let (_, res) = string_literal(empty_str).unwrap();
    assert_eq!(
        Literal::Quoted {
            datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string")),
            value: Cow::Borrowed(""),
            lang: None,
        },
        res
    );
}

#[test]
fn test_string_delimit_bugs() {
    let s = r#"'"s"'"#;
    let (_, res) = string_literal(s).unwrap();
    assert_eq!(
        res,
        Literal::Quoted {
            datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string")),
            value: Cow::Borrowed(r#""s""#),
            lang: None,
        },
    );

    let s = r#""'s'""#;
    let (_, res) = string_literal(s).unwrap();
    assert_eq!(
        res,
        Literal::Quoted {
            datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string")),
            value: Cow::Borrowed(r#"'s'"#),
            lang: None,
        },
    );

    let s = r#""""'s'""""#;
    let (_, res) = string_literal(s).unwrap();
    assert_eq!(
        res,
        Literal::Quoted {
            datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string")),
            value: Cow::Borrowed(r#"'s'"#),
            lang: None,
        },
    );

    let s = r#"'''"s"'''"#;
    let (_, res) = string_literal(s).unwrap();
    assert_eq!(
        res,
        Literal::Quoted {
            datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string")),
            value: Cow::Borrowed(r#""s""#),
            lang: None,
        },
    );
}
