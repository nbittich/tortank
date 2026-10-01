use std::{borrow::Cow, sync::Arc};

use crate::turtle::turtle_doc::{Literal, Node, Statement, TurtleDoc};

#[test]
fn test_query_uuid() {
    let mut buf = String::new();
    let doc =
        TurtleDoc::from_file("examples/turtle_doc/query/complemented.ttl", None, &mut buf).unwrap();
    let stmts = doc.list_statements(
        Some(&Node::Iri(Cow::Borrowed(
            "http://data.lblod.info/id/zittingen/65F2D12B8426B6E4C5562BA4",
        ))),
        Some(&Node::Iri(Cow::Borrowed(
            "http://mu.semte.ch/vocabularies/core/uuid",
        ))),
        None,
    );
    assert_eq!(
        stmts,
        vec![&Statement {
            subject: Node::Ref(Arc::new(Node::Iri(Cow::Borrowed(
                "http://data.lblod.info/id/zittingen/65F2D12B8426B6E4C5562BA4"
            )))),
            predicate: Node::Iri(Cow::Borrowed("http://mu.semte.ch/vocabularies/core/uuid",)),
            object: Node::Literal(Literal::Quoted {
                datatype: Some(Box::new(Node::Iri(Cow::Borrowed(
                    "http://www.w3.org/2001/XMLSchema#string",
                )))),
                value: "019695f939b37cb2a025d0286331d05a".into(),
                lang: None,
            }),
        },]
    )
}
