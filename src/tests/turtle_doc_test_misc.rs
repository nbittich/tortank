use crate::{
    shared::XSD_STRING,
    turtle::turtle_doc::{
        Literal, Node, RdfJsonNode, RdfJsonNodeResult, RdfJsonTriple, Statement, TurtleDoc,
    },
};
use serial_test::serial;
use std::borrow::Cow;
use Cow::Borrowed;
use Node::Iri;

#[test]
#[serial]
fn turtle_doc_add_test() {
    let doc1 = r#"
        @prefix : <http://example.com/>.
        :a :b ( "apple" "banana" ) .
        "#;
    let doc2 = r#"
        @prefix foaf: <http://foaf.com/>.
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

        "#;
    let turtle1: TurtleDoc = (doc1, None).try_into().unwrap();
    assert_eq!(5, turtle1.list_statements(None, None, None).len());

    let turtle2: TurtleDoc = (doc2, None).try_into().unwrap();
    assert_eq!(8, turtle2.list_statements(None, None, None).len());

    let turtle3 = turtle1 + turtle2;
    assert_eq!(13, turtle3.list_statements(None, None, None).len());
    let mut turtle = TurtleDoc::default();
    turtle.add_statement(
        Iri(Borrowed("http://xxx.com/123")),
        Iri(Borrowed("http://bar.com/345")),
        Node::Literal(Literal::Decimal(123f32)),
    );
    let turtle4 = turtle + turtle3;
    assert_eq!(14, turtle4.list_statements(None, None, None).len());
}

#[test]
#[serial]
fn turtle_doc_list_statements_test() {
    let doc = r#"
        @prefix foaf: <http://foaf.com/>.
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

        "#;
    let turtle: TurtleDoc = (doc, None).try_into().unwrap();
    let statements = turtle.list_statements(None, None, Some(&Iri(Borrowed("bob@example.com"))));
    assert_eq!(1, statements.len());
    println!("{statements:?}");
    let statement = statements[0];
    let statements = turtle.list_statements(Some(&statement.subject), None, None);
    assert_eq!(5, statements.len());
}

#[test]
#[serial]
fn test_convert_rdf_triple_to_doc() {
    let triple = RdfJsonTriple {
        subject: RdfJsonNodeResult::SingleNode(RdfJsonNode {
            typ: "uri".into(),
            datatype: None,
            lang: None,
            value: "http://xx.com/xxx".into(),
        }),
        predicate: RdfJsonNodeResult::SingleNode(RdfJsonNode {
            typ: "uri".into(),
            datatype: None,
            lang: None,
            value: "http://xx.com/pred".into(),
        }),
        object: RdfJsonNodeResult::SingleNode(RdfJsonNode {
            typ: "literal".into(),
            datatype: Some(XSD_STRING.into()),
            lang: None,
            value: "hello".into(),
        }),
    };
    let stmt: Statement = (&triple).try_into().unwrap();

    let expected = Statement {
        subject: Node::Iri(Cow::Borrowed("http://xx.com/xxx")),
        predicate: Node::Iri(Cow::Borrowed("http://xx.com/pred")),
        object: Node::Literal(Literal::Quoted {
            datatype: Some(Box::new(Node::Iri(Cow::Borrowed(
                "http://www.w3.org/2001/XMLSchema#string",
            )))),
            value: Cow::Borrowed("hello"),
            lang: None,
        }),
    };
    assert_eq!(stmt, expected);
}
