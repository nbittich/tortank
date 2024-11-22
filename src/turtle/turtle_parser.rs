use std::collections::VecDeque;

use crate::prelude::*;

use crate::shared::RDF_NIL;

use crate::triple_common_parser::iri::iri;
use crate::triple_common_parser::literal::literal_turtle as literal;
use crate::triple_common_parser::prologue::{
    base_sparql, base_turtle, prefix_sparql, prefix_turtle,
};
use crate::triple_common_parser::triple::{
    anon_bnode, collection, labeled_bnode, ns_type, object_list, predicate_list,
};
use crate::triple_common_parser::{comments, BlankNode, Iri, Literal};

#[derive(PartialEq, Debug)]
pub enum TurtleValue<'a> {
    Base(Iri<'a>),
    Prefix((&'a str, Iri<'a>)),
    Iri(Iri<'a>),
    Literal(Literal<'a>),
    BNode(BlankNode<'a>),
    ObjectList(Vec<TurtleValue<'a>>),
    Collection(VecDeque<TurtleValue<'a>>),
    PredicateObject {
        predicate: Box<TurtleValue<'a>>,
        object: Box<TurtleValue<'a>>,
    },
    Statement {
        subject: Box<TurtleValue<'a>>,
        predicate_objects: Vec<TurtleValue<'a>>,
    },
}

fn object_lists(s: &str) -> ParserResult<TurtleValue> {
    object_list(object, TurtleValue::ObjectList)(s)
}

fn predicate_lists<'a, F>(subject_extractor: F) -> impl FnMut(&'a str) -> ParserResult<TurtleValue>
where
    F: Fn(&'a str) -> ParserResult<TurtleValue>,
{
    let map_predicate_object = |(predicate, objects)| TurtleValue::PredicateObject {
        predicate: Box::new(predicate),
        object: Box::new(objects),
    };
    predicate_list(
        subject_extractor,
        predicate,
        object_lists,
        map_predicate_object,
        |subject, list| TurtleValue::Statement {
            subject: Box::new(subject),
            predicate_objects: list,
        },
    )
}

fn collection_turtle(s: &str) -> ParserResult<TurtleValue> {
    map(collection(object), |res: VecDeque<TurtleValue>| {
        if res.is_empty() {
            TurtleValue::Iri(Iri::Enclosed(RDF_NIL))
        } else {
            TurtleValue::Collection(res)
        }
    })(s)
}

fn anon_bnode_turtle(s: &str) -> ParserResult<TurtleValue> {
    let unlabeled_subject = |s| Ok((s, TurtleValue::BNode(BlankNode::Unlabeled)));
    anon_bnode(alt((predicate_lists(unlabeled_subject), unlabeled_subject)))(s)
}

fn blank_node(s: &str) -> ParserResult<TurtleValue> {
    alt((map(labeled_bnode, TurtleValue::BNode), anon_bnode_turtle))(s)
}

fn iri_turtle(s: &str) -> ParserResult<TurtleValue> {
    map(iri, TurtleValue::Iri)(s)
}
fn literal_turtle(s: &str) -> ParserResult<TurtleValue> {
    map(literal, TurtleValue::Literal)(s)
}

pub(crate) fn subject(s: &str) -> ParserResult<TurtleValue> {
    alt((blank_node, iri_turtle, collection_turtle))(s)
}
pub(crate) fn predicate(s: &str) -> ParserResult<TurtleValue> {
    alt((map(ns_type, TurtleValue::Iri), iri_turtle))(s)
}

pub(crate) fn object(s: &str) -> ParserResult<TurtleValue> {
    preceded(
        multispace0,
        alt((iri_turtle, blank_node, collection_turtle, literal_turtle)),
    )(s)
}

fn triples(s: &str) -> ParserResult<TurtleValue> {
    terminated(
        alt((predicate_lists(subject), anon_bnode_turtle)),
        preceded(multispace0, terminated(alt((tag("."), eof)), multispace0)),
    )(s)
}

fn directive(s: &str) -> ParserResult<TurtleValue> {
    let base_to_turtle = map(alt((base_sparql, base_turtle)), TurtleValue::Base);
    let prefix_to_turtle = map(alt((prefix_turtle, prefix_sparql)), TurtleValue::Prefix);
    alt((base_to_turtle, prefix_to_turtle))(s)
}

fn statement(s: &str) -> ParserResult<TurtleValue> {
    preceded(comments, alt((directive, triples)))(s)
}

pub fn statements(s: &str) -> IResult<&str, Vec<TurtleValue>> {
    many0(statement)(s)
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
    use std::collections::VecDeque;

    use crate::triple_common_parser::iri::prefixed_iri;
    use crate::triple_common_parser::prologue::{
        base_sparql, base_turtle, prefix_sparql, prefix_turtle,
    };
    use crate::triple_common_parser::{BlankNode, Iri, Literal};
    use crate::turtle::turtle_parser::{labeled_bnode, statements, triples, TurtleValue};

    #[test]
    fn base_test() {
        let sparql = r#"
              BASE   <http://one.example/sparql>

        "#;
        let turtle = r#"

             @base    <http://one.example/turtle> .
        "#;

        let (_, base_turtle) = base_turtle(turtle).unwrap();
        assert_eq!(Iri::Enclosed("http://one.example/turtle"), base_turtle);

        let (_, base_sparql) = base_sparql(sparql).unwrap();
        assert_eq!(Iri::Enclosed("http://one.example/sparql"), base_sparql);
    }

    #[test]
    fn prefix_test() {
        let sparql = r#"
        PREFIX p: <http://two.example/sparql>

        "#;
        let turtle = r#"

             @prefix    p:    <http://two.example/turtle> .
        "#;
        let empty_turtle = r#"

             @prefix    :    <http://two.example/empty> .
        "#;

        let (_, turtle) = prefix_turtle(turtle).unwrap();
        assert_eq!(("p", Iri::Enclosed("http://two.example/turtle")), turtle);
        let (_, prefix_empty_turtle) = prefix_turtle(empty_turtle).unwrap();
        assert_eq!(
            ("", Iri::Enclosed("http://two.example/empty")),
            prefix_empty_turtle
        );

        let (_, sparql) = prefix_sparql(sparql).unwrap();
        assert_eq!(("p", Iri::Enclosed("http://two.example/sparql")), sparql);
    }

    #[test]
    fn prefixed_iri_test() {
        let s = "foaf:firstName";
        let res = prefixed_iri(s);
        assert_eq!(
            Ok((
                "",
                Iri::Prefixed {
                    prefix: "foaf",
                    local_name: "firstName",
                },
            ),),
            res
        );
    }

    #[test]
    fn labeled_bnode_test() {
        let s = "_:alice";
        let res = labeled_bnode(s);
        assert_eq!(Ok(("", BlankNode::Labeled("alice"))), res);
    }

    #[test]
    fn predicate_lists_test() {
        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>
            <http://example.org/elements/atomicNumber>  2 ;
            <http://example.org/elements/atomicMass> 4.002602 ;
            <http://example.org/elements/isOk> true ;
            <http://example.org/elements/isNotOk> false ;
            <http://example.org/elements/specificGravity> 1.663E-4 .
        "#;
        let (_, res) = triples(s).unwrap();
        dbg!(&res);
        let s = r#"
            _:helium <http://example.org/elements/atomicNumber>  "2".
        "#;
        let (_, res) = triples(s).unwrap();
        dbg!(&res);

        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
        "#;
        let (_, res) = triples(s).unwrap();
        dbg!(res);

        let s = r#"
            <http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> "Spiderman", "Человек-паук"@ru .
        "#;
        let (_, res) = triples(s).unwrap();
        dbg!(res);
        let s = r#"
            <http://example.org/#spiderman> a person:Person,skos:Concept.
        "#;
        let (_, res) = triples(s).unwrap();
        dbg!(res);
    }

    #[test]
    fn with_unlabeled_type_bnode_test() {
        let s = r#"
        [foaf:name "Bob"] .
        "#;
        let (rest, res) = statements(s).unwrap();
        assert!(rest.trim().is_empty());
        assert_eq!(
            res,
            vec![TurtleValue::Statement {
                subject: Box::new(TurtleValue::BNode(BlankNode::Unlabeled,)),
                predicate_objects: vec![TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                        prefix: "foaf",
                        local_name: "name",
                    },)),
                    object: Box::new(TurtleValue::Literal(Literal::Quoted {
                        datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string",),),
                        value: Cow::Borrowed("Bob"),
                        lang: None,
                    },)),
                },],
            },]
        );
    }

    #[test] // NORDINE
    fn collection_test_from_other() {
        let s = r#"
        ex:techProducts ex:hasProducts ( ex:product1 ex:product2 )."#;

        let (rest, res) = statements(s).unwrap();
        assert!(rest.trim().is_empty());
        assert_eq!(
            res,
            vec![TurtleValue::Statement {
                subject: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "ex",
                    local_name: "techProducts",
                },)),
                predicate_objects: vec![TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                        prefix: "ex",
                        local_name: "hasProducts",
                    },)),
                    object: Box::new(TurtleValue::Collection(
                        [
                            TurtleValue::Iri(Iri::Prefixed {
                                prefix: "ex",
                                local_name: "product1",
                            },),
                            TurtleValue::Iri(Iri::Prefixed {
                                prefix: "ex",
                                local_name: "product2",
                            },),
                        ]
                        .into(),
                    )),
                },],
            },]
        );
    }

    #[test]
    fn unlabeled_nested_bnode() {
        let s = r#"
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .
				
        "#;
        let (rest, res) = triples(s).unwrap();
        assert!(rest.trim().is_empty());
        dbg!(res);

        let s = r#"[] foaf:knows [foaf:name "Bob"] ."#;
        let (rest, res) = triples(s).unwrap();
        assert!(rest.trim().is_empty());

        dbg!(res);
    }

    #[test]
    fn test_object_list() {
        let s = r#"
        @prefix ex: <http://example.org/ns#> .
        ex:ComplexResource  ex:hasValue 42 , "forty-two"@en .
        "#;
        let (rest, s) = statements(s).unwrap();
        assert!(rest.trim().is_empty());
        assert_eq!(
            s,
            vec![
                TurtleValue::Prefix(("ex", Iri::Enclosed("http://example.org/ns#",),),),
                TurtleValue::Statement {
                    subject: Box::new(TurtleValue::Iri(Iri::Prefixed {
                        prefix: "ex",
                        local_name: "ComplexResource",
                    },)),
                    predicate_objects: vec![TurtleValue::PredicateObject {
                        predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                            prefix: "ex",
                            local_name: "hasValue",
                        },)),
                        object: Box::new(TurtleValue::ObjectList(vec![
                            TurtleValue::Literal(Literal::Integer(42,),),
                            TurtleValue::Literal(Literal::Quoted {
                                datatype: None,
                                value: Cow::Borrowed("forty-two"),
                                lang: Some("en",),
                            },),
                        ],)),
                    },],
                },
            ]
        );
    }

    #[test]
    fn list_with_nested_collection() {
        let s = r#"
        # RDF List with nested collections
        ex:ComplexList ex:hasList (
            "First Item" 
            (
                "Nested Item 1"
                "Nested Item 2"
            )
            "Second Item"
        ) .
        "#;
        let (rest, res) = statements(s).unwrap();
        assert!(rest.trim().is_empty());
        assert_eq!(
            res,
            vec![TurtleValue::Statement {
                subject: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "ex",
                    local_name: "ComplexList",
                },)),
                predicate_objects: vec![TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                        prefix: "ex",
                        local_name: "hasList",
                    },)),
                    object: Box::new(TurtleValue::Collection(VecDeque::from([
                        TurtleValue::Literal(Literal::Quoted {
                            datatype: Some(Iri::Enclosed(
                                "http://www.w3.org/2001/XMLSchema#string",
                            ),),
                            value: Cow::Borrowed("First Item"),
                            lang: None,
                        },),
                        TurtleValue::Collection(
                            [
                                TurtleValue::Literal(Literal::Quoted {
                                    datatype: Some(Iri::Enclosed(
                                        "http://www.w3.org/2001/XMLSchema#string",
                                    ),),
                                    value: Cow::Borrowed("Nested Item 1"),
                                    lang: None,
                                },),
                                TurtleValue::Literal(Literal::Quoted {
                                    datatype: Some(Iri::Enclosed(
                                        "http://www.w3.org/2001/XMLSchema#string",
                                    ),),
                                    value: "Nested Item 2".into(),
                                    lang: None,
                                },),
                            ]
                            .into(),
                        ),
                        TurtleValue::Literal(Literal::Quoted {
                            datatype: Some(Iri::Enclosed(
                                "http://www.w3.org/2001/XMLSchema#string",
                            ),),
                            value: "Second Item".into(),
                            lang: None,
                        },),
                    ],))),
                },],
            },]
        );
    }

    #[test]
    fn collection_test() {
        let s = r#":a :b ( "apple" "banana" ) ."#;
        let (rest, res) = triples(s).unwrap();
        assert!(rest.trim().is_empty());
        dbg!(res);
        let s = r#"(1 2.0 3E1) :p "w" ."#;
        let (rest, res) = triples(s).unwrap();
        assert!(rest.trim().is_empty());
        dbg!(res);
        let s = r#"(1 [:p :q] ( 2 ) ) :p2 :q2 ."#;
        let (rest, res) = triples(s).unwrap();
        dbg!(res);
        assert!(rest.trim().is_empty());
        let s = r#":subject :predicate2 () ."#;
        let (rest, res) = triples(s).unwrap();
        assert!(rest.trim().is_empty());
        dbg!(res);
    }
}
