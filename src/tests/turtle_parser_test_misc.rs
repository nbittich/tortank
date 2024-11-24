use std::{borrow::Cow, collections::VecDeque};

use crate::{
    triple_common_parser::{
        iri::prefixed_iri,
        prologue::{base_sparql, base_turtle, prefix_sparql, prefix_turtle},
        triple::labeled_bnode,
        BlankNode, Iri, Literal,
    },
    turtle::turtle_parser::{statements, triples, TurtleValue},
};

#[test]
fn simple_collection_test() {
    let s = r#":a :b ( "apple" "banana" ) ."#;
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Iri(Iri::Prefixed {
                prefix: "",
                local_name: "a",
            },)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "b",
                },)),
                object: Box::new(TurtleValue::Collection(
                    [
                        TurtleValue::Literal(Literal::Quoted {
                            datatype: Some(Iri::Enclosed(
                                "http://www.w3.org/2001/XMLSchema#string",
                            ),),
                            value: "apple".into(),
                            lang: None,
                        },),
                        TurtleValue::Literal(Literal::Quoted {
                            datatype: Some(Iri::Enclosed(
                                "http://www.w3.org/2001/XMLSchema#string",
                            ),),
                            value: "banana".into(),
                            lang: None,
                        },),
                    ]
                    .into(),
                )),
            },]
            .into(),
        }
    );
    let s = r#"(1 2.0 3E1) :p "w" ."#;
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Collection(
                [
                    TurtleValue::Literal(Literal::Integer(1,),),
                    TurtleValue::Literal(Literal::Decimal(2.0,),),
                    TurtleValue::Literal(Literal::Decimal(30.0,),),
                ]
                .into(),
            )),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "p",
                },)),
                object: Box::new(TurtleValue::Literal(Literal::Quoted {
                    datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string",),),
                    value: "w".into(),
                    lang: None,
                },)),
            },]
            .into(),
        }
    );
    let s = r#"(1 [:p :q] ( 2 ) ) :p2 :q2 ."#;
    let (rest, res) = triples(s).unwrap();
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Collection(
                [
                    TurtleValue::Literal(Literal::Integer(1,),),
                    TurtleValue::Statement {
                        subject: Box::new(TurtleValue::BNode(BlankNode::Unlabeled,)),
                        predicate_objects: [TurtleValue::PredicateObject {
                            predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                                prefix: "",
                                local_name: "p",
                            },)),
                            object: Box::new(TurtleValue::Iri(Iri::Prefixed {
                                prefix: "",
                                local_name: "q",
                            },)),
                        },]
                        .into(),
                    },
                    TurtleValue::Collection([TurtleValue::Literal(Literal::Integer(2,),),].into(),),
                ]
                .into(),
            )),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "p2",
                },)),
                object: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "q2",
                },)),
            },]
            .into()
        }
    );
    assert!(rest.trim().is_empty());
    let s = r#":subject :predicate2 () ."#;
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Iri(Iri::Prefixed {
                prefix: "",
                local_name: "subject",
            },)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "predicate2",
                },)),
                object: Box::new(TurtleValue::Iri(Iri::Enclosed(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil",
                ),),)
            },]
            .into(),
        }
    );
}

#[test]
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
                        datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string",),),
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
                        datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string",),),
                        value: "Second Item".into(),
                        lang: None,
                    },),
                ],))),
            },],
        },]
    );
}

#[test]
fn labeled_bnode_using_proper_rules_test1() {
    let s = "_:b.node :a :b";
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::BNode(BlankNode::Labeled("b.node",),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "a",
                },)),
                object: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "b",
                },)),
            },]
            .into(),
        }
    );
}

#[test]
fn labeled_bnode_using_proper_rules_test2() {
    let s = "_:b-node :a :b";
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::BNode(BlankNode::Labeled("b-node",),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "a",
                },)),
                object: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "b",
                },)),
            },]
            .into(),
        }
    );
}

#[test]
fn labeled_bnode_using_proper_rules_test3() {
    let s = "_:b·node :a :b";
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::BNode(BlankNode::Labeled("b·node",),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "a",
                },)),
                object: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "b",
                },)),
            },]
            .into(),
        }
    );
}

#[test]
fn labeled_bnode_using_proper_rules_test4() {
    let s = "_:b-jöhn :a :b";
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::BNode(BlankNode::Labeled("b-jöhn",),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "a",
                },)),
                object: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "b",
                },)),
            },]
            .into(),
        }
    );
}

#[test]
fn labeled_bnode_using_proper_rules_test5() {
    let s = "_:b_undertie‿node :a :b";
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::BNode(BlankNode::Labeled("b_undertie‿node",),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "a",
                },)),
                object: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "",
                    local_name: "b",
                },)),
            },]
            .into(),
        }
    );
}

#[test]
fn predicate_labeled_bnode_test() {
    let s = r#"
        <http://example.org/ns#ComplexResource> <http://example.org/ns#hasNestedObject> _:1.

        "#;
    let (rest, res) = statements(s).unwrap();
    assert!(rest.trim().is_empty());
    assert_eq!(
        res,
        vec![TurtleValue::Statement {
            subject: Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://example.org/ns#ComplexResource",
            ),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                    "http://example.org/ns#hasNestedObject",
                ),)),
                object: Box::new(TurtleValue::BNode(BlankNode::Labeled("1",),)),
            },]
            .into(),
        },]
    );
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

#[test]
fn labeled_bnode_test() {
    let s = "_:alice";
    let res = labeled_bnode(s);
    assert_eq!(Ok(("", BlankNode::Labeled("alice"))), res);
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
fn unlabeled_nested_bnodes() {
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
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Statement {
                subject: Box::new(TurtleValue::BNode(BlankNode::Unlabeled,)),
                predicate_objects: [TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                        prefix: "foaf",
                        local_name: "name",
                    },)),
                    object: Box::new(TurtleValue::Literal(Literal::Quoted {
                        datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string",),),
                        value: "Alice".into(),
                        lang: None,
                    },)),
                },]
                .into(),
            }),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "foaf",
                    local_name: "knows",
                },)),
                object: Box::new(TurtleValue::Statement {
                    subject: Box::new(TurtleValue::BNode(BlankNode::Unlabeled,)),
                    predicate_objects: [
                        TurtleValue::PredicateObject {
                            predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                                prefix: "foaf",
                                local_name: "name",
                            },)),
                            object: Box::new(TurtleValue::Literal(Literal::Quoted {
                                datatype: Some(Iri::Enclosed(
                                    "http://www.w3.org/2001/XMLSchema#string",
                                ),),
                                value: "Bob".into(),
                                lang: None,
                            },)),
                        },
                        TurtleValue::PredicateObject {
                            predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                                prefix: "foaf",
                                local_name: "lastName",
                            },)),
                            object: Box::new(TurtleValue::ObjectList(
                                [
                                    TurtleValue::Literal(Literal::Quoted {
                                        datatype: Some(Iri::Enclosed(
                                            "http://www.w3.org/2001/XMLSchema#string",
                                        ),),
                                        value: "George".into(),
                                        lang: None,
                                    },),
                                    TurtleValue::Literal(Literal::Quoted {
                                        datatype: Some(Iri::Enclosed(
                                            "http://www.w3.org/2001/XMLSchema#string",
                                        ),),
                                        value: "Joshua".into(),
                                        lang: None,
                                    },),
                                ]
                                .into(),
                            )),
                        },
                        TurtleValue::PredicateObject {
                            predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                                prefix: "foaf",
                                local_name: "knows",
                            },)),
                            object: Box::new(TurtleValue::Statement {
                                subject: Box::new(TurtleValue::BNode(BlankNode::Unlabeled,)),
                                predicate_objects: [TurtleValue::PredicateObject {
                                    predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                                        prefix: "foaf",
                                        local_name: "name",
                                    },)),
                                    object: Box::new(TurtleValue::Literal(Literal::Quoted {
                                        datatype: Some(Iri::Enclosed(
                                            "http://www.w3.org/2001/XMLSchema#string",
                                        ),),
                                        value: "Eve".into(),
                                        lang: None,
                                    },)),
                                },]
                                .into(),
                            }),
                        },
                        TurtleValue::PredicateObject {
                            predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                                prefix: "foaf",
                                local_name: "mbox",
                            },)),
                            object: Box::new(TurtleValue::Iri(Iri::Enclosed("bob@example.com",),)),
                        },
                    ]
                    .into(),
                }),
            },]
            .into(),
        }
    );

    let s = r#"[] foaf:knows [foaf:name "Bob"] ."#;
    let (rest, res) = triples(s).unwrap();
    assert!(rest.trim().is_empty());

    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::BNode(BlankNode::Unlabeled,)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                    prefix: "foaf",
                    local_name: "knows",
                },)),
                object: Box::new(TurtleValue::Statement {
                    subject: Box::new(TurtleValue::BNode(BlankNode::Unlabeled,)),
                    predicate_objects: [TurtleValue::PredicateObject {
                        predicate: Box::new(TurtleValue::Iri(Iri::Prefixed {
                            prefix: "foaf",
                            local_name: "name",
                        },)),
                        object: Box::new(TurtleValue::Literal(Literal::Quoted {
                            datatype: Some(Iri::Enclosed(
                                "http://www.w3.org/2001/XMLSchema#string",
                            ),),
                            value: "Bob".into(),
                            lang: None,
                        },)),
                    },]
                    .into(),
                }),
            },]
            .into(),
        }
    );
}

#[test]
fn predicate_lists_testx() {
    let s = r#"
            <http://en.wikipedia.org/wiki/Helium>
            <http://example.org/elements/atomicNumber>  2 ;
            <http://example.org/elements/atomicMass> 4.002602 ;
            <http://example.org/elements/isOk> true ;
            <http://example.org/elements/isNotOk> false ;
            <http://example.org/elements/specificGravity> 1.663E-4 .
        "#;
    let (_, res) = triples(s).unwrap();
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://en.wikipedia.org/wiki/Helium",
            ),)),
            predicate_objects: [
                TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                        "http://example.org/elements/atomicNumber",
                    ),)),
                    object: Box::new(TurtleValue::Literal(Literal::Integer(2,),)),
                },
                TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                        "http://example.org/elements/atomicMass",
                    ),)),
                    object: Box::new(TurtleValue::Literal(Literal::Decimal(4.002602,),)),
                },
                TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                        "http://example.org/elements/isOk",
                    ),)),
                    object: Box::new(TurtleValue::Literal(Literal::Boolean(true,),)),
                },
                TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                        "http://example.org/elements/isNotOk",
                    ),)),
                    object: Box::new(TurtleValue::Literal(Literal::Boolean(false,),)),
                },
                TurtleValue::PredicateObject {
                    predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                        "http://example.org/elements/specificGravity",
                    ),)),
                    object: Box::new(TurtleValue::Literal(Literal::Decimal(0.0001663,),)),
                },
            ]
            .into(),
        }
    );
    let s = r#"
            _:helium <http://example.org/elements/atomicNumber>  "2".
        "#;
    let (_, res) = triples(s).unwrap();
    dbg!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::BNode(BlankNode::Labeled("helium",),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                    "http://example.org/elements/atomicNumber",
                ),)),
                object: Box::new(TurtleValue::Literal(Literal::Quoted {
                    datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string",),),
                    value: "2".into(),
                    lang: None,
                },)),
            },]
            .into(),
        }
    );

    let s = r#"
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
        "#;
    let (_, res) = triples(s).unwrap();
    dbg!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://en.wikipedia.org/wiki/Helium",
            ),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                    "http://example.org/elements/atomicNumber",
                ),)),
                object: Box::new(TurtleValue::Literal(Literal::Quoted {
                    datatype: Some(Iri::Enclosed("http://www.w3.org/2001/XMLSchema#string",),),
                    value: "2".into(),
                    lang: None,
                },)),
            },]
            .into(),
        }
    );

    let s = r#"
            <http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> "Spiderman", "Человек-паук"@ru .
        "#;
    let (_, res) = triples(s).unwrap();
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://example.org/#spiderman",
            ),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                    "http://xmlns.com/foaf/0.1/name",
                ),)),
                object: Box::new(TurtleValue::ObjectList(
                    [
                        TurtleValue::Literal(Literal::Quoted {
                            datatype: Some(Iri::Enclosed(
                                "http://www.w3.org/2001/XMLSchema#string",
                            ),),
                            value: "Spiderman".into(),
                            lang: None,
                        },),
                        TurtleValue::Literal(Literal::Quoted {
                            datatype: None,
                            value: "Человек-паук".into(),
                            lang: Some("ru",),
                        },),
                    ]
                    .into(),
                )),
            },]
            .into(),
        }
    );
    let s = r#"
            <http://example.org/#spiderman> a person:Person,skos:Concept.
        "#;
    let (_, res) = triples(s).unwrap();
    assert_eq!(
        res,
        TurtleValue::Statement {
            subject: Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://example.org/#spiderman",
            ),)),
            predicate_objects: [TurtleValue::PredicateObject {
                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                ),)),
                object: Box::new(TurtleValue::ObjectList(
                    [
                        TurtleValue::Iri(Iri::Prefixed {
                            prefix: "person",
                            local_name: "Person",
                        },),
                        TurtleValue::Iri(Iri::Prefixed {
                            prefix: "skos",
                            local_name: "Concept",
                        },),
                    ]
                    .into(),
                )),
            },]
            .into(),
        }
    );
}
