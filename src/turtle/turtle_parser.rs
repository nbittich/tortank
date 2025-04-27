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

fn predicate_lists<'a, F>(
    subject_extractor: F,
) -> impl FnMut(&'a str) -> ParserResult<'a, TurtleValue<'a>>
where
    F: Fn(&'a str) -> ParserResult<'a, TurtleValue<'a>>,
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

pub(crate) fn triples(s: &str) -> ParserResult<TurtleValue> {
    terminated(
        alt((predicate_lists(subject), anon_bnode_turtle)),
        preceded(multispace0, terminated(alt((tag("."), eof)), multispace0)),
    )(s)
}

pub(crate) fn ntriple_statement(s: &str) -> ParserResult<TurtleValue> {
    preceded(
        comments,
        terminated(
            map(
                tuple((ntriple_subject, ntriple_predicate, ntriple_object)),
                |(s, p, o)| TurtleValue::Statement {
                    subject: Box::new(s),
                    predicate_objects: vec![TurtleValue::PredicateObject {
                        predicate: Box::new(p),
                        object: Box::new(o),
                    }],
                },
            ),
            preceded(multispace0, terminated(alt((tag("."), eof)), multispace0)),
        ),
    )(s)
}
pub(crate) fn ntriple_subject(s: &str) -> ParserResult<TurtleValue> {
    alt((map(labeled_bnode, TurtleValue::BNode), iri_turtle))(s)
}
pub(crate) fn ntriple_predicate(s: &str) -> ParserResult<TurtleValue> {
    alt((map(ns_type, TurtleValue::Iri), iri_turtle))(s)
}

pub(crate) fn ntriple_object(s: &str) -> ParserResult<TurtleValue> {
    preceded(
        multispace0,
        alt((
            iri_turtle,
            map(labeled_bnode, TurtleValue::BNode),
            literal_turtle,
        )),
    )(s)
}

fn directive(s: &str) -> ParserResult<TurtleValue> {
    let base_to_turtle = map(alt((base_sparql, base_turtle)), TurtleValue::Base);
    let prefix_to_turtle = map(alt((prefix_turtle, prefix_sparql)), TurtleValue::Prefix);
    alt((base_to_turtle, prefix_to_turtle))(s)
}

pub(crate) fn statement(s: &str) -> ParserResult<TurtleValue> {
    preceded(comments, alt((directive, triples)))(s)
}

pub fn statements(s: &str) -> ParserResult<Vec<TurtleValue>> {
    many0(statement)(s)
}
