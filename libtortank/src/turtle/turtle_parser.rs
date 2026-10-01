use std::collections::VecDeque;

use nom_language::error::VerboseError;

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
use crate::triple_common_parser::{BlankNode, Iri, Literal, comments};

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

fn object_lists(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    object_list(object, TurtleValue::ObjectList).parse(s)
}

fn predicate_lists<'a, F>(
    subject_extractor: F,
) -> impl Parser<&'a str, Output = TurtleValue<'a>, Error = VerboseError<&'a str>>
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

fn collection_turtle(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    map(collection(object), |res: VecDeque<TurtleValue>| {
        if res.is_empty() {
            TurtleValue::Iri(Iri::Enclosed(RDF_NIL))
        } else {
            TurtleValue::Collection(res)
        }
    })
    .parse(s)
}

fn anon_bnode_turtle(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    let unlabeled_subject = |s| Ok((s, TurtleValue::BNode(BlankNode::Unlabeled)));
    anon_bnode(alt((predicate_lists(unlabeled_subject), unlabeled_subject))).parse(s)
}

fn blank_node(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    alt((map(labeled_bnode, TurtleValue::BNode), anon_bnode_turtle)).parse(s)
}

fn iri_turtle(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    map(iri, TurtleValue::Iri).parse(s)
}
fn literal_turtle(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    map(literal, TurtleValue::Literal).parse(s)
}

pub(crate) fn subject(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    alt((blank_node, iri_turtle, collection_turtle)).parse(s)
}
pub(crate) fn predicate(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    alt((map(ns_type, TurtleValue::Iri), iri_turtle)).parse(s)
}

pub(crate) fn object(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    preceded(
        multispace0,
        alt((iri_turtle, blank_node, collection_turtle, literal_turtle)),
    )
    .parse(s)
}

pub(crate) fn triples(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    terminated(
        alt((predicate_lists(subject), anon_bnode_turtle)),
        preceded(multispace0, terminated(alt((tag("."), eof)), multispace0)),
    )
    .parse(s)
}

pub(crate) fn ntriple_statement(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    preceded(
        comments,
        terminated(
            map(
                (ntriple_subject, ntriple_predicate, ntriple_object),
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
    )
    .parse(s)
}
pub(crate) fn ntriple_subject(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    alt((map(labeled_bnode, TurtleValue::BNode), iri_turtle)).parse(s)
}
pub(crate) fn ntriple_predicate(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    alt((map(ns_type, TurtleValue::Iri), iri_turtle)).parse(s)
}

pub(crate) fn ntriple_object(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    preceded(
        multispace0,
        alt((
            iri_turtle,
            map(labeled_bnode, TurtleValue::BNode),
            literal_turtle,
        )),
    )
    .parse(s)
}

fn directive(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    let base_to_turtle = map(alt((base_sparql, base_turtle)), TurtleValue::Base);
    let prefix_to_turtle = map(alt((prefix_turtle, prefix_sparql)), TurtleValue::Prefix);
    alt((base_to_turtle, prefix_to_turtle)).parse(s)
}

pub(crate) fn statement(s: &'_ str) -> ParserResult<'_, TurtleValue<'_>> {
    preceded(comments, alt((directive, triples))).parse(s)
}

pub fn statements(s: &'_ str) -> ParserResult<'_, Vec<TurtleValue<'_>>> {
    many0(statement).parse(s)
}
