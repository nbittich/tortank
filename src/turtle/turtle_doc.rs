use crate::grammar::{
    BLANK_NODE_LABEL, STRING_LITERAL_LONG_QUOTE, STRING_LITERAL_LONG_SINGLE_QUOTE,
};
use crate::iri::IRI;
use crate::shared::{
    DATE_FORMATS, DEFAULT_DATE_FORMAT, DEFAULT_TIME_FORMAT, RDF_FIRST, RDF_NIL, RDF_REST,
    TIME_FORMATS, XSD_BOOLEAN, XSD_DATE, XSD_DATE_TIME, XSD_DECIMAL, XSD_DOUBLE, XSD_INTEGER,
    XSD_TIME,
};
use crate::triple_common_parser::{BlankNode, Iri};
use crate::triple_common_parser::{Literal as ASTLiteral, comments};
use crate::turtle::turtle_parser::{
    TurtleValue, object as parse_obj, predicate as parse_pred, statements, subject as parse_sub,
};
use crate::utils::XSD_STRING;
use chrono::{DateTime, FixedOffset, Local, NaiveDateTime, SecondsFormat};
use serde_derive::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};
use std::fs::{File, OpenOptions};
use std::io::Read;
use std::io::{BufReader, prelude::*};
use std::num::{ParseFloatError, ParseIntError};
use std::ops::Add;
use std::path::PathBuf;
use std::str::ParseBoolError;
use std::sync::Arc;

use super::turtle_parser::ntriple_statement;

#[cfg(test)]
static FAKE_UUID_GEN: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

#[cfg(test)]
pub(crate) fn reset_fake_uuid_gen() {
    FAKE_UUID_GEN.store(0, std::sync::atomic::Ordering::SeqCst);
}

#[cfg(not(test))]
fn get_uuid() -> String {
    uuid::Uuid::now_v7().to_string()
}
#[cfg(test)]
fn get_uuid() -> String {
    FAKE_UUID_GEN.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    format!(
        "{}",
        FAKE_UUID_GEN.load(std::sync::atomic::Ordering::SeqCst)
    )
}
const SPECIAL_TTL_RDF_TYPE_PREFIX: (&str, &str) = ("rdf:type", "a");
const PREFIX_XSD: (&str, &str) = ("xsd:", "http://www.w3.org/2001/XMLSchema#");
const XSD_DATATYPE_FN: fn(&str) -> String = |s| format!("^^xsd:{s}");
const PREFIXES: &[(&str, &str)] = &[
    ("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
    ("org:", "http://www.w3.org/ns/org#"),
    ("rdfs:", "http://www.w3.org/2000/01/rdf-schema#"),
    PREFIX_XSD,
    ("foaf:", "http://xmlns.com/foaf/0.1/"),
    ("dc:", "http://purl.org/dc/elements/1.1/"),
    ("dcterms:", "http://purl.org/dc/terms/"),
    ("skos:", "http://www.w3.org/2004/02/skos/core#"),
    ("prov:", "http://www.w3.org/ns/prov#"),
    ("schema:", "http://schema.org/"),
    ("dcat:", "http://www.w3.org/ns/dcat#"),
    ("adms:", "http://www.w3.org/ns/adms#"),
    ("tree:", "https://w3id.org/tree#"),
    ("qunit:", "http://qudt.org/vocab/unit/"),
    ("quantitykind:", "http://qudt.org/vocab/quantitykind/"),
    ("vs:", "http://www.w3.org/2003/06/sw-vocab-status/ns#"),
    ("tribont:", "https://w3id.org/tribont/core#"),
    (
        "conceptscheme:",
        "http://data.vlaanderen.be/id/conceptscheme/",
    ),
    ("cidoc:", "http://www.cidoc-crm.org/cidoc-crm/"),
    ("mu:", "http://mu.semte.ch/vocabularies/core/"),
    ("besluit:", "http://data.vlaanderen.be/ns/besluit#"),
    ("mandaat:", "http://data.vlaanderen.be/ns/mandaat#"),
    ("eli:", "http://data.europa.eu/eli/ontology#"),
    ("euvoc:", "http://publications.europa.eu/ontology/euvoc#"),
    ("mobiliteit:", "https://data.vlaanderen.be/ns/mobiliteit#"),
    ("ldes:", "http://w3id.org/ldes#"),
    ("owl:", "http://www.w3.org/2002/07/owl#"),
    ("qb:", "http://purl.org/linked-data/cube#"),
    ("time:", "http://www.w3.org/2006/time#"),
    ("geo:", "http://www.w3.org/2003/01/geo/wgs84_pos#"),
    ("vcard:", "http://www.w3.org/2006/vcard/ns#"),
    ("cc:", "http://creativecommons.org/ns#"),
    ("rdfa:", "http://www.w3.org/ns/rdfa#"),
    ("ssn:", "http://www.w3.org/ns/ssn/"),
    ("rr:", "http://www.w3.org/ns/r2rml#"),
    ("wot:", "http://xmlns.com/wot/0.1/"),
    ("dbo:", "http://dbpedia.org/ontology/"),
    ("dbp:", "http://dbpedia.org/property/"),
    ("dbpprop:", "http://dbpedia.org/property/"),
    ("ex:", "http://example.org/"),
    ("bibo:", "http://purl.org/ontology/bibo/"),
    ("obo:", "http://purl.obolibrary.org/obo/"),
    ("ext:", "http://mu.semte.ch/vocabularies/ext/"),
    ("qudt:", "http://qudt.org/schema/qudt/"),
    ("geo:", "<http://www.opengis.net/ont/geosparql#>"),
];
const PREFIX_OR_NONE: fn(&str, &mut HashMap<&'static str, &'static str>) -> Option<String> =
    |s, used_prefixes| {
        let mut prefixes: Vec<_> = PREFIXES.iter().collect();
        prefixes.sort_by(|(_, a), (_, b)| b.len().cmp(&a.len())); // prevent prefix collisions

        prefixes.iter().find_map(|(p, uri)| {
            if s.contains(uri) {
                used_prefixes.insert(p, uri);
                let replaced = s.replace(uri, p);
                if replaced == SPECIAL_TTL_RDF_TYPE_PREFIX.0 {
                    Some(SPECIAL_TTL_RDF_TYPE_PREFIX.1.to_string())
                } else {
                    Some(replaced)
                }
            } else {
                None
            }
        })
    };

#[derive(PartialEq, Debug)]
pub struct TurtleDocError {
    pub message: String,
}

struct Context<'a> {
    base: Option<&'a str>,
    well_known_prefix: Option<String>,
    prefixes: BTreeMap<Cow<'a, str>, Cow<'a, str>>,
}

#[derive(Serialize, PartialEq, Deserialize, Clone, Debug)]
pub struct RdfJsonNode {
    #[serde(rename = "type")]
    pub typ: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub datatype: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lang: Option<String>,
    pub value: String,
}

#[derive(Serialize, PartialEq, Deserialize, Clone, Debug)]
#[serde(untagged)]
pub enum RdfJsonNodeResult {
    SingleNode(RdfJsonNode),
    ListNodes(Vec<RdfJsonNodeResult>),
}

#[derive(Serialize, PartialEq, Deserialize, Clone, Debug)]
pub struct RdfJsonTriple {
    pub subject: RdfJsonNodeResult,
    pub predicate: RdfJsonNodeResult,
    pub object: RdfJsonNodeResult,
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Literal<'a> {
    Quoted {
        datatype: Option<Box<Node<'a>>>,
        value: Cow<'a, str>,
        lang: Option<&'a str>,
    },
    Double(f64),
    Decimal(f32),
    Integer(i64),
    Boolean(bool),
    Date(DateTime<FixedOffset>),
    DateTime(DateTime<FixedOffset>),
    Time(DateTime<FixedOffset>),
}
#[derive(Debug, PartialOrd, Clone)]
pub enum Node<'a> {
    Iri(Cow<'a, str>),
    Literal(Literal<'a>),
    Ref(Arc<Node<'a>>),
    List(Vec<Node<'a>>),
    LabeledBlankNode(String),
}
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub struct Statement<'a> {
    pub subject: Node<'a>,
    pub predicate: Node<'a>,
    pub object: Node<'a>,
}
#[derive(PartialEq, PartialOrd, Debug, Default)]
pub struct TurtleDoc<'a> {
    well_known_prefix: Option<String>,
    base: Option<&'a str>,
    prefixes: BTreeMap<Cow<'a, str>, Cow<'a, str>>,
    statements: Vec<Statement<'a>>,
}

impl<'a> Node<'a> {
    pub fn get_iri(&self) -> Result<Cow<'a, str>, TurtleDocError> {
        match self {
            Node::Iri(cow) => Ok(cow.clone()),
            Node::Ref(node) => node.get_iri(),
            Node::LabeledBlankNode(_) => Ok(Cow::Owned(self.to_string())),
            node => Err(TurtleDocError {
                message: format!("node is not an iri {node}"),
            }),
        }
    }
}

impl<'a> Statement<'a> {
    pub fn from_rdf_json_triples(
        triples: &'a [RdfJsonTriple],
    ) -> Result<Vec<Statement<'a>>, TurtleDocError> {
        let mut stmts = Vec::with_capacity(triples.len());
        for triple in triples {
            stmts.push(Statement::try_from(triple)?);
        }
        Ok(stmts)
    }
}
impl RdfJsonTriple {
    pub fn to_json_string(&self) -> Result<String, TurtleDocError> {
        serde_json::to_string(self).map_err(|e| TurtleDocError {
            message: e.to_string(),
        })
    }
    pub fn from_json(value: &str) -> Result<Vec<RdfJsonTriple>, TurtleDocError> {
        serde_json::from_str(value).map_err(|e| TurtleDocError {
            message: e.to_string(),
        })
    }
    pub fn from_json_file(value: impl Into<PathBuf>) -> Result<Vec<RdfJsonTriple>, TurtleDocError> {
        let file = File::open(value.into()).map_err(|e| TurtleDocError {
            message: e.to_string(),
        })?;
        serde_json::from_reader(BufReader::new(file)).map_err(|e| TurtleDocError {
            message: e.to_string(),
        })
    }
}
impl<'a> TurtleDoc<'a> {
    pub fn from_file(
        path: impl Into<PathBuf>,
        well_known_prefix: Option<String>,
        buf: &'a mut String,
    ) -> Result<Self, TurtleDocError> {
        let path = path.into();
        let extension = path.extension().and_then(|p| p.to_str());
        if !path.exists() || (extension != Some("ttl") && extension != Some("n3")) {
            return Err(TurtleDocError {
                message: format!("file {path:?} doesn't exist or extension not n3/ttl"),
            });
        }
        let mut file = File::open(&path).map_err(|err| TurtleDocError {
            message: format!("cannot open file: {err}"),
        })?;

        file.read_to_string(buf).map_err(|err| TurtleDocError {
            message: format!("cannot read file: {err}"),
        })?;
        (buf.as_str(), well_known_prefix).try_into()
    }
    pub fn add_prefixes(
        &mut self,
        prefixes: BTreeMap<String, String>,
    ) -> Result<(), TurtleDocError> {
        let base = self.base.unwrap_or("");
        let mut prefixes: BTreeMap<Cow<str>, Cow<str>> = prefixes
            .into_iter()
            .map(|(k, v)| (Cow::Owned(k), Cow::Owned(v)))
            .collect();
        for (_, prefix) in prefixes.iter_mut() {
            let iri = IRI::try_from(prefix.as_ref()).map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;
            if iri.is_relative() {
                *prefix = Cow::Owned(format!("{base}{prefix}"));
            }
        }
        self.prefixes.extend(prefixes);
        Ok(())
    }
    pub fn add_statement(&mut self, subject: Node<'a>, predicate: Node<'a>, object: Node<'a>) {
        let stmt = Statement {
            subject,
            predicate,
            object,
        };
        if !self.statements.contains(&stmt) {
            self.statements.push(stmt);
        }
    }
    pub fn len(&self) -> usize {
        self.statements.len()
    }
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }

    /// creates a new, indepependent, turtle_doc containing all the statements in this turtle_doc
    /// that are not in another.
    pub fn difference(&'_ self, other: &TurtleDoc) -> Result<TurtleDoc<'_>, TurtleDocError> {
        let diff = self
            .statements
            .iter()
            .filter(|stmt| !other.statements.iter().any(|stmt2| stmt == &stmt2))
            .cloned()
            .collect::<Vec<_>>();

        diff.try_into()
    }

    /// creates a new, indepependent, turtle_doc containing all statements that are in both this
    /// model and another
    pub fn intersection(&'_ self, other: &TurtleDoc) -> Result<TurtleDoc<'_>, TurtleDocError> {
        let intersection = self
            .statements
            .iter()
            .filter(|stmt| other.statements.iter().any(|stmt2| stmt == &stmt2))
            .cloned()
            .collect::<Vec<_>>();

        intersection.try_into()
    }

    pub fn all_subjects(&self) -> Vec<Node<'_>> {
        let mut subjects = self
            .statements
            .iter()
            .map(|st| st.subject.clone())
            .collect::<Vec<_>>();

        subjects.dedup();
        subjects
    }

    pub fn parse_ntriples_statement(
        s: &'a str,
    ) -> Result<Option<(&'a str, Statement<'a>)>, TurtleDocError> {
        if s.trim().is_empty() {
            return Ok(None);
        }
        let (rest, stmt) = ntriple_statement(s).map_err(|e| TurtleDocError {
            message: e.to_string(),
        })?;
        let mut res = Vec::with_capacity(1);
        Self::get_node(
            stmt,
            &Context {
                base: None,
                well_known_prefix: None,
                prefixes: BTreeMap::new(),
            },
            &mut res,
        )?;
        if res.is_empty() {
            return Ok(None);
        }
        Ok(Some((rest, res.remove(0))))
    }

    pub fn parse_and_list_statements(
        &'_ self,
        subject: Option<String>,
        predicate: Option<String>,
        object: Option<String>,
    ) -> Result<Vec<&'_ Statement<'_>>, TurtleDocError> {
        let mut statements: Vec<&Statement> = self.statements.iter().collect();
        let prefixes: BTreeMap<Cow<str>, Cow<str>> = self.prefixes.clone();
        let base = self.base.map(Cow::Borrowed);

        if let Some(subject) = subject {
            let (_, s) = parse_sub(&subject).map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;

            let subject =
                &Self::simple_turtle_value_to_node(s, base.clone(), prefixes.clone(), false)?;
            statements.retain(|s| &s.subject == subject);
        }
        if let Some(predicate) = predicate {
            let (_, p) = parse_pred(&predicate).map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;

            let predicate =
                &Self::simple_turtle_value_to_node(p, base.clone(), prefixes.clone(), false)?;
            statements.retain(|s| &s.predicate == predicate);
        }
        if let Some(object) = object {
            let (_, o) = parse_obj(&object).map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;

            let object = &Self::simple_turtle_value_to_node(o, base.clone(), prefixes, true)?;
            statements.retain(|s| &s.object == object);
        }

        Ok(statements)
    }

    /// list statements in document
    /// takes ?s ?p ?o as optional parameters to filter more stuff
    pub fn list_statements(
        &'_ self,
        subject: Option<&Node>,
        predicate: Option<&Node>,
        object: Option<&Node>,
    ) -> Vec<&'_ Statement<'_>> {
        let mut statements: Vec<&Statement> = self.statements.iter().collect();

        if let Some(subject) = subject {
            statements.retain(|s| &s.subject == subject);
        }
        if let Some(predicate) = predicate {
            statements.retain(|s| &s.predicate == predicate);
        }
        if let Some(object) = object {
            statements.retain(|s| &s.object == object);
        }
        statements
    }

    pub fn to_file(
        &self,
        path: impl Into<PathBuf>,
        buf_size: Option<usize>,
        json: bool,
    ) -> Result<(), TurtleDocError> {
        let path = path.into();
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;
        }
        let buf_size = if let Some(buf_size) = buf_size {
            buf_size
        } else {
            self.len()
        };
        let mut file = OpenOptions::new()
            .create_new(true)
            .append(true)
            .open(path)
            .map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;

        if json {
            // hackish bcos we buffer stmt
            write!(&mut file, "[").map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;
        }

        for chunk in self.statements.chunks(buf_size) {
            let out = if json {
                let chunk = chunk
                    .iter()
                    .map(Into::<RdfJsonTriple>::into)
                    .map(|rdf| rdf.to_json_string())
                    .collect::<Result<Vec<String>, _>>()?;
                chunk.join(",")
            } else {
                chunk
                    .iter()
                    .map(|stmt| stmt.to_string())
                    .collect::<Vec<String>>()
                    .join("\n")
            };
            file.write_all(out.as_bytes()).map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;
        }
        if json {
            // hackish bcos we buffer stmt
            write!(&mut file, "]").map_err(|e| TurtleDocError {
                message: e.to_string(),
            })?;
        }

        writeln!(&mut file).map_err(|e| TurtleDocError {
            message: e.to_string(),
        })?;
        Ok(())
    }

    fn new(
        turtle_values: Vec<TurtleValue<'a>>,
        well_known_prefix: Option<String>,
    ) -> Result<Self, TurtleDocError> {
        // let well_known_prefix =
        //     well_known_prefix.unwrap_or_else(|| DEFAULT_WELL_KNOWN_PREFIX.to_string());
        let mut context = Context {
            base: None,
            well_known_prefix,
            prefixes: BTreeMap::new(),
        };
        let mut statements: Vec<Statement> = vec![];

        for turtle_value in turtle_values {
            match turtle_value {
                TurtleValue::Base(base) => {
                    context.base = Some(Self::extract_iri(base)?);
                }
                TurtleValue::Prefix((k, prefix)) => {
                    let mut prefix = Cow::Borrowed(TurtleDoc::extract_iri(prefix)?);
                    let k = Cow::Borrowed(k);
                    let base = context.base.unwrap_or("");
                    let iri = IRI::try_from(prefix.as_ref()).map_err(|e| TurtleDocError {
                        message: e.to_string(),
                    })?;
                    if iri.is_relative() {
                        prefix = Cow::Owned(format!("{base}{prefix}"));
                    }
                    context.prefixes.insert(k, prefix);
                }
                statement @ TurtleValue::Statement {
                    subject: _,
                    predicate_objects: _,
                } => {
                    Self::get_node(statement, &context, &mut statements)?;
                }
                _ => {
                    return Err(TurtleDocError {
                        message: "incorrect turtle value".into(),
                    });
                }
            }
        }

        statements.dedup();
        Ok(TurtleDoc {
            base: context.base,
            well_known_prefix: context.well_known_prefix,
            statements,
            prefixes: context.prefixes,
        })
    }

    fn extract_iri(value: Iri<'_>) -> Result<&str, TurtleDocError> {
        if let Iri::Enclosed(iri) = value {
            Ok(iri)
        } else {
            Err(TurtleDocError {
                message: "Not enclosed iri".into(),
            })
        }
    }
    fn simple_turtle_value_to_node<'x>(
        s: TurtleValue<'x>,
        base: Option<Cow<str>>,
        prefixes: BTreeMap<Cow<str>, Cow<str>>,
        allow_literals: bool,
    ) -> Result<Node<'x>, TurtleDocError> {
        match s {
            TurtleValue::Iri(Iri::Enclosed(iri)) => {
                // FIXME this is better but slow.
                // Call if iri.is_relative instead of has_scheme
                // let iri_rfc3987 = IRI::try_from(iri).map_err(|e| TurtleDocError {
                //     message: e.to_string(),
                // })?;
                if !IRI::has_scheme(iri)
                    && let Some(base) = base
                {
                    let iri = (*base).to_owned() + iri;
                    return Ok(Node::Iri(Cow::Owned(iri.to_string())));
                }
                Ok(Node::Iri(Cow::Borrowed(iri)))
            }
            TurtleValue::Iri(Iri::Prefixed { prefix, local_name }) => {
                let Some(prefix) = prefixes.get(prefix) else {
                    return Err(TurtleDocError {
                        message: format!("prefix {prefix} unknown"),
                    });
                };

                let iri = prefix.clone() + local_name;
                Ok(Node::Iri(Cow::Owned(iri.to_string())))
                //
            }
            TurtleValue::Literal(literal) if allow_literals => match literal {
                ASTLiteral::Quoted {
                    datatype,
                    value,
                    lang,
                } => {
                    let datatype = if let Some(dt) = datatype {
                        let dt = Self::simple_turtle_value_to_node(
                            TurtleValue::Iri(dt),
                            base,
                            prefixes,
                            allow_literals,
                        )?;
                        Some(dt)
                    } else {
                        None
                    };
                    match datatype {
                        Some(Node::Iri(iri)) if iri == XSD_BOOLEAN => {
                            Ok(Node::Literal(Literal::Boolean(value.parse().map_err(
                                |e: ParseBoolError| TurtleDocError {
                                    message: e.to_string(),
                                },
                            )?)))
                        }
                        Some(Node::Iri(iri)) if iri == XSD_INTEGER => {
                            Ok(Node::Literal(Literal::Integer(value.parse().map_err(
                                |e: ParseIntError| TurtleDocError {
                                    message: e.to_string(),
                                },
                            )?)))
                        }
                        Some(Node::Iri(iri)) if iri == XSD_DECIMAL => {
                            Ok(Node::Literal(Literal::Decimal(value.parse().map_err(
                                |e: ParseFloatError| TurtleDocError {
                                    message: e.to_string(),
                                },
                            )?)))
                        }
                        Some(Node::Iri(iri)) if iri == XSD_DOUBLE => {
                            Ok(Node::Literal(Literal::Double(value.parse().map_err(
                                |e: ParseFloatError| TurtleDocError {
                                    message: e.to_string(),
                                },
                            )?)))
                        }
                        Some(Node::Iri(ref iri)) if iri == XSD_DATE => {
                            let parse_from_str = DateTime::parse_from_str;
                            let parse_from_str_no_tz = NaiveDateTime::parse_from_str;

                            let date = DATE_FORMATS
                                .iter()
                                .find_map(|f| parse_from_str(&value, f).ok())
                                .or_else(|| DateTime::parse_from_rfc3339(&value).ok())
                                .or_else(|| {
                                    DATE_FORMATS
                                        .iter()
                                        .find_map(|f| parse_from_str_no_tz(&value, f).ok())
                                        .and_then(|f| {
                                            f.and_local_timezone(Local::now().timezone())
                                                .map(|f| f.fixed_offset())
                                                .latest()
                                        })
                                });

                            if let Some(date) = date {
                                Ok(Node::Literal(Literal::Date(date)))
                            } else {
                                Ok(Node::Literal(Literal::Quoted {
                                    datatype: datatype.map(Box::new),
                                    lang,
                                    value,
                                }))
                            }
                        }

                        Some(Node::Iri(ref iri)) if iri == XSD_TIME => {
                            let parse_from_str = DateTime::parse_from_str;

                            let date = TIME_FORMATS
                                .iter()
                                .find_map(|f| parse_from_str(&value, f).ok());

                            if let Some(date) = date {
                                Ok(Node::Literal(Literal::Time(date)))
                            } else {
                                Ok(Node::Literal(Literal::Quoted {
                                    datatype: datatype.map(Box::new),
                                    lang,
                                    value,
                                }))
                            }
                        }
                        Some(Node::Iri(ref iri)) if iri == XSD_DATE_TIME => {
                            let parse_from_str = DateTime::parse_from_str;

                            let parse_from_str_no_tz = NaiveDateTime::parse_from_str;
                            let date = DATE_FORMATS
                                .iter()
                                .find_map(|f| parse_from_str(&value, f).ok())
                                .or_else(|| DateTime::parse_from_rfc3339(&value).ok())
                                .or_else(|| {
                                    DATE_FORMATS
                                        .iter()
                                        .find_map(|f| parse_from_str_no_tz(&value, f).ok())
                                        .and_then(|f| {
                                            f.and_local_timezone(Local::now().timezone())
                                                .map(|f| f.fixed_offset())
                                                .latest()
                                        })
                                });

                            if let Some(date) = date {
                                Ok(Node::Literal(Literal::DateTime(date)))
                            } else {
                                Ok(Node::Literal(Literal::Quoted {
                                    datatype: datatype.map(Box::new),
                                    lang,
                                    value,
                                }))
                            }
                        }
                        dt => Ok(Node::Literal(Literal::Quoted {
                            datatype: dt.map(Box::new),
                            lang,
                            value,
                        })),
                    }
                }
                ASTLiteral::Boolean(b) => Ok(Node::Literal(Literal::Boolean(b))),
                ASTLiteral::Double(b) => Ok(Node::Literal(Literal::Double(b))),
                ASTLiteral::Decimal(b) => Ok(Node::Literal(Literal::Decimal(b))),
                ASTLiteral::Integer(b) => Ok(Node::Literal(Literal::Integer(b))),
            },
            _ => Err(TurtleDocError {
                message: format!("{s:?} not valid!"),
            }),
        }
    }
    fn get_node<'x>(
        value: TurtleValue<'a>,
        ctx: &'x Context,
        statements: &'x mut Vec<Statement<'a>>,
    ) -> Result<Node<'a>, TurtleDocError> {
        match value {
            v @ TurtleValue::Iri(_) | v @ TurtleValue::Literal(_) => {
                let prefixes: BTreeMap<Cow<str>, Cow<str>> = ctx.prefixes.clone();
                let base = ctx.base.map(Cow::Borrowed);
                Self::simple_turtle_value_to_node(v, base, prefixes, true)
            }
            TurtleValue::BNode(BlankNode::Labeled(label)) => {
                if let Some(well_known_prefix) = ctx.well_known_prefix.as_ref() {
                    Ok(Node::Iri(Cow::Owned(well_known_prefix.to_owned() + label)))
                } else {
                    Ok(Node::LabeledBlankNode(label.into()))
                }
            }
            TurtleValue::BNode(BlankNode::Unlabeled) => {
                let uuid = get_uuid();
                if let Some(well_known_prefix) = ctx.well_known_prefix.as_ref() {
                    Ok(Node::Iri(Cow::Owned(format!("{well_known_prefix}{uuid}"))))
                } else {
                    Ok(Node::LabeledBlankNode(uuid.replace("-", "")))
                }
            }
            TurtleValue::Statement {
                subject,
                predicate_objects,
            } => {
                let subject = {
                    let subject = Self::get_node(*subject, ctx, statements)?;
                    if let Node::Ref(s) = subject {
                        s
                    } else {
                        Arc::new(subject)
                    }
                };
                for predicate_object in predicate_objects {
                    if let TurtleValue::PredicateObject { predicate, object } = predicate_object {
                        let predicate = Self::get_node(*predicate, ctx, statements)?;
                        let object = Self::get_node(*object, ctx, statements)?;
                        match object {
                            Node::List(nodes) => {
                                let predicate = if let Node::Ref(p) = predicate {
                                    p
                                } else {
                                    Arc::new(predicate)
                                };
                                for node in nodes {
                                    let statement = Statement {
                                        subject: Node::Ref(Arc::clone(&subject)),
                                        predicate: Node::Ref(Arc::clone(&predicate)),
                                        object: node,
                                    };
                                    if !statements.contains(&statement) {
                                        statements.push(statement);
                                    }
                                }
                            }
                            node => {
                                let statement = Statement {
                                    subject: Node::Ref(Arc::clone(&subject)),
                                    predicate,
                                    object: node,
                                };
                                if !statements.contains(&statement) {
                                    statements.push(statement);
                                }
                            }
                        }
                    } else {
                        return Err(TurtleDocError {
                            message: "at this point it should be a predicate_object".into(),
                        });
                    }
                }
                Ok(Node::Ref(subject))
            }
            TurtleValue::Collection(mut nodes) => {
                let subject = TurtleValue::BNode(BlankNode::Unlabeled);
                let first = nodes.pop_front().ok_or(TurtleDocError {
                    message: "collection should have at least one element. something went wrong"
                        .into(),
                })?;

                let rest = if nodes.is_empty() {
                    TurtleValue::Iri(Iri::Enclosed(RDF_NIL))
                } else {
                    TurtleValue::Collection(nodes)
                };
                Self::get_node(
                    TurtleValue::Statement {
                        subject: Box::new(subject),
                        predicate_objects: vec![
                            TurtleValue::PredicateObject {
                                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(RDF_FIRST))),
                                object: Box::new(first),
                            },
                            TurtleValue::PredicateObject {
                                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(RDF_REST))),
                                object: Box::new(rest),
                            },
                        ],
                    },
                    ctx,
                    statements,
                )
            }
            TurtleValue::PredicateObject {
                predicate: _,
                object: _,
            } => Err(TurtleDocError {
                message: "PredicateObject: should never happen".into(),
            }),
            TurtleValue::ObjectList(values) => {
                let nodes: Vec<Node<'a>> = values
                    .into_iter()
                    .map(|v| Self::get_node(v, ctx, statements))
                    .collect::<Result<Vec<Node<'a>>, TurtleDocError>>()?;
                Ok(Node::List(nodes))
            }
            _ => Err(TurtleDocError {
                message: "should never happen".into(),
            }),
        }
    }
}

impl<'a> TryFrom<Vec<Statement<'a>>> for TurtleDoc<'a> {
    type Error = TurtleDocError;
    fn try_from(statements: Vec<Statement<'a>>) -> Result<Self, Self::Error> {
        let mut doc = TurtleDoc::new(Vec::with_capacity(statements.len()), None)?;
        doc.statements.extend(statements);
        Ok(doc)
    }
}
impl<'a> TryFrom<&'a Vec<RdfJsonTriple>> for TurtleDoc<'a> {
    type Error = TurtleDocError;
    fn try_from(triples: &'a Vec<RdfJsonTriple>) -> Result<Self, Self::Error> {
        let mut doc = TurtleDoc::new(Vec::with_capacity(triples.len()), None)?;
        for triple in triples {
            let s: Statement = triple.try_into()?;
            doc.statements.push(s);
        }
        Ok(doc)
    }
}
impl<'a> TryFrom<&'a Vec<Statement<'a>>> for TurtleDoc<'a> {
    type Error = TurtleDocError;
    fn try_from(statements: &Vec<Statement<'a>>) -> Result<Self, Self::Error> {
        let mut doc = TurtleDoc::new(Vec::with_capacity(statements.len()), None)?;
        let statements: Vec<Statement> = statements.to_vec();
        doc.statements.extend(statements);
        Ok(doc)
    }
}
impl<'a> TryFrom<Vec<&'a Statement<'a>>> for TurtleDoc<'a> {
    type Error = TurtleDocError;
    fn try_from(statements: Vec<&'a Statement<'a>>) -> Result<Self, Self::Error> {
        let mut doc = TurtleDoc::new(Vec::with_capacity(statements.len()), None)?;
        let statements: Vec<Statement> = statements.into_iter().cloned().collect();
        doc.statements.extend(statements);
        Ok(doc)
    }
}
impl<'a> TryFrom<(&'a str, Option<String>)> for TurtleDoc<'a> {
    type Error = TurtleDocError;

    fn try_from((s, prefix): (&'a str, Option<String>)) -> Result<Self, Self::Error> {
        let (res, statements) = statements(s).map_err(|err| TurtleDocError {
            message: format!("parsing error: {err}"),
        })?;
        let (res, _) = comments(res).map_err(|err| TurtleDocError {
            message: format!("parsing error: {err}"),
        })?;
        if !res.trim().is_empty() {
            return Err(TurtleDocError {
                message: format!("could not parse the doc completely: rest => {res}"),
            });
        }
        Self::new(statements, prefix)
    }
}
impl<'a> TryFrom<&'a RdfJsonTriple> for Statement<'a> {
    type Error = TurtleDocError;

    fn try_from(value: &'a RdfJsonTriple) -> Result<Self, Self::Error> {
        fn rjs_to_node(n: &RdfJsonNode) -> Result<Node<'_>, TurtleDocError> {
            match n.typ.as_str() {
                "uri" => Ok(Node::Iri(Cow::Borrowed(&n.value))),
                "bnode" => Ok(Node::LabeledBlankNode(n.value.to_string())),
                "literal" => match &n.datatype {
                    Some(dt) => match dt.as_str() {
                        XSD_DOUBLE => n
                            .value
                            .parse::<f64>()
                            .map(|f| Node::Literal(Literal::Double(f)))
                            .map_err(|e| TurtleDocError {
                                message: e.to_string(),
                            }),
                        XSD_DECIMAL => n
                            .value
                            .parse::<f32>()
                            .map(|f| Node::Literal(Literal::Decimal(f)))
                            .map_err(|e| TurtleDocError {
                                message: e.to_string(),
                            }),
                        XSD_INTEGER => n
                            .value
                            .parse::<i64>()
                            .map(|f| Node::Literal(Literal::Integer(f)))
                            .map_err(|e| TurtleDocError {
                                message: e.to_string(),
                            }),
                        XSD_BOOLEAN => n
                            .value
                            .parse::<bool>()
                            .map(|f| Node::Literal(Literal::Boolean(f)))
                            .map_err(|e| TurtleDocError {
                                message: e.to_string(),
                            }),
                        _ => Ok(Node::Literal(Literal::Quoted {
                            datatype: Some(Box::new(Node::Iri(Cow::Borrowed(dt)))),
                            value: Cow::Borrowed(&n.value),
                            lang: if let Some(lang) = &n.lang {
                                Some(lang.as_str())
                            } else {
                                None
                            },
                        })),
                    },
                    None => Ok(Node::Literal(Literal::Quoted {
                        datatype: None,
                        value: Cow::Borrowed(&n.value),
                        lang: if let Some(lang) = &n.lang {
                            Some(lang.as_str())
                        } else {
                            None
                        },
                    })),
                },
                t => Err(TurtleDocError {
                    message: format!("type {t} unknown"),
                }),
            }
        }

        fn rnr_to_node(n: &RdfJsonNodeResult) -> Result<Node<'_>, TurtleDocError> {
            match n {
                RdfJsonNodeResult::SingleNode(node) => rjs_to_node(node),
                RdfJsonNodeResult::ListNodes(rnr_nodes) => {
                    let mut nodes = vec![];
                    for node in rnr_nodes.iter() {
                        nodes.push(rnr_to_node(node)?);
                    }
                    Ok(Node::List(nodes))
                }
            }
        }

        let stmt = Statement {
            subject: rnr_to_node(&value.subject)?,
            predicate: rnr_to_node(&value.predicate)?,
            object: rnr_to_node(&value.object)?,
        };
        Ok(stmt)
    }
}

impl From<&TurtleDoc<'_>> for Vec<RdfJsonTriple> {
    fn from(value: &TurtleDoc<'_>) -> Self {
        let statements = &value.statements;
        let mut out: Vec<RdfJsonTriple> = Vec::with_capacity(statements.len());
        for statement in statements {
            let statement = statement.into();
            if !out.contains(&statement) {
                out.push(statement);
            }
        }
        out
    }
}
impl From<&Statement<'_>> for RdfJsonTriple {
    fn from(value: &Statement<'_>) -> Self {
        let subject = &value.subject;
        let predicate = &value.predicate;
        let object = &value.object;
        RdfJsonTriple {
            subject: subject.into(),
            predicate: predicate.into(),
            object: object.into(),
        }
    }
}
impl From<&Node<'_>> for RdfJsonNodeResult {
    fn from(value: &Node<'_>) -> Self {
        let typ_uri = "uri".into();
        let typ_literal = "literal".into();
        let typ_bnode = "bnode".into();
        match value {
            Node::LabeledBlankNode(bnode) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_bnode,
                datatype: None,
                lang: None,
                value: bnode.into(),
            }),
            Node::Iri(iri) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_uri,
                datatype: None,
                lang: None,
                value: iri.to_string(),
            }),
            Node::Literal(Literal::Quoted {
                datatype,
                lang,
                value,
            }) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: if let Some(datatype) = datatype {
                    match datatype.as_ref() {
                        Node::Iri(iri) => Some(iri.to_string()),
                        Node::Ref(iri) => Some(iri.to_string()),
                        other => {
                            eprintln!("this is not a valid datatype {other:?}");
                            None
                        }
                    }
                } else {
                    None
                },
                lang: lang.map(|l| l.to_string()),
                value: value.to_string(),
            }),
            Node::Literal(Literal::Integer(i)) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: Some(XSD_INTEGER.to_string()),
                lang: None,
                value: i.to_string(),
            }),
            Node::Literal(Literal::Decimal(d)) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: Some(XSD_DECIMAL.to_string()),
                lang: None,
                value: d.to_string(),
            }),
            Node::Literal(Literal::Double(d)) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: Some(XSD_DOUBLE.to_string()),
                lang: None,
                value: d.to_string(),
            }),

            Node::Literal(Literal::Boolean(d)) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: Some(XSD_BOOLEAN.to_string()),
                lang: None,
                value: d.to_string(),
            }),
            Node::Literal(Literal::Date(d)) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: Some(XSD_DATE.to_string()),
                lang: None,
                value: d.format("%Y-%m-%d").to_string(),
            }),
            Node::Literal(Literal::DateTime(d)) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: Some(XSD_DATE.to_string()),
                lang: None,
                value: d.format("%+").to_string(),
            }),

            Node::Literal(Literal::Time(d)) => RdfJsonNodeResult::SingleNode(RdfJsonNode {
                typ: typ_literal,
                datatype: Some(XSD_TIME.to_string()),
                lang: None,
                value: d.to_string(),
            }),
            Node::Ref(r) => r.as_ref().into(),
            Node::List(list) => {
                let mut v: Vec<RdfJsonNodeResult> = Vec::with_capacity(list.len());

                for node in list {
                    let node = node.into();
                    if !v.contains(&node) {
                        v.push(node);
                    }
                }

                RdfJsonNodeResult::ListNodes(v)
            }
        }
    }
}

impl<'a> IntoIterator for TurtleDoc<'a> {
    type Item = Statement<'a>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

impl PartialEq for Node<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Node::Iri(n1), Node::Iri(n2)) => n1.eq(n2),
            (Node::Literal(n1), Node::Literal(n2)) => n1 == n2,
            (
                l @ Node::Literal(_)
                | l @ Node::Iri(_)
                | l @ Node::List(_)
                | l @ Node::LabeledBlankNode(_),
                Node::Ref(n2),
            ) => n2.as_ref().eq(l),

            (
                Node::Ref(n1),
                r @ Node::Iri(_)
                | r @ Node::Literal(_)
                | r @ Node::List(_)
                | r @ Node::LabeledBlankNode(_),
            ) => n1.as_ref().eq(r),
            (Node::Ref(n1), Node::Ref(n2)) => n1 == n2,
            (Node::List(n1), Node::List(n2)) => n1 == n2,
            (Node::LabeledBlankNode(n1), Node::LabeledBlankNode(n2)) => n1 == n2,
            (Node::Iri(_), Node::Literal(_))
            | (Node::Iri(_), Node::LabeledBlankNode(_))
            | (Node::Iri(_), Node::List(_))
            | (Node::LabeledBlankNode(_), Node::Iri(_))
            | (Node::LabeledBlankNode(_), Node::Literal(_))
            | (Node::LabeledBlankNode(_), Node::List(_))
            | (Node::Literal(_), Node::Iri(_))
            | (Node::Literal(_), Node::LabeledBlankNode(_))
            | (Node::Literal(_), Node::List(_))
            | (Node::List(_), Node::Iri(_))
            | (Node::List(_), Node::LabeledBlankNode(_))
            | (Node::List(_), Node::Literal(_)) => false,
        }
    }
}

impl Add for TurtleDoc<'_> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut statements: Vec<Statement> = Vec::with_capacity(self.len() + rhs.len());

        for stmt in self.statements.into_iter().chain(rhs.statements) {
            if !statements.contains(&stmt) {
                statements.push(stmt);
            }
        }

        let prefixes: BTreeMap<Cow<'_, str>, Cow<'_, str>> =
            self.prefixes.into_iter().chain(rhs.prefixes).collect();
        TurtleDoc {
            well_known_prefix: self.well_known_prefix,
            base: self.base,
            statements: statements.into_iter().collect(),
            prefixes,
        }
    }
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Iri(iri) => write!(f, "<{iri}>"),
            Node::Ref(iri) => write!(f, "{iri}"),
            Node::Literal(Literal::Quoted {
                datatype,
                lang,
                value,
            }) => {
                let value = value.replace(STRING_LITERAL_LONG_SINGLE_QUOTE, "\'\'\'");
                let separator = if value.ends_with('"') || value.contains(STRING_LITERAL_LONG_QUOTE)
                {
                    STRING_LITERAL_LONG_SINGLE_QUOTE
                } else {
                    STRING_LITERAL_LONG_QUOTE
                };
                let mut s = format!("{separator}{value}{separator}",);
                if let Some(datatype) = datatype {
                    s.push_str(&format!("^^{datatype}"));
                } else if let Some(lang) = lang {
                    s.push_str(&format!("@{lang}"));
                }
                write!(f, "{s}")
            }
            Node::Literal(Literal::Integer(i)) => {
                write!(f, r#""{i}"^^<{XSD_INTEGER}>"#)
            }
            Node::Literal(Literal::Decimal(d)) => {
                write!(f, r#""{d}"^^<{}>"#, XSD_DECIMAL)
            }
            Node::Literal(Literal::Double(d)) => {
                write!(f, r#""{d}"^^<{}>"#, XSD_DOUBLE)
            }
            Node::Literal(Literal::Boolean(d)) => {
                write!(f, r#""{d}"^^<{}>"#, XSD_BOOLEAN)
            }
            Node::Literal(Literal::Date(d)) => {
                write!(f, r#""{}"^^<{}>"#, d.format(DEFAULT_DATE_FORMAT), XSD_DATE)
            }
            Node::Literal(Literal::DateTime(d)) => {
                write!(
                    f,
                    r#""{}"^^<{}>"#,
                    d.to_rfc3339_opts(SecondsFormat::Millis, true),
                    XSD_DATE_TIME
                )
            }
            Node::Literal(Literal::Time(d)) => {
                write!(f, r#""{}"^^<{}>"#, d.format(DEFAULT_TIME_FORMAT), XSD_TIME)
            }
            Node::LabeledBlankNode(bnode) => {
                write!(f, "{BLANK_NODE_LABEL}{bnode}")
            }
            Node::List(list) => {
                panic!("encountered node list where we shouldn't {list:?}");
            }
        }
    }
}
impl Display for Statement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Statement {
            subject,
            predicate,
            object,
        } = self;
        write!(f, r#"{subject} {predicate} {object}."#)
    }
}
impl Display for TurtleDoc<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(Statement::to_string)
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}
impl Display for TurtleDocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "error: {}", self.message)
    }
}

impl TurtleDoc<'_> {
    fn object_to_turtle(
        object: &Node,
        used_prefixes: &mut HashMap<&'static str, &'static str>,
    ) -> String {
        match object {
            Node::Iri(object_str) => {
                PREFIX_OR_NONE(object_str, used_prefixes).unwrap_or(format!("<{object_str}>"))
            }
            Node::Literal(literal) => {
                let object_str = object.to_string();
                match literal {
                    Literal::Quoted { datatype, .. } => {
                        if let Some(datatype) = datatype
                            && datatype.as_ref() == &Node::Iri(Cow::Borrowed(XSD_STRING))
                        {
                            object_str.replace(&format!("^^<{XSD_STRING}>"), "")
                        } else {
                            object_str
                        }
                    }
                    Literal::Double(_) => {
                        object_str.replace(&format!("^^<{XSD_DOUBLE}>"), &XSD_DATATYPE_FN("double"))
                    }
                    Literal::Decimal(_) => object_str
                        .replace(&format!("^^<{XSD_DECIMAL}>"), &XSD_DATATYPE_FN("decimal")),
                    Literal::Integer(v) => {
                        format!("{v}")
                    }
                    Literal::Boolean(b) => {
                        if *b {
                            "true".to_string()
                        } else {
                            "false".to_string()
                        }
                    }
                    Literal::Date(_) => {
                        object_str.replace(&format!("^^<{XSD_DATE}>"), &XSD_DATATYPE_FN("date"))
                    }
                    Literal::DateTime(_) => object_str.replace(
                        &format!("^^<{XSD_DATE_TIME}>"),
                        &XSD_DATATYPE_FN("dateTime"),
                    ),
                    Literal::Time(_) => {
                        object_str.replace(&format!("^^<{XSD_TIME}>"), &XSD_DATATYPE_FN("time"))
                    }
                }
            }

            Node::Ref(r) => Self::object_to_turtle(r.as_ref(), used_prefixes),
            _ => object.to_string(),
        }
    }
    pub fn as_turtle(&self) -> Result<String, TurtleDocError> {
        let mut turtle_map = HashMap::new();
        let mut used_prefixes = HashMap::from([PREFIX_XSD]);

        for Statement {
            subject,
            predicate,
            object,
        } in self.statements.iter()
        {
            let resource = turtle_map
                .entry(subject.to_string())
                .or_insert_with(HashMap::<String, Vec<String>>::new);
            let predicate = {
                let predicate = predicate.get_iri()?;
                PREFIX_OR_NONE(&predicate, &mut used_prefixes)
                    .unwrap_or_else(|| format!("<{predicate}>"))
            };
            let predicate_array = resource.entry(predicate).or_default();
            let object = Self::object_to_turtle(object, &mut used_prefixes);
            predicate_array.push(object.to_string());
        }

        Ok(used_prefixes
            .iter()
            .map(|(k, v)| format!("@prefix {k} <{v}>."))
            .collect::<Vec<_>>()
            .join("\n")
            + "\n\n"
            + &turtle_map
                .into_iter()
                .map(|(subject, predicates)| {
                    format!(
                        "{subject} {}.",
                        predicates
                            .iter()
                            .enumerate()
                            .map(|(idx, (p, o))| format!(
                                "{}{p} {}",
                                if idx == 0 { "" } else { "\t" },
                                o.chunks(2)
                                    .map(|n| n.join(", "))
                                    .collect::<Vec<_>>()
                                    .join(&format!(",\n\t\t{}", if idx == 0 { "" } else { "\t" }))
                            ))
                            .collect::<Vec<_>>()
                            .join(";\n")
                    )
                })
                .collect::<Vec<_>>()
                .join("\n\n"))
    }
}
