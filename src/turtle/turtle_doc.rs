use crate::shared::{
    DEFAULT_WELL_KNOWN_PREFIX, RDF_FIRST, RDF_NIL, RDF_REST, XSD_BOOLEAN, XSD_DECIMAL, XSD_DOUBLE,
    XSD_INTEGER,
};
use crate::triple_common_parser::Literal as ASTLiteral;
use crate::triple_common_parser::{BlankNode, Iri};
use crate::turtle::turtle_parser::{
    object as parse_obj, predicate as parse_pred, statements, subject as parse_sub, TurtleValue,
};
use serde_derive::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::fs::{File, OpenOptions};
use std::io::Read;
use std::io::{prelude::*, BufReader};
use std::num::{ParseFloatError, ParseIntError};
use std::ops::Add;
use std::path::PathBuf;
use std::str::ParseBoolError;
use std::sync::Arc;
use uuid::Uuid;

struct Context<'a> {
    base: Option<&'a str>,
    prefixes: BTreeMap<&'a str, &'a str>,
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
}
#[derive(Debug, PartialOrd, Clone)]
pub enum Node<'a> {
    Iri(Cow<'a, str>),
    Literal(Literal<'a>),
    Ref(Arc<Node<'a>>),
    List(Vec<Node<'a>>),
}
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub struct Statement<'a> {
    pub subject: Node<'a>,
    pub predicate: Node<'a>,
    pub object: Node<'a>,
}
#[derive(PartialEq, PartialOrd, Debug, Default)]
pub struct TurtleDoc<'a> {
    base: Option<&'a str>,
    prefixes: BTreeMap<Cow<'a, str>, Cow<'a, str>>,
    statements: Vec<Statement<'a>>,
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

impl<'a> PartialEq for Node<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Node::Iri(n1), Node::Iri(n2)) => n1.eq(n2),
            (Node::Literal(n1), Node::Literal(n2)) => n1 == n2,
            (l @ Node::Literal(_) | l @ Node::Iri(_) | l @ Node::List(_), Node::Ref(n2)) => {
                n2.as_ref().eq(l)
            }

            (Node::Ref(n1), r @ Node::Iri(_) | r @ Node::Literal(_) | r @ Node::List(_)) => {
                n1.as_ref().eq(r)
            }
            (Node::Ref(n1), Node::Ref(n2)) => n1 == n2,
            (Node::List(n1), Node::List(n2)) => n1 == n2,
            (Node::Iri(_), Node::Literal(_))
            | (Node::Iri(_), Node::List(_))
            | (Node::Literal(_), Node::Iri(_))
            | (Node::Literal(_), Node::List(_))
            | (Node::List(_), Node::Iri(_))
            | (Node::List(_), Node::Literal(_)) => false,
        }
    }
}

impl<'a> TryFrom<Vec<Statement<'a>>> for TurtleDoc<'a> {
    type Error = TurtleDocError;
    fn try_from(statements: Vec<Statement<'a>>) -> Result<Self, Self::Error> {
        let mut doc = TurtleDoc::new(Vec::with_capacity(statements.len()))?;
        doc.statements.extend(statements);
        Ok(doc)
    }
}
impl<'a> TryFrom<&'a str> for TurtleDoc<'a> {
    type Error = TurtleDocError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let (_, statements) = statements(s).map_err(|err| TurtleDocError {
            message: format!("parsing error: {err}"),
        })?;
        Self::new(statements)
    }
}

impl<'a> TurtleDoc<'a> {
    pub fn from_file(
        path: impl Into<PathBuf>,
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
        buf.as_str().try_into()
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
    pub fn add_prefixes(&mut self, prefixes: BTreeMap<String, String>) {
        let prefixes: BTreeMap<Cow<str>, Cow<str>> = prefixes
            .into_iter()
            .map(|(k, v)| (Cow::Owned(k), Cow::Owned(v)))
            .collect();
        self.prefixes.extend(prefixes);
    }
    pub fn len(&self) -> usize {
        self.statements.len()
    }
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }

    /// creates a new, indepependent, turtle_doc containing all the statements in this turtle_doc
    /// that are not in another.
    pub fn difference(&self, other: &TurtleDoc) -> Result<TurtleDoc, TurtleDocError> {
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
    pub fn intersection(&self, other: &TurtleDoc) -> Result<TurtleDoc, TurtleDocError> {
        let intersection = self
            .statements
            .iter()
            .filter(|stmt| other.statements.iter().any(|stmt2| stmt == &stmt2))
            .cloned()
            .collect::<Vec<_>>();

        intersection.try_into()
    }

    pub fn parse_and_list_statements(
        &self,
        subject: Option<String>,
        predicate: Option<String>,
        object: Option<String>,
    ) -> Result<Vec<&Statement>, TurtleDocError> {
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
        &self,
        subject: Option<&Node>,
        predicate: Option<&Node>,
        object: Option<&Node>,
    ) -> Vec<&Statement> {
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

    fn new(turtle_values: Vec<TurtleValue<'a>>) -> Result<Self, TurtleDocError> {
        let mut context = Context {
            base: None,
            prefixes: BTreeMap::new(),
        };
        let mut statements: Vec<Statement> = vec![];

        for turtle_value in turtle_values {
            match turtle_value {
                TurtleValue::Base(base) => {
                    context.base = Some(Self::extract_iri(base)?);
                }
                TurtleValue::Prefix((prefix, iri)) => {
                    let iri = TurtleDoc::extract_iri(iri)?;
                    context.prefixes.insert(prefix, iri);
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
        Ok(TurtleDoc {
            base: context.base,
            statements,
            prefixes: context
                .prefixes
                .into_iter()
                .map(|(k, v)| (Cow::Borrowed(k), (Cow::Borrowed(v))))
                .collect(),
        })
    }

    fn extract_iri(value: Iri) -> Result<&str, TurtleDocError> {
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
                if !iri.starts_with("http://") && !iri.starts_with("https://") {
                    if let Some(base) = base {
                        let iri = (*base).to_owned() + iri;
                        return Ok(Node::Iri(Cow::Owned(iri.to_string())));
                    }
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
                let prefixes: BTreeMap<Cow<str>, Cow<str>> = ctx
                    .prefixes
                    .iter()
                    .map(|(k, v)| (Cow::Borrowed(*k), Cow::Borrowed(*v)))
                    .collect();
                let base = ctx.base.map(Cow::Borrowed);
                Self::simple_turtle_value_to_node(v, base, prefixes, true)
            }
            TurtleValue::BNode(BlankNode::Labeled(label)) => Ok(Node::Iri(Cow::Owned(
                DEFAULT_WELL_KNOWN_PREFIX.to_owned() + label,
            ))),
            TurtleValue::BNode(BlankNode::Unlabeled) => {
                let uuid = Uuid::new_v4().to_string();
                Ok(Node::Iri(Cow::Owned(format!(
                    "{DEFAULT_WELL_KNOWN_PREFIX}{uuid}"
                ))))
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
                return Self::get_node(
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
                );
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
impl<'a> IntoIterator for TurtleDoc<'a> {
    type Item = Statement<'a>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
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
            base: self.base,
            statements: statements.into_iter().collect(),
            prefixes,
        }
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
        match value {
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
            Node::Ref(r) => r.as_ref().into(),
            Node::List(list) => {
                // todo
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
impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Iri(iri) => f.write_str(&format!("<{}>", iri)),
            Node::Ref(iri) => f.write_str(&format!("{}", iri)),
            Node::Literal(Literal::Quoted {
                datatype,
                lang,
                value,
            }) => {
                let mut s = format!(r#""{value}""#);
                if let Some(datatype) = datatype {
                    s.push_str(&format!(r#"^^{datatype}"#));
                } else if let Some(lang) = lang {
                    s.push_str(&format!(r#"@{lang}"#));
                }
                f.write_str(&s)
            }
            Node::Literal(Literal::Integer(i)) => {
                f.write_str(&format!(r#""{i}"^^<{}>"#, XSD_INTEGER))
            }
            Node::Literal(Literal::Decimal(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_DECIMAL))
            }
            Node::Literal(Literal::Double(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_DOUBLE))
            }
            Node::Literal(Literal::Boolean(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_BOOLEAN))
            }
            _ => Err(std::fmt::Error),
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
        f.write_str(&format!(r#"{subject} {predicate} {object}."#))
    }
}

impl Display for TurtleDoc<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &self
                .statements
                .iter()
                .map(Statement::to_string)
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}

#[derive(PartialEq, Debug)]
pub struct TurtleDocError {
    pub message: String,
}
impl Display for TurtleDocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("error: {}", self.message))
    }
}

impl<'a> TryFrom<&'a RdfJsonTriple> for Statement<'a> {
    type Error = TurtleDocError;

    fn try_from(value: &'a RdfJsonTriple) -> Result<Self, Self::Error> {
        fn rjs_to_node(n: &RdfJsonNode) -> Result<Node<'_>, TurtleDocError> {
            match n.typ.as_str() {
                "uri" => return Ok(Node::Iri(Cow::Borrowed(&n.value))),
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
                        _ => {
                            return Ok(Node::Literal(Literal::Quoted {
                                datatype: Some(Box::new(Node::Iri(Cow::Borrowed(dt)))),
                                value: Cow::Borrowed(&n.value),
                                lang: if let Some(lang) = &n.lang {
                                    Some(lang.as_str())
                                } else {
                                    None
                                },
                            }))
                        }
                    },
                    None => {
                        return Ok(Node::Literal(Literal::Quoted {
                            datatype: None,
                            value: Cow::Borrowed(&n.value),
                            lang: if let Some(lang) = &n.lang {
                                Some(lang.as_str())
                            } else {
                                None
                            },
                        }))
                    }
                },
                t => Err(TurtleDocError {
                    message: format!("type {t} unknown"),
                }),
            }
        }

        fn rnr_to_node(n: &RdfJsonNodeResult) -> Result<Node<'_>, TurtleDocError> {
            match n {
                RdfJsonNodeResult::SingleNode(node) => return rjs_to_node(node),
                RdfJsonNodeResult::ListNodes(rnr_nodes) => {
                    let mut nodes = vec![];
                    for node in rnr_nodes.iter() {
                        nodes.push(rnr_to_node(node)?);
                    }
                    return Ok(Node::List(nodes));
                }
            };
        }

        let stmt = Statement {
            subject: rnr_to_node(&value.subject)?,
            predicate: rnr_to_node(&value.predicate)?,
            object: rnr_to_node(&value.object)?,
        };
        Ok(stmt)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        shared::XSD_STRING,
        turtle::turtle_doc::{Literal, Node, RdfJsonTriple, Statement, TurtleDoc},
    };
    use std::borrow::Cow;
    use Cow::Borrowed;
    use Node::Iri;

    use super::RdfJsonNodeResult;

    #[test]
    fn turtle_doc_test() {
        let doc = include_str!("example/input.ttl");
        let expected = include_str!("example/output.ttl");
        let turtle: TurtleDoc<'_> = doc.try_into().unwrap();
        let expected_turtle: TurtleDoc<'_> = expected.try_into().unwrap();
        let expected_statements = expected_turtle.list_statements(None, None, None);
        let statements = turtle.list_statements(None, None, None);
        assert_eq!(&expected_statements.len(), &statements.len());

        assert_eq!(expected_turtle.to_string(), turtle.to_string());
    }
    #[test]
    fn turtle_doc_bnode_test() {
        let doc = r#"
        @prefix foaf: <http://foaf.com/>.
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

        "#;

        let turtle: TurtleDoc<'_> = doc.try_into().unwrap();

        assert_eq!(8, turtle.statements.len());
    }

    #[test]
    fn turtle_doc_collection_test() {
        let s = r#"
        @prefix : <http://example.com/>.
        :a :b ( "apple" "banana" ) .
        "#;
        let turtle: TurtleDoc = s.try_into().unwrap();
        assert_eq!(5, turtle.statements.len());
    }
    #[test]
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
        let turtle1: TurtleDoc = doc1.try_into().unwrap();
        assert_eq!(5, turtle1.statements.len());

        let turtle2: TurtleDoc = doc2.try_into().unwrap();
        assert_eq!(8, turtle2.statements.len());

        let turtle3 = turtle1 + turtle2;
        assert_eq!(13, turtle3.statements.len());
        let mut turtle = TurtleDoc::default();
        turtle.add_statement(
            Iri(Borrowed("http://xxx.com/123")),
            Iri(Borrowed("http://bar.com/345")),
            Node::Literal(Literal::Decimal(123f32)),
        );
        let turtle4 = turtle + turtle3;
        assert_eq!(14, turtle4.statements.len());
    }
    #[test]
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
        let turtle: TurtleDoc = doc.try_into().unwrap();
        let statements =
            turtle.list_statements(None, None, Some(&Iri(Borrowed("bob@example.com"))));
        assert_eq!(1, statements.len());
        println!("{statements:?}");
        let statement = statements[0];
        let statements = turtle.list_statements(Some(&statement.subject), None, None);
        assert_eq!(5, statements.len());
    }

    #[test]
    fn parse_test() {
        let triple = r#"
        # this is a comment
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>. # this is a comment at EOF
             <http://bittich.be/some/url/123><http://example.org/firstName2><http://n.com/nordine>.
             <http://example.org/show/218> <http://www.w3.org/2000/01/rdf-schema#label> "That Seventies Show".
             <http://example.org/show/218> <http://example.org/show/localName> "That Seventies Show"@en .
         <http://bittich.be/some/url/1233>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/1243>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/1253>    <http://example.org/firstName><http://n.com/nordine>  .
         #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .


            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            _:alice <http://xmlns.com/foaf/0.1/knows> _:bob .
            _:bob <http://xmlns.com/foaf/0.1/knows> _:alice .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordineB>  .
         <http://example.org/show/218> <http://example.org/show/localName> "Cette Série des Années Septante"@fr-be .

         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         "#;

        let triples: TurtleDoc = triple.try_into().unwrap();
        assert_eq!(triples.len(), 12);
    }
    #[test]
    fn test_multi_comments() {
        let triples = r#"
            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            _:alice <http://xmlns.com/foaf/0.1/knows> _:bob .
            _:bob <http://xmlns.com/foaf/0.1/knows> _:alice .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://example.org/show/218> <http://example.org/show/localName> "Cette Série des Années Septante"@fr-be .

         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity2> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#decimal> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1123"^^<http://www.w3.org/2001/XMLSchema#integer> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/nice> "true"^^<http://www.w3.org/2001/XMLSchema#boolean> .
         "#;

        let triples: TurtleDoc = triples.try_into().unwrap();

        assert_eq!(9, triples.len());
    }

    #[test]
    fn turtle_doc_diff_test() {
        let mut buf_a = String::new();
        let mut buf_b = String::new();
        let _doc = include_str!("example/input.ttl");
        let expected:TurtleDoc= r#"
        <mailto:person@example.net> <http://xmlns.com/foaf/0.1/name> "Anne Example-Person"^^<http://www.w3.org/2001/XMLSchema#string>
        "#.try_into().unwrap();
        let turtle_a = TurtleDoc::from_file("tests/modelA.ttl", &mut buf_a).unwrap();
        let turtle_b = TurtleDoc::from_file("tests/modelB.ttl", &mut buf_b).unwrap();
        let diff = turtle_a.difference(&turtle_b).unwrap();

        dbg!(&expected);
        assert_eq!(diff, expected);
    }
    #[test]
    fn turtle_doc_to_json_test() {
        let doc = r#"
                    @prefix foaf: <http://foaf.com/>.
                    [ foaf:name "Alice" ] foaf:knows [
                foaf:name "Bob" ;
                foaf:description "\"c'est l'histoire de la vie\"\n"@fr;
                foaf:lastName "George", "Joshua" ;
                foaf:age 34;
                foaf:knows [
                    foaf:name "Eve" ] ;
                foaf:mbox <bob@example.com>] .

        "#;
        let turtle: TurtleDoc = doc.try_into().unwrap();
        let json_triples: Vec<RdfJsonTriple> = (&turtle).into();
        assert_eq!(json_triples.len(), turtle.len());
        println!("{}", serde_json::to_string_pretty(&json_triples).unwrap());
    }
    #[test]
    fn test_convert_rdf_triple_to_doc() {
        let triple = RdfJsonTriple {
            subject: RdfJsonNodeResult::SingleNode(super::RdfJsonNode {
                typ: "uri".into(),
                datatype: None,
                lang: None,
                value: "http://xx.com/xxx".into(),
            }),
            predicate: RdfJsonNodeResult::SingleNode(super::RdfJsonNode {
                typ: "uri".into(),
                datatype: None,
                lang: None,
                value: "http://xx.com/pred".into(),
            }),
            object: RdfJsonNodeResult::SingleNode(super::RdfJsonNode {
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

    #[test]
    fn turtle_doc_to_json_bug_test() {
        let doc = r#"
                    @prefix foaf: <http://foaf.com/>.
                    [ foaf:name "Alice" ] foaf:knows [
                foaf:name "Bob" ;
                foaf:description "\"c'est l'histoire de la vie\"\n"@fr;
                foaf:lastName "George", "Joshua" ;
                foaf:age 34;
                foaf:knows [
                    foaf:name "Eve" ] ;
                foaf:mbox <bob@example.com>] .

        "#;
        let turtle: TurtleDoc = doc.try_into().unwrap();
        let stmts = turtle.list_statements(None, None, None);
        let rdfjs: RdfJsonTriple = stmts[0].try_into().unwrap();

        dbg!(rdfjs);
    }
}
