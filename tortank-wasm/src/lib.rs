#![cfg(target_arch = "wasm32")]

use lol_alloc::{AssumeSingleThreaded, FreeListAllocator};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

// Adjust this import if tortank is the current crate rather than a dependency.
use tortank::turtle::turtle_doc::{
    RdfJsonNode,
    RdfJsonNodeResult,
    RdfJsonTriple,
    TurtleDoc as NativeTurtleDoc,
};

// SAFETY: wasm32 without threads is single threaded.
#[global_allocator]
static ALLOCATOR: AssumeSingleThreaded<FreeListAllocator> =
    unsafe { AssumeSingleThreaded::new(FreeListAllocator::new()) };

fn js_err<E: std::fmt::Display>(err: E) -> JsValue {
    js_sys::Error::new(&err.to_string()).into()
}

fn to_js<T: Serialize>(value: &T) -> Result<JsValue, JsValue> {
    serde_wasm_bindgen::to_value(value).map_err(js_err)
}

fn from_js<T>(value: JsValue) -> Result<T, JsValue>
where
    T: for<'de> Deserialize<'de>,
{
    serde_wasm_bindgen::from_value(value).map_err(js_err)
}

/// Owned WASM representation.
///
/// We deliberately store RDF-JSON triples rather than NativeTurtleDoc<'a>,
/// because NativeTurtleDoc borrows its source input.
#[wasm_bindgen(js_name = TurtleDoc)]
pub struct WasmTurtleDoc {
    triples: Vec<RdfJsonTriple>,
}

impl WasmTurtleDoc {
     fn from_native(doc: &NativeTurtleDoc<'_>) -> Self {
        Self {
            triples: Vec::<RdfJsonTriple>::from(doc),
        }
    }

    fn with_native<R>(
        &self,
        f: impl FnOnce(&NativeTurtleDoc<'_>) -> Result<R, JsValue>,
    ) -> Result<R, JsValue> {
        let doc = NativeTurtleDoc::try_from(&self.triples)
            .map_err(js_err)?;
        f(&doc)
    }

    fn node_eq(a: &RdfJsonNodeResult, b: &RdfJsonNodeResult) -> bool {
        a == b
    }}

#[wasm_bindgen(js_class = TurtleDoc)]
impl WasmTurtleDoc {
    /// Parse a Turtle/N3 document.
    ///
    /// JS:
    ///   const doc = TurtleDoc.parse(ttl);
    #[wasm_bindgen(js_name = parse)]
    pub fn parse(
        input: &str,
        well_known_prefix: Option<String>,
    ) -> Result<WasmTurtleDoc, JsValue> {
        let doc = NativeTurtleDoc::try_from((
            input,
            well_known_prefix,
        ))
        .map_err(js_err)?;

        Ok(Self::from_native(&doc))
    }
    #[wasm_bindgen(js_name = toTurtle)]
    pub fn to_turtle(&self) -> Result<String, JsValue> {
        self.with_native(|doc| {
            doc.as_turtle().map_err(js_err)
        })
    }

    /// Construct from the RDF-JSON representation returned by `toJSON()`.
    #[wasm_bindgen(js_name = fromJSON)]
    pub fn from_json(value: JsValue) -> Result<WasmTurtleDoc, JsValue> {
        let triples: Vec<RdfJsonTriple> = from_js(value)?;

        // Validate by constructing the native document.
        NativeTurtleDoc::try_from(&triples).map_err(js_err)?;

        Ok(Self { triples })
    }

    /// Construct from an RDF-JSON JSON string.
    #[wasm_bindgen(js_name = fromJSONString)]
    pub fn from_json_string(value: &str) -> Result<WasmTurtleDoc, JsValue> {
        let triples = RdfJsonTriple::from_json(value).map_err(js_err)?;

        NativeTurtleDoc::try_from(&triples).map_err(js_err)?;

        Ok(Self { triples })
    }

    /// Number of statements.
    #[wasm_bindgen(getter)]
    pub fn length(&self) -> usize {
        self.triples.len()
    }

    #[wasm_bindgen(js_name = len)]
    pub fn len(&self) -> usize {
        self.triples.len()
    }

    #[wasm_bindgen(js_name = isEmpty)]
    pub fn is_empty(&self) -> bool {
        self.triples.is_empty()
    }

    /// Return all statements as JS objects.
    ///
    /// [
    ///   {
    ///     subject: { type: "uri", value: "..." },
    ///     predicate: { type: "uri", value: "..." },
    ///     object: {
    ///       type: "literal",
    ///       value: "...",
    ///       datatype: "...",
    ///       lang: "..."
    ///     }
    ///   }
    /// ]
    #[wasm_bindgen(js_name = statements)]
    pub fn statements(&self) -> Result<JsValue, JsValue> {
        to_js(&self.triples)
    }

    /// Alias useful for JSON-oriented JS APIs.
    #[wasm_bindgen(js_name = toJSON)]
    pub fn to_json(&self) -> Result<JsValue, JsValue> {
        to_js(&self.triples)
    }

    #[wasm_bindgen(js_name = toJSONString)]
    pub fn to_json_string(&self) -> Result<String, JsValue> {
        serde_json::to_string(&self.triples).map_err(js_err)
    }

    /// Return the N-Triples/Turtle-style statement serialization.
    ///
    /// This mirrors the native Statement Display implementation.
    #[wasm_bindgen(js_name = toString)]
    pub fn to_string(&self) -> Result<String, JsValue> {
        self.to_turtle()
    } 

    /// Query using Turtle syntax.
    ///
    /// Examples:
    ///
    /// doc.match("ex:alice", null, null)
    /// doc.match(null, "rdf:type", "schema:Person")
    /// doc.match(null, null, "\"hello\"@en")
    ///
    /// Prefix/base resolution is performed by TurtleDoc itself.
    #[wasm_bindgen(js_name = match)]
    pub fn match_statements(
        &self,
        subject: Option<String>,
        predicate: Option<String>,
        object: Option<String>,
    ) -> Result<JsValue, JsValue> {
        // A document reconstructed solely from RDF JSON no longer carries
        // Turtle prefix declarations. Therefore query values should normally
        // use absolute IRIs.
        //
        // For documents coming directly from parse(), see the note below
        // about preserving context.
        self.with_native(|doc| {
            let stmts = doc
                .parse_and_list_statements(subject, predicate, object)
                .map_err(js_err)?;

            let triples: Vec<RdfJsonTriple> = stmts
                .into_iter()
                .map(Into::into)
                .collect();

            to_js(&triples)
        })
    }

    /// Exact RDF-JSON node matching.
    ///
    /// Unlike match(), this doesn't parse Turtle terms.
    #[wasm_bindgen(js_name = matchNodes)]
    pub fn match_nodes(
        &self,
        subject: Option<JsValue>,
        predicate: Option<JsValue>,
        object: Option<JsValue>,
    ) -> Result<JsValue, JsValue> {
        let s: Option<RdfJsonNodeResult> = match subject {
            Some(v) if !v.is_null() && !v.is_undefined() => Some(from_js(v)?),
            _ => None,
        };

        let p: Option<RdfJsonNodeResult> = match predicate {
            Some(v) if !v.is_null() && !v.is_undefined() => Some(from_js(v)?),
            _ => None,
        };

        let o: Option<RdfJsonNodeResult> = match object {
            Some(v) if !v.is_null() && !v.is_undefined() => Some(from_js(v)?),
            _ => None,
        };

        let result: Vec<&RdfJsonTriple> = self
            .triples
            .iter()
            .filter(|t| {
                s.as_ref()
                    .map(|n| Self::node_eq(&t.subject, n))
                    .unwrap_or(true)
                    && p.as_ref()
                        .map(|n| Self::node_eq(&t.predicate, n))
                        .unwrap_or(true)
                    && o.as_ref()
                        .map(|n| Self::node_eq(&t.object, n))
                        .unwrap_or(true)
            })
            .collect();

        to_js(&result)
    }

    /// Add a statement represented using RDF-JSON nodes.
    ///
    /// Duplicate statements are ignored, matching TurtleDoc::add_statement.
    #[wasm_bindgen(js_name = add)]
    pub fn add(
        &mut self,
        subject: JsValue,
        predicate: JsValue,
        object: JsValue,
    ) -> Result<bool, JsValue> {
        let triple = RdfJsonTriple {
            subject: from_js(subject)?,
            predicate: from_js(predicate)?,
            object: from_js(object)?,
        };

        // Validate RDF node types using TurtleDoc's conversion.
        let validation = vec![triple.clone()];
        NativeTurtleDoc::try_from(&validation).map_err(js_err)?;

        if self.triples.contains(&triple) {
            return Ok(false);
        }

        self.triples.push(triple);
        Ok(true)
    }

    /// Add a complete RDF-JSON triple object.
    #[wasm_bindgen(js_name = addTriple)]
    pub fn add_triple(&mut self, triple: JsValue) -> Result<bool, JsValue> {
        let triple: RdfJsonTriple = from_js(triple)?;

        let validation = vec![triple.clone()];
        NativeTurtleDoc::try_from(&validation).map_err(js_err)?;

        if self.triples.contains(&triple) {
            return Ok(false);
        }

        self.triples.push(triple);
        Ok(true)
    }

    /// Remove matching triples.
    ///
    /// null/undefined means wildcard.
    ///
    /// Returns number removed.
    #[wasm_bindgen(js_name = remove)]
    pub fn remove(
        &mut self,
        subject: Option<JsValue>,
        predicate: Option<JsValue>,
        object: Option<JsValue>,
    ) -> Result<usize, JsValue> {
        let s: Option<RdfJsonNodeResult> = match subject {
            Some(v) if !v.is_null() && !v.is_undefined() => Some(from_js(v)?),
            _ => None,
        };

        let p: Option<RdfJsonNodeResult> = match predicate {
            Some(v) if !v.is_null() && !v.is_undefined() => Some(from_js(v)?),
            _ => None,
        };

        let o: Option<RdfJsonNodeResult> = match object {
            Some(v) if !v.is_null() && !v.is_undefined() => Some(from_js(v)?),
            _ => None,
        };

        let before = self.triples.len();

        self.triples.retain(|t| {
            let matches =
                s.as_ref()
                    .map(|n| &t.subject == n)
                    .unwrap_or(true)
                && p.as_ref()
                    .map(|n| &t.predicate == n)
                    .unwrap_or(true)
                && o.as_ref()
                    .map(|n| &t.object == n)
                    .unwrap_or(true);

            !matches
        });

        Ok(before - self.triples.len())
    }

    #[wasm_bindgen(js_name = clear)]
    pub fn clear(&mut self) {
        self.triples.clear();
    }

    #[wasm_bindgen(js_name = contains)]
    pub fn contains(&self, triple: JsValue) -> Result<bool, JsValue> {
        let triple: RdfJsonTriple = from_js(triple)?;
        Ok(self.triples.contains(&triple))
    }

    /// TurtleDoc::difference
    #[wasm_bindgen(js_name = difference)]
    pub fn difference(&self, other: &WasmTurtleDoc) -> WasmTurtleDoc {
        let triples = self
            .triples
            .iter()
            .filter(|t| !other.triples.contains(t))
            .cloned()
            .collect();

        Self { triples }
    }

    /// TurtleDoc::intersection
    #[wasm_bindgen(js_name = intersection)]
    pub fn intersection(&self, other: &WasmTurtleDoc) -> WasmTurtleDoc {
        let triples = self
            .triples
            .iter()
            .filter(|t| other.triples.contains(t))
            .cloned()
            .collect();

        Self { triples }
    }

    /// Return unique subjects.
    #[wasm_bindgen(js_name = allSubjects)]
    pub fn all_subjects(&self) -> Result<JsValue, JsValue> {
        let mut subjects: Vec<RdfJsonNodeResult> = Vec::new();

        for triple in &self.triples {
            if !subjects.contains(&triple.subject) {
                subjects.push(triple.subject.clone());
            }
        }

        to_js(&subjects)
    }

    #[wasm_bindgen(js_name = clone)]
    pub fn clone_doc(&self) -> WasmTurtleDoc {
        Self {
            triples: self.triples.clone(),
        }
    }
}

/// Parse a single N-Triples statement.
///
/// Returns:
///
/// {
///   rest: "...",
///   statement: { subject, predicate, object }
/// }
///
/// or null for empty input.
#[wasm_bindgen(js_name = parseNTriplesStatement)]
pub fn parse_ntriples_statement(input: &str) -> Result<JsValue, JsValue> {
    let parsed =
        NativeTurtleDoc::parse_ntriples_statement(input).map_err(js_err)?;

    let Some((rest, statement)) = parsed else {
        return Ok(JsValue::NULL);
    };

    let triple: RdfJsonTriple = (&statement).into();

    #[derive(Serialize)]
    struct ResultValue<'a> {
        rest: &'a str,
        statement: RdfJsonTriple,
    }

    to_js(&ResultValue {
        rest,
        statement: triple,
    })
}

// --------------------------------------------------------------------------
// JS-friendly RDF node constructors
// --------------------------------------------------------------------------

#[wasm_bindgen(js_name = uri)]
pub fn uri(value: &str) -> Result<JsValue, JsValue> {
    to_js(&RdfJsonNodeResult::SingleNode(RdfJsonNode {
        typ: "uri".into(),
        datatype: None,
        lang: None,
        value: value.into(),
    }))
}

#[wasm_bindgen(js_name = blankNode)]
pub fn blank_node(value: &str) -> Result<JsValue, JsValue> {
    to_js(&RdfJsonNodeResult::SingleNode(RdfJsonNode {
        typ: "bnode".into(),
        datatype: None,
        lang: None,
        value: value.into(),
    }))
}

#[wasm_bindgen(js_name = literal)]
pub fn literal(
    value: &str,
    datatype: Option<String>,
    lang: Option<String>,
) -> Result<JsValue, JsValue> {
    to_js(&RdfJsonNodeResult::SingleNode(RdfJsonNode {
        typ: "literal".into(),
        datatype,
        lang,
        value: value.into(),
    }))
}
