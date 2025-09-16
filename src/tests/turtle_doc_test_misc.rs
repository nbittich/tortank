use crate::{
    shared::XSD_STRING,
    turtle::turtle_doc::{
        Literal, Node, RdfJsonNode, RdfJsonNodeResult, RdfJsonTriple, Statement, TurtleDoc,
    },
};
use Cow::Borrowed;
use Node::Iri;
use serial_test::serial;
use std::{borrow::Cow, sync::Arc};

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
    assert_eq!(
        statements,
        vec![&Statement {
            subject: Node::Ref(Arc::new(Node::LabeledBlankNode("10".into()))),
            predicate: Node::Iri(Cow::Borrowed("http://foaf.com/mbox")),
            object: Node::Iri(Cow::Borrowed("bob@example.com"))
        }]
    );
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
#[test]
#[serial]
fn test_as_turtle_basic() {
    let r = r#"
<http://mu.semte.ch/streams/ldes-mow-register> a <http://w3id.org/ldes#EventStream>, <https://w3id.org/tree#Collection>;
    <https://w3id.org/tree#view> <./1>;
    <http://w3id.org/ldes#timestampPath> <http://www.w3.org/ns/prov#generatedAtTime>;
    <http://w3id.org/ldes#versionOfPath> <http://purl.org/dc/terms/isVersionOf>.
<./1> a <https://w3id.org/tree#Node>.
<http://mu.semte.ch/streams/ldes-mow-register> <https://w3id.org/tree#member> <http://mu.semte.ch/services/ldes-time-fragmenter/versioned/1>.
<http://mu.semte.ch/services/ldes-time-fragmenter/versioned/1> <http://qudt.org/schema/qudt/hasQuantityKind> <http://qudt.org/vocab/quantitykind/Width>;
    <http://www.w3.org/2000/01/rdf-schema#value> 1;
    <http://qudt.org/schema/qudt/hasUnit> <http://qudt.org/vocab/unit/MilliM>;
    a <http://www.cidoc-crm.org/cidoc-crm/E54_Dimension>;
    <http://mu.semte.ch/vocabularies/core/uuid> "673E2195C7E1A9B9F3B8666A";
    <http://purl.org/dc/terms/isVersionOf> <http://data.lblod.info/dimensions/673E2195C7E1A9B9F3B8666A>;
    <http://www.w3.org/ns/prov#generatedAtTime> "2024-11-22T07:12:35.573Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>.
<http://mu.semte.ch/streams/ldes-mow-register> <https://w3id.org/tree#member> <http://mu.semte.ch/services/ldes-time-fragmenter/versioned/2>.
<http://mu.semte.ch/services/ldes-time-fragmenter/versioned/2> <http://www.cidoc-crm.org/cidoc-crm/P2_has_type> <http://data.lblod.info/concept-schemes/0e0897d1-5c74-47ae-9868-adecbde6f2f3>;
    <http://www.cidoc-crm.org/cidoc-crm/P43_has_dimension> <http://data.lblod.info/dimensions/673E2195C7E1A9B9F3B8666A>;
    a <https://w3id.org/tribont/core#Shape>;
    <http://mu.semte.ch/vocabularies/core/uuid> "673E2195C7E1A9B9F3B8666B";
    <http://purl.org/dc/terms/isVersionOf> <http://data.lblod.info/tribont-shapes/673E2195C7E1A9B9F3B8666B>;
    <http://www.w3.org/ns/prov#generatedAtTime> "2024-11-22T07:12:35.574Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>.
<http://mu.semte.ch/streams/ldes-mow-register> <https://w3id.org/tree#member> <http://mu.semte.ch/services/ldes-time-fragmenter/versioned/3>.
<http://mu.semte.ch/services/ldes-time-fragmenter/versioned/3> <http://purl.org/dc/terms/type> <http://data.vlaanderen.be/id/concept/Verkeersbordcatergorie/2982567006d9e19f04063df73123f56f40e3a28941031a7ba6e6667f64740fa9>;
    a <https://data.vlaanderen.be/ns/mobiliteit#Verkeerstekenconcept>, <https://data.vlaanderen.be/ns/mobiliteit#Verkeersbordconcept>;
    <http://mu.semte.ch/vocabularies/core/uuid> "f9312556b1bdfbb278ec04033417152abbb254466df0069ff1894d0ea7a55482";
    <http://data.lblod.info/vocabularies/mobiliteit/heeftGerelateerdVerkeersbordconcept> <http://data.vlaanderen.be/id/concept/Verkeersbordconcept/9cbedafef411f1c41317f8b9f4066ea6eccfc832edfc930d421725c3ebc5c167>;
    <http://www.w3.org/2004/02/skos/core#prefLabel> "A11";
    <http://www.w3.org/2003/06/sw-vocab-status/ns#termStatus> <http://mow.lblod.info/VerkeersbordconceptStatus/f073030491519fd8e60e230713fb1eee4a2cc49c93e69e39bb00fc9924ae9edd>;
    <https://w3id.org/isCharacterisedBy#isCharacterisedBy> <http://data.lblod.info/tribont-shapes/673E2195C7E1A9B9F3B8666B>;
    <http://www.w3.org/2004/02/skos/core#topConceptOf> <http://data.vlaanderen.be/id/conceptscheme/Verkeersbordconcept>;
    <http://www.w3.org/2004/02/skos/core#scopeNote> "Uitweg op een kaai of een oever.";
    <https://data.vlaanderen.be/ns/mobiliteit#grafischeWeergave> <http://mobiliteit.vo.data.gift/images/46864672-e9e5-435a-9ab9-586ccdbd8149>;
    <https://data.vlaanderen.be/ns/mobiliteit#heeftOnderbordConcept> <http://data.vlaanderen.be/id/concept/Verkeersbordconcept/79d0bab2f07f815d457412b68edab1ba70ee15764d5fcbb531fa87ce24574da6>, <http://data.vlaanderen.be/id/concept/Verkeersbordconcept/061d2edb3caf482931ee7cea26f85d49538a5d4d0781872c0faad20328650beb>;
    <http://www.w3.org/2004/02/skos/core#inScheme> <http://data.vlaanderen.be/id/conceptscheme/Verkeersbordconcept>;
    <http://mu.semte.ch/vocabularies/ext/zonality> <http://lblod.data.gift/concepts/b651931b-923c-477c-8da9-fc7dd841fdcc>;
    <http://purl.org/dc/terms/isVersionOf> <http://data.vlaanderen.be/id/concept/Verkeersbordconcept/f9312556b1bdfbb278ec04033417152abbb254466df0069ff1894d0ea7a55482>;
    <http://www.w3.org/ns/prov#generatedAtTime> "2024-11-22T07:12:35.575Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>.
    "#;
    let doc = TurtleDoc::try_from((r, None as Option<String>)).unwrap();
    let turtle = doc.as_turtle().unwrap();
    println!("FIXME: make it a proper test!\n{turtle}")
}
