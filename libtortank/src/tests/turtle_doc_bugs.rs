use crate::turtle::turtle_doc::TurtleDoc;

#[test]
fn test_well_known_prefixed() {
    let ttl = r#"
    @prefix ex: <http://example.org/>.
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.

<http://example.org/.well-known/01a10b3c96db765bb6d67715d40d7997> foaf:mailbox <mailto:ivan@w3.org>;
        foaf:name """Ivan Herman""".

<http://www.example.org/#somebody> foaf:knows <http://danbri.org/foaf.rdf#danbri>, <http://example.org/.well-known/01a10b3c96db765bb6d67715d40d7997>.

<http://danbri.org/foaf.rdf#danbri> a foaf:Person;
        foaf:name """Dan Brickley""".
    "#;
    let doc: TurtleDoc<'_> = (ttl, None, None).try_into().unwrap();

    assert_eq!(doc.len(), 6);
    let turtle = doc.as_turtle().unwrap();
    let expected: TurtleDoc<'_> = (turtle.as_str(), None, None).try_into().unwrap();
    assert_eq!(doc.difference(&expected).unwrap().len(), 0);
}
