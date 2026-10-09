// tests/node.test.mjs
import { describe, test } from "node:test";
import assert from "node:assert/strict";
import {
  TurtleDoc,
  uri,
  literal,
  parseNTriplesStatement,
} from "../pkg-node/tortank_wasm.js";

const TTL = `
@prefix ex: <https://example.com/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:name "Alice" .
`;

const BNODE_TTL = `<https://example.com/a> <https://example.com/knows> [ <https://example.com/name> "Bob" ] .`;

describe("TurtleDoc", () => {
  test("parse", () => {
    const doc = TurtleDoc.parse(TTL);
    assert.equal(doc.length, 1);
    assert.equal(doc.isEmpty(), false);
  });

  test("toJSON / fromJSON round-trip", () => {
    const doc = TurtleDoc.parse(TTL);
    const json = doc.toJSON();

    assert.equal(json.length, 1);
    assert.equal(json[0].subject.value, "https://example.com/alice");

    const copy = TurtleDoc.fromJSON(json);
    assert.equal(copy.length, 1);
    assert.equal(doc.difference(copy).length, 0);
    assert.equal(copy.difference(doc).length, 0);
    assert.equal(doc.intersection(copy).length, 1);
  });

  test("add and duplicate add", () => {
    const doc = TurtleDoc.parse(TTL);
    const s = uri("https://example.com/bob");
    const p = uri("http://xmlns.com/foaf/0.1/name");
    const o = literal("Bob");

    assert.equal(doc.add(s, p, o), true);
    assert.equal(doc.length, 2);

    assert.equal(doc.add(s, p, o), false);
    assert.equal(doc.length, 2);
  });

  test("toTurtle round-trip", () => {
    const doc = TurtleDoc.parse(TTL);
    doc.add(
      uri("https://example.com/bob"),
      uri("http://xmlns.com/foaf/0.1/name"),
      literal("Bob"),
    );

    const turtle = doc.toTurtle();
    assert.equal(typeof turtle, "string");
    assert.ok(turtle.length > 0);
    assert.ok(
      turtle.includes("Alice"),
      `expected serialized Turtle to contain Alice:\n${turtle}`,
    );

    const reparsed = TurtleDoc.parse(turtle);
    assert.equal(reparsed.length, doc.length);
    assert.equal(doc.difference(reparsed).length, 0);
    assert.equal(reparsed.difference(doc).length, 0);
  });

  test("custom uuid fn names blank nodes", () => {
    const doc = TurtleDoc.parse(BNODE_TTL, null, () => "1");

    const expected = TurtleDoc.fromJSON([
      {
        subject: { type: "bnode", value: "1" },
        predicate: { type: "uri", value: "https://example.com/name" },
        object: {
          type: "literal",
          datatype: "http://www.w3.org/2001/XMLSchema#string",
          value: "Bob",
        },
      },
      {
        subject: { type: "uri", value: "https://example.com/a" },
        predicate: { type: "uri", value: "https://example.com/knows" },
        object: { type: "bnode", value: "1" },
      },
    ]);

    assert.equal(doc.length, expected.length);
    assert.equal(doc.difference(expected).length, 0);
    assert.equal(expected.difference(doc).length, 0);
  });

  test("uuid fn returning a non-string throws", () => {
    assert.throws(() => TurtleDoc.parse(BNODE_TTL, null, () => 1));
  });

  test("uuid fn that throws propagates its error", () => {
    assert.throws(
      () =>
        TurtleDoc.parse(BNODE_TTL, null, () => {
          throw new Error("boom");
        }),
      /boom/,
    );
  });

  test("invalid turtle throws", () => {
    assert.throws(() => TurtleDoc.parse("this definitely isn't turtle {"));
  });
});

describe("parseNTriplesStatement", () => {
  test("parses a single statement", () => {
    const parsed = parseNTriplesStatement(
      '<https://example.com/a> <https://example.com/p> "x" .',
    );
    assert.equal(parsed.statement.subject.value, "https://example.com/a");
    assert.equal(parsed.statement.object.value, "x");
  });
});