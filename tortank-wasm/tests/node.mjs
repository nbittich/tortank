// tests/node.mjs
import assert from "node:assert/strict";
import {
  TurtleDoc,
  uri,
  literal,
  parseNTriplesStatement,
} from "../pkg-node/tortank_wasm.js";

const ttl = `
@prefix ex: <https://example.com/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:name "Alice" .
`;

const doc = TurtleDoc.parse(ttl);

assert.equal(doc.length, 1);
assert.equal(doc.isEmpty(), false);

const json = doc.toJSON();
console.log(json);
assert.equal(json.length, 1);
assert.equal(json[0].subject.value, "https://example.com/alice");

const copy = TurtleDoc.fromJSON(json);

assert.equal(copy.length, 1);
assert.equal(doc.difference(copy).length, 0);
assert.equal(doc.intersection(copy).length, 1);

const s = uri("https://example.com/bob");
const p = uri("http://xmlns.com/foaf/0.1/name");
const o = literal("Bob");

assert.equal(doc.add(s, p, o), true);
assert.equal(doc.length, 2);

// duplicate
assert.equal(doc.add(s, p, o), false);
assert.equal(doc.length, 2);

const parsed = parseNTriplesStatement(
  '<https://example.com/a> <https://example.com/p> "x" .',
);

assert.equal(parsed.statement.subject.value, "https://example.com/a");

console.log("node wasm tests passed");
