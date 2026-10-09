import { test, describe } from 'node:test';
import assert from 'node:assert/strict';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const { DataFactory, Dataset, parseNTriplesQuad, TurtleDoc } = require('../pkg/tortank_wasm.js');

const EX = 'http://example.org/';
const XSD = 'http://www.w3.org/2001/XMLSchema#';
const df = new DataFactory();
const n = (s) => df.namedNode(EX + s);
const lit = (s, x) => df.literal(s, x);
const q = (s, p, o) => df.quad(n(s), n(p), o);
const ds = (...quads) => new Dataset(quads);

describe('DataFactory', () => {
  test('namedNode', () => {
    const t = n('a');
    assert.equal(t.termType, 'NamedNode');
    assert.equal(t.value, EX + 'a');
    assert.ok(t.equals(n('a')));
    assert.ok(!t.equals(n('b')));
    assert.ok(!t.equals(null));
  });

  test('blankNode', () => {
    assert.equal(df.blankNode('x').value, 'x');
    assert.notEqual(df.blankNode().value, df.blankNode().value);
    assert.equal(df.blankNode().termType, 'BlankNode');
  });

  test('literal variants', () => {
    const plain = lit('hi');
    assert.equal(plain.language, '');
    assert.equal(plain.datatype.value, XSD + 'string');

    const tagged = lit('hi', 'en');
    assert.equal(tagged.language, 'en');
    assert.equal(
      tagged.datatype.value,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',
    );

    const typed = lit('1', df.namedNode(XSD + 'integer'));
    assert.equal(typed.datatype.value, XSD + 'integer');
    assert.ok(typed.equals(lit('1', df.namedNode(XSD + 'integer'))));
    assert.ok(!typed.equals(lit('1')));
    assert.ok(!tagged.equals(lit('hi', 'fr')));
  });

  test('defaultGraph', () => {
    const g = df.defaultGraph();
    assert.equal(g.termType, 'DefaultGraph');
    assert.equal(g.value, '');
    assert.ok(g.equals(df.defaultGraph()));
  });

  test('quad / triple', () => {
    const a = q('s', 'p', lit('o'));
    assert.equal(a.termType, 'Quad');
    assert.equal(a.graph.termType, 'DefaultGraph');
    assert.ok(a.equals(q('s', 'p', lit('o'))));
    assert.ok(!a.equals(q('s', 'p', lit('x'))));
    assert.ok(a.equals(df.triple(n('s'), n('p'), lit('o'))));
  });
});

describe('Dataset (DatasetCore)', () => {
  test('add / has / size / dedup', () => {
    const d = new Dataset();
    assert.equal(d.size, 0);
    d.add(q('s', 'p', lit('o'))).add(q('s', 'p', lit('o')));
    assert.equal(d.size, 1);
    assert.ok(d.has(q('s', 'p', lit('o'))));
    assert.ok(!d.has(q('s', 'p', lit('nope'))));
  });

  test('mutators return this', () => {
    const d = new Dataset();
    assert.equal(d.add(q('s', 'p', lit('o'))), d);
    assert.equal(d.delete(q('s', 'p', lit('o'))), d);
    assert.equal(d.addAll([]), d);
    assert.equal(d.deleteMatches(), d);
  });

  test('delete', () => {
    const d = ds(q('s', 'p', lit('1')), q('s', 'p', lit('2')));
    d.delete(q('s', 'p', lit('1')));
    assert.equal(d.size, 1);
    assert.ok(d.has(q('s', 'p', lit('2'))));
  });

  test('iterator yields RDF/JS quads', () => {
    const d = ds(q('a', 'p', lit('1')), q('b', 'p', lit('2')));
    const out = [...d];
    assert.equal(out.length, 2);
    for (const quad of out) {
      assert.equal(quad.termType, 'Quad');
      assert.equal(typeof quad.equals, 'function');
    }
    assert.ok(out.some((x) => x.equals(q('a', 'p', lit('1')))));
  });

  test('match with wildcards', () => {
    const d = ds(q('a', 'p', lit('1')), q('a', 'q', lit('2')), q('b', 'p', lit('3')));
    assert.equal(d.match(n('a')).size, 2);
    assert.equal(d.match(null, n('p')).size, 2);
    assert.equal(d.match(undefined, n('p'), undefined).size, 2);
    assert.equal(d.match(n('a'), n('p')).size, 1);
    assert.equal(d.match(null, null, lit('3')).size, 1);
    assert.equal(d.match().size, 3);
    assert.equal(d.match(null, null, null, df.defaultGraph()).size, 3);
  });

  test('match treats Variable as wildcard', () => {
    const d = ds(q('a', 'p', lit('1')), q('b', 'p', lit('2')));
    const v = { termType: 'Variable', value: 'x', equals: () => false };
    assert.equal(d.match(v, n('p'), v).size, 2);
  });

  test('match on named graph is empty', () => {
    const d = ds(q('a', 'p', lit('1')));
    assert.equal(d.match(null, null, null, n('g')).size, 0);
  });

  test('match returns an independent Dataset', () => {
    const d = ds(q('a', 'p', lit('1')));
    const m = d.match(n('a'));
    m.add(q('z', 'p', lit('2')));
    assert.equal(d.size, 1);
    assert.ok(m instanceof Dataset);
  });
});

describe('Dataset (extended)', () => {
  test('constructor accepts any iterable', () => {
    const quads = [q('a', 'p', lit('1')), q('b', 'p', lit('2'))];
    assert.equal(new Dataset(quads).size, 2);
    assert.equal(new Dataset(new Set(quads)).size, 2);
    assert.equal(new Dataset(ds(...quads)).size, 2);
    assert.equal(new Dataset(null).size, 0);
  });

  test('addAll rejects non-iterables', () => {
    assert.throws(() => new Dataset().addAll(1));
  });

  test('deleteMatches', () => {
    const d = ds(q('a', 'p', lit('1')), q('a', 'q', lit('2')), q('b', 'p', lit('3')));
    d.deleteMatches(n('a'));
    assert.equal(d.size, 1);
    d.deleteMatches(null, null, null, n('g')); // named graph: no-op
    assert.equal(d.size, 1);
    d.deleteMatches();
    assert.equal(d.size, 0);
  });

  test('toArray', () => {
    const arr = ds(q('a', 'p', lit('1'))).toArray();
    assert.ok(Array.isArray(arr));
    assert.equal(arr.length, 1);
  });

  test('includes / contains / equals', () => {
    const a = ds(q('s', 'p', lit('1')), q('s', 'p', lit('2')));
    const b = ds(q('s', 'p', lit('2')), q('s', 'p', lit('1')));
    const sub = ds(q('s', 'p', lit('1')));
    assert.ok(a.includes(q('s', 'p', lit('1'))));
    assert.ok(a.contains(sub));
    assert.ok(!sub.contains(a));
    assert.ok(a.equals(b));
    assert.ok(!a.equals(sub));
    assert.ok(!a.equals(null));
  });

  test('union / intersection / difference', () => {
    const a = ds(q('s', 'p', lit('1')), q('s', 'p', lit('2')));
    const b = ds(q('s', 'p', lit('2')), q('s', 'p', lit('3')));
    assert.equal(a.union(b).size, 3);
    assert.equal(a.intersection(b).size, 1);
    assert.ok(a.intersection(b).has(q('s', 'p', lit('2'))));
    assert.equal(a.difference(b).size, 1);
    assert.ok(a.difference(b).has(q('s', 'p', lit('1'))));
    assert.equal(a.size, 2); // inputs untouched
  });

  test('forEach / some / every', () => {
    const d = ds(q('a', 'p', lit('1')), q('b', 'p', lit('2')));
    const seen = [];
    d.forEach((quad, set) => {
      assert.equal(set, d);
      seen.push(quad.subject.value);
    });
    assert.deepEqual(seen.sort(), [EX + 'a', EX + 'b']);
    assert.ok(d.some((x) => x.subject.value === EX + 'a'));
    assert.ok(!d.some((x) => x.subject.value === EX + 'zzz'));
    assert.ok(d.every((x) => x.predicate.value === EX + 'p'));
    assert.ok(!d.every((x) => x.subject.value === EX + 'a'));
  });

  test('filter / map return Datasets', () => {
    const d = ds(q('a', 'p', lit('1')), q('b', 'p', lit('2')));
    const f = d.filter((x) => x.subject.value === EX + 'a');
    assert.ok(f instanceof Dataset);
    assert.equal(f.size, 1);
    const m = d.map((x) => df.quad(x.subject, n('renamed'), x.object));
    assert.equal(m.size, 2);
    assert.equal(m.match(null, n('renamed')).size, 2);
  });

  test('reduce', () => {
    const d = ds(q('a', 'p', lit('1')), q('b', 'p', lit('2')));
    assert.equal(d.reduce((acc) => acc + 1, 0), 2);
    assert.ok(typeof d.reduce((acc, x) => acc ?? x) === 'object');
  });
});

describe('term fidelity', () => {
  test('blank nodes round-trip', () => {
    const b = df.blankNode('b1');
    const d = ds(df.quad(b, n('p'), n('o')));
    const [out] = d;
    assert.equal(out.subject.termType, 'BlankNode');
    assert.ok(out.subject.equals(b));
    assert.ok(d.has(df.quad(b, n('p'), n('o'))));
  });

  test('language and datatype round-trip', () => {
    const tagged = lit('hi', 'en');
    const typed = lit('1', df.namedNode(XSD + 'integer'));
    const d = ds(q('s', 'p', tagged), q('s', 'p', typed), q('s', 'p', lit('plain')));
    assert.equal(d.size, 3);
    assert.ok(d.has(q('s', 'p', tagged)));
    assert.ok(d.has(q('s', 'p', typed)));
    assert.ok(!d.has(q('s', 'p', lit('hi'))));
    const objs = [...d].map((x) => x.object);
    assert.ok(objs.some((o) => o.language === 'en'));
    assert.ok(objs.some((o) => o.datatype.value === XSD + 'integer'));
  });

  test('explicit xsd:string equals implicit', () => {
    const d = ds(q('s', 'p', lit('x')));
    assert.ok(d.has(q('s', 'p', lit('x', df.namedNode(XSD + 'string')))));
  });

  test('plain-object terms are accepted', () => {
    const plain = {
      subject: { termType: 'NamedNode', value: EX + 's' },
      predicate: { termType: 'NamedNode', value: EX + 'p' },
      object: { termType: 'Literal', value: 'x', language: '', datatype: { value: XSD + 'string' } },
    };
    const d = new Dataset([plain]);
    assert.ok(d.has(q('s', 'p', lit('x'))));
  });
});

describe('validation', () => {
  test('non-quads throw', () => {
    const d = new Dataset();
    assert.throws(() => d.add('nope'));
    assert.throws(() => d.add(null));
    assert.throws(() => d.add({ subject: n('s') }));
  });

  test('named-graph quads throw', () => {
    assert.throws(
      () => new Dataset().add(df.quad(n('s'), n('p'), lit('o'), n('g'))),
      /named graphs/,
    );
  });

  test('unsupported term types throw', () => {
    const quad = {
      subject: { termType: 'Variable', value: 'x' },
      predicate: n('p'),
      object: lit('o'),
    };
    assert.throws(() => new Dataset().add(quad));
  });
});

describe('Turtle / RDF-JSON extras', () => {
  const ttl = '@prefix ex: <http://example.org/> .\nex:a ex:p "x", "y" .\nex:b ex:p "z" .\n';

  test('parse + size', () => {
    assert.equal(Dataset.parse(ttl).size, 3);
  });

  test('matchTurtle uses document prefixes', () => {
    const d = Dataset.parse(ttl);
    assert.equal(d.matchTurtle('ex:a').size, 2);
    assert.equal(d.matchTurtle(null, 'ex:p', '"z"').size, 1);
  });

  test('parse with custom uuid fn', () => {
    let calls = 0;
    const d = Dataset.parse(
      '@prefix ex: <http://example.org/> .\nex:a ex:p [ ex:q "v" ] .\n',
      undefined,
      () => `id-${calls++}`,
    );
    assert.ok(d.size >= 2);
    assert.ok(calls >= 1);
  });

  test('uuid fn errors propagate', () => {
    assert.throws(
      () =>
        Dataset.parse('@prefix ex: <http://example.org/> .\nex:a ex:p [ ex:q "v" ] .\n', undefined, () => {
          throw new Error('boom');
        }),
      /boom/,
    );
  });

  test('toTurtle / toString round-trip', () => {
    const d = ds(q('a', 'p', lit('x')), q('b', 'p', lit('y')));
    assert.ok(d.difference(Dataset.parse(d.toTurtle())).size==0);
    const back = Dataset.parse(d.toTurtle());
    assert.ok(back.equals(d));
  });

  test('RDF-JSON round-trip', () => {
    const d = ds(q('a', 'p', lit('x')), q('a', 'p', lit('hi', 'en')));
    assert.ok(Dataset.fromRdfJson(d.toRdfJson()).equals(d));
    assert.ok(Dataset.fromRdfJsonString(d.toRdfJsonString()).equals(d));
    assert.ok(Array.isArray(JSON.parse(d.toRdfJsonString())));
  });

  test('clear / clone are independent', () => {
    const d = ds(q('a', 'p', lit('x')));
    const copy = d.clone();
    d.clear();
    assert.equal(d.size, 0);
    assert.equal(copy.size, 1);
  });
});

describe('parseNTriplesQuad', () => {
  test('parses one statement and returns the rest', () => {
    const input =
      '<http://example.org/a> <http://example.org/p> "x" .\n<http://example.org/b> <http://example.org/p> "y" .\n';
    const { rest, quad } = parseNTriplesQuad(input);
    assert.equal(quad.termType, 'Quad');
    assert.equal(quad.subject.value, EX + 'a');
    assert.equal(quad.object.value, 'x');
    assert.equal(quad.graph.termType, 'DefaultGraph');
    assert.ok(rest.includes('http://example.org/b'));
    assert.ok(new Dataset([quad]).has(q('a', 'p', lit('x'))));
  });

  test('lang literal and blank node', () => {
    const { quad } = parseNTriplesQuad('_:b1 <http://example.org/p> "hi"@en .\n');
    assert.equal(quad.subject.termType, 'BlankNode');
    assert.equal(quad.object.language, 'en');
  });

  test('empty input yields null or throws', () => {
    let res;
    try {
      res = parseNTriplesQuad('');
    } catch {
      return;
    }
    assert.equal(res, null);
  });
});