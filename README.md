# tortank

Turtle / N-Triples parser and in-memory RDF store written in Rust, usable from:

| API | Language | Package |
| --- | --- | --- |
| [Rust API](#rust-api) | Rust | [`tortank`](https://crates.io/crates/tortank) (crates.io) |
| [RDF/JS API](#rdfjs-api) | JavaScript / TypeScript (wasm) | `@nbittich/tortank-wasm` (Node), `@nbittich/web-tortank` (browser) |
| [Legacy JS API](#legacy-js-api) | JavaScript / TypeScript (wasm) | same packages as above |

Playground: <https://nbittich.github.io/tortank>

## Features

- Covers (roughly) the [Turtle spec](https://www.w3.org/TR/turtle/): prefixes, collections, blank nodes, predicate-object lists, etc.
- Comments are filtered.
- Blank nodes are skolemized (`<http://example.org/.well-known/genid#<uuid>>`). The prefix and the uuid generator are configurable.
- Set operations (`difference`, `intersection`), pattern matching and turtle serialization.
- Only the default graph is supported (no named graphs).

## Repository layout

| Crate | Description |
| --- | --- |
| [`libtortank`](./libtortank) | The parser / store, published as `tortank` |
| [`tortank-wasm`](./tortank-wasm) | wasm bindings (RDF/JS + legacy API) |

---

## Rust API

### Installation

```sh
cargo add tortank
```

or in your `Cargo.toml`:

```toml
[dependencies]
tortank = "0.31"
```

Requires Rust edition 2024 (see `rust-version` in [`Cargo.toml`](./Cargo.toml)).

### Example

```rust
use tortank::turtle::turtle_doc::{TurtleDoc, TurtleDocError};

fn main() -> Result<(), TurtleDocError> {
    let input = r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix ex: <http://example.org/> .

        ex:alice foaf:name "Alice" ; foaf:knows ex:bob .
        ex:bob   foaf:name "Bob" .
    "#;

    // Parse a turtle string. 2nd argument: prefix used to skolemize blank nodes
    // (None = default). 3rd argument: custom uuid generator (None = default).
    let doc = TurtleDoc::try_from((input, None, None))?;
    println!("{} statements", doc.len());

    // Query using turtle syntax (prefixes of the document are available)
    for stmt in doc.parse_and_list_statements(
        None,
        Some("foaf:name".into()),
        Some("\"Alice\"".into()),
    )? {
        println!("{stmt}");
    }

    // Serialize back to turtle
    println!("{}", doc.as_turtle()?);
    Ok(())
}
```

Output (the order of statements/prefixes in `as_turtle()` is not guaranteed):

```
3 statements
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> """Alice"""^^<http://www.w3.org/2001/XMLSchema#string>.
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
@prefix ex: <http://example.org/>.
@prefix foaf: <http://xmlns.com/foaf/0.1/>.

<http://example.org/alice> foaf:name """Alice""";
	foaf:knows ex:bob.

<http://example.org/bob> foaf:name """Bob""".
```

Other useful methods on `TurtleDoc`: `empty_doc`, `from_file`, `to_file`, `add_statement`, `remove_statements`, `list_statements`, `difference`, `intersection`, `all_subjects`, `clear`.

---

## JavaScript / TypeScript (wasm)

Two JS APIs are exposed by the same wasm module:

- **[RDF/JS API](#rdfjs-api)**: `DataFactory` + `Dataset`, following the [RDF/JS specification](https://rdf.js.org/). Recommended for new code, it interoperates with other RDF/JS libraries.
- **[Legacy JS API](#legacy-js-api)**: `TurtleDoc` working on plain [RDF/JSON](https://www.w3.org/TR/rdf-json/)-like objects. Kept for backward compatibility.

### Installation

**Node.js**

```sh
npm i @nbittich/tortank-wasm
```

```js
const { Dataset, DataFactory } = require("@nbittich/tortank-wasm");
```

**Browser** (`--target web` build)

```sh
npm i @nbittich/web-tortank
```

```js
import init, { Dataset, DataFactory } from "@nbittich/web-tortank";

await init(); // loads the .wasm file, must be awaited once before any other call
```

**Building from source** (needs Rust and [`wasm-pack`](https://rustwasm.github.io/wasm-pack/))

```sh
cd tortank-wasm
RUSTFLAGS='--cfg getrandom_backend="wasm_js" -Ctarget-cpu=mvp' \
  wasm-pack build --target nodejs --release --scope nbittich .   # or --target web
```

The output is generated in `tortank-wasm/pkg`.

> **Note**
> The RDF/JS API is more recent than `0.31.6`. If `Dataset` / `DataFactory` are
> not exported by the version you installed, use a newer release or build from source.

---

### RDF/JS API

`DataFactory` creates terms and quads, `Dataset` implements `DatasetCore` / `Dataset` from the RDF/JS spec
(`add`, `delete`, `has`, `match`, `size`, `addAll`, `deleteMatches`, `difference`, `intersection`, `union`, `filter`, `map`,
`forEach`, `some`, `every`, `reduce`, `equals`, `contains`, `toArray`, and `[Symbol.iterator]`).
`null`, `undefined` or a `Variable` act as wildcards in `match` / `deleteMatches`.

Extras that are not part of RDF/JS:

| Method | Description |
| --- | --- |
| `Dataset.parse(turtle, wellKnownPrefix?, uuidFn?)` | Parse a turtle string |
| `dataset.toTurtle()` / `toString()` | Serialize to turtle |
| `dataset.matchTurtle(s?, p?, o?)` | Match using turtle syntax strings (`"ex:foo"`, `"a"`, `"\"lit\""`) |
| `Dataset.fromRdfJson(json)` / `fromRdfJsonString(str)` | Import RDF-JSON |
| `dataset.toRdfJson()` / `toRdfJsonString()` | Export RDF-JSON |
| `dataset.clear()` / `clone()` | |
| `parseNTriplesQuad(line, uuidFn?)` | Parse one N-Triples statement, returns `{ rest, quad }` or `null` |

Only the default graph exists: quads with a named graph are rejected by `add`.

#### Example

```js
const { DataFactory, Dataset } = require("@nbittich/tortank-wasm");

const df = new DataFactory();
const foaf = (name) => df.namedNode("http://xmlns.com/foaf/0.1/" + name);
const ex = (name) => df.namedNode("http://example.org/" + name);

// 1. Parse turtle
const ds = Dataset.parse(`
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .

  ex:alice foaf:name "Alice" ; foaf:knows ex:bob .
  ex:bob   foaf:name "Bob" .
`);
console.log(ds.size); // 3

// 2. Add a quad (mutators return the dataset, so they can be chained)
ds.add(df.quad(ex("carol"), foaf("name"), df.literal("Carol")));
console.log(ds.size); // 4

// 3. Match: every foaf:name (null = wildcard)
for (const quad of ds.match(null, foaf("name"), null)) {
  console.log(quad.subject.value, "->", quad.object.value);
}

// 4. Check / remove
const alice = df.quad(ex("alice"), foaf("name"), df.literal("Alice"));
console.log(ds.has(alice)); // true
ds.delete(alice);
console.log(ds.has(alice)); // false

// 5. Serialize
console.log(ds.toTurtle());
```

---

### Legacy JS API

`TurtleDoc` works with plain objects in RDF-JSON shape (`{ type, value, datatype?, lang? }`).
Helpers `uri(value)`, `blankNode(value)` and `literal(value, datatype?, lang?)` build those nodes.

| Method | Description |
| --- | --- |
| `TurtleDoc.parse(turtle, wellKnownPrefix?, uuidFn?)` | Parse a turtle string |
| `TurtleDoc.newEmptyDoc()` | Empty document |
| `TurtleDoc.fromJSON(json)` / `fromJSONString(str)` | Import RDF-JSON triples |
| `doc.toTurtle()` / `toString()` | Serialize to turtle |
| `doc.statements()` / `toJSON()` / `toJSONString()` | Export RDF-JSON triples |
| `doc.length` / `len()` / `isEmpty()` | Size |
| `doc.match(s?, p?, o?)` | Match using turtle syntax strings |
| `doc.matchNodes(s?, p?, o?)` | Exact match with RDF-JSON nodes (`null` = wildcard) |
| `doc.add(s, p, o)` / `addTriple(triple)` | Returns `false` if the statement already exists |
| `doc.remove(s?, p?, o?)` | Returns the number of removed statements |
| `doc.contains(triple)` | |
| `doc.difference(other)` / `intersection(other)` | |
| `doc.allSubjects()` / `clear()` / `clone()` | |
| `parseNTriplesStatement(line, uuidFn?)` | Parse one N-Triples statement, returns `{ rest, statement }` or `null` |

#### Example

```js
const { TurtleDoc, uri, literal } = require("@nbittich/tortank-wasm");

const doc = TurtleDoc.parse(`
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .

  ex:alice foaf:name "Alice" ; foaf:knows ex:bob .
  ex:bob   foaf:name "Bob" .
`);
console.log(doc.length); // 3

doc.add(
  uri("http://example.org/carol"),
  uri("http://xmlns.com/foaf/0.1/name"),
  literal("Carol"),
);

// match with turtle syntax (null = wildcard), prefixes of the document are available
console.log(doc.match(null, "foaf:name", null).length); // 3
console.log(JSON.stringify(doc.match("ex:alice", null, null)));
// [{"subject":{"type":"uri","value":"http://example.org/alice"},
//   "predicate":{"type":"uri","value":"http://xmlns.com/foaf/0.1/name"},
//   "object":{"type":"literal","datatype":"http://www.w3.org/2001/XMLSchema#string","value":"Alice"}}, ...]

console.log(doc.toTurtle());
```

---

## Custom blank node ids

Blank nodes are skolemized with random uuids. Pass a function returning a string to control them (useful for tests):

```js
let i = 0;
const doc = TurtleDoc.parse(
  `<https://example.com/a> <https://example.com/knows> [ <https://example.com/name> "Bob" ] .`,
  null,        // well known prefix (null = default)
  () => `${++i}`, // uuid generator
);
```

## Example: blank nodes and collections

Input

```turtle
@prefix foaf: <http://foaf.com/>.
 [ foaf:name "Alice" ] foaf:knows [
 foaf:name "Bob" ;
 foaf:lastName "George", "Joshua" ;
 foaf:knows [
     foaf:name "Eve" ] ;
 foaf:mbox <bob@example.com>] .
```

Output (N-Triples, uuids differ on each run)

```
<http://example.org/.well-known/genid#e162c9a7-52cf-4240-9359-b1b1f977f642> <http://foaf.com/name> "Alice"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/name> "Bob"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/lastName> "George"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/lastName> "Joshua"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#7a9cfb63-1bfa-44ee-bfb9-c3d3db7da920> <http://foaf.com/name> "Eve"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/knows> <http://example.org/.well-known/genid#7a9cfb63-1bfa-44ee-bfb9-c3d3db7da920>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/mbox> <bob@example.com>.
<http://example.org/.well-known/genid#e162c9a7-52cf-4240-9359-b1b1f977f642> <http://foaf.com/knows> <http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2>.
```

Input

```turtle
@prefix : <http://example.com/>.
:a :b ( "apple" "banana" ) .
```

Output

```
<http://example.org/.well-known/genid#018a16a1-9f82-4acd-930e-2be1ea090e83> <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> "apple"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#4ef6beb7-5e21-4d77-9459-2662b6845375> <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> "banana"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#4ef6beb7-5e21-4d77-9459-2662b6845375> <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>.
<http://example.org/.well-known/genid#018a16a1-9f82-4acd-930e-2be1ea090e83> <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://example.org/.well-known/genid#4ef6beb7-5e21-4d77-9459-2662b6845375>.
<http://example.com/a> <http://example.com/b> <http://example.org/.well-known/genid#018a16a1-9f82-4acd-930e-2be1ea090e83>.
```

## Todo

- Error handling
- Named graphs

## License

[MIT](./LICENSE)
