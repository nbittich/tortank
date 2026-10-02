// Adjust to the name of your wasm-pack output
const WASM_MODULE_PATH = "./pkg/tortank_wasm.js";
const ISSUE_URL = "https://github.com/nbittich/tortank/issues/new";

async function loadWasmContext() {
  const module = await import(WASM_MODULE_PATH);
  await module.default();
  return module; // TurtleDoc, uri, blankNode, literal, parseNTriplesStatement
}

/* ------------------------------------------------------------------ */
/* Default example data                                               */
/* ------------------------------------------------------------------ */
const TURTLE_A = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
  foaf:name "Alice"@en ;
  foaf:knows ex:bob .

ex:bob a foaf:Person ;
  foaf:name "Bob" .
`;

const TURTLE_B = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
  foaf:name "Alice"@en .

ex:carol a foaf:Person ;
  foaf:name "Carol" .
`;

const JSON_S = `{"type":"uri","value":"http://example.org/alice"}`;
const JSON_P = `{"type":"uri","value":"http://xmlns.com/foaf/0.1/knows"}`;
const JSON_O = `{"type":"uri","value":"http://example.org/bob"}`;
const JSON_TRIPLE = `{
  "subject": ${JSON_S},
  "predicate": ${JSON_P},
  "object": ${JSON_O}
}`;

/* ------------------------------------------------------------------ */
/* Field builders                                                     */
/* ------------------------------------------------------------------ */
const turtleField = (id = "turtle", label = "Turtle", value = TURTLE_A) => ({
  id,
  label,
  type: "textarea",
  rows: 10,
  value,
});
const wellKnownField = {
  id: "wellKnownPrefix",
  label: "Well Known prefix",
  type: "url",
  value: "http://example.org/.well-known/",
  help: "Well known prefix for Skolemisation (optional)",
  optional: true,
};
const termField = (id, label, value = "") => ({
  id,
  label,
  type: "text",
  value,
  optional: true,
  help: "Turtle term (e.g. ex:alice, a, \"hello\"@en). Leave empty for wildcard.",
});
const nodeField = (id, label, value = "", optional = true) => ({
  id,
  label,
  type: "textarea",
  rows: 7,
  value,
  optional,
  help: optional
    ? "RDF-JSON node. Leave empty for wildcard."
    : "RDF-JSON node.",
});
const textField = (id, label, value = "", optional = false) => ({
  id,
  label,
  type: "text",
  value,
  optional,
});

/* ------------------------------------------------------------------ */
/* Function catalogue                                                 */
/* Each entry: label, help, fields[], run(values, api) => any         */
/* api.parse(text) parses a doc (tracked and freed afterwards)        */
/* ------------------------------------------------------------------ */
const FUNCTIONS = {
  /* ---- conversions ---- */
  "TurtleDoc.parse → toTurtle": {
    help: "Parse a Turtle/N3 document and serialize it back.",
    fields: [turtleField(), wellKnownField],
    run: (v, api) => api.parse(v.turtle).toTurtle(),
  },
  "toJSON": {
    help: "RDF-JSON representation.",
    fields: [turtleField(), wellKnownField],
    run: (v, api) => api.parse(v.turtle).toJSON(),
  },
  "fromJSON": {
    help: "Build a doc from the RDF-JSON returned by toJSON(), then output Turtle.",
    fields: [
      {
        id: "json",
        label: "RDF-JSON",
        type: "textarea",
        rows: 10,
        value: `[
  {
    "subject": { "type": "uri", "value": "http://example.org/alice" },
    "predicate": { "type": "uri", "value": "http://xmlns.com/foaf/0.1/name" },
    "object": { "type": "literal", "value": "Alice", "lang": "en" }
  }
]`,
      },
    ],
    run: (v, api) => api.track(api.T.fromJSON(JSON.parse(v.json))).toTurtle(),
  },
  /* ---- inspection ---- */
  "statements": {
    help: "All statements as JS objects.",
    fields: [turtleField(), wellKnownField],
    run: (v, api) => api.parse(v.turtle).statements(),
  },
  "allSubjects": {
    help: "Unique subjects.",
    fields: [turtleField(), wellKnownField],
    run: (v, api) => api.parse(v.turtle).allSubjects(),
  },
  "len / length / isEmpty": {
    help: "Size information.",
    fields: [turtleField(), wellKnownField],
    run: (v, api) => {
      const d = api.parse(v.turtle);
      return { len: d.len(), length: d.length, isEmpty: d.isEmpty() };
    },
  },
  "contains": {
    help: "Check whether a complete RDF-JSON triple is in the doc.",
    fields: [
      turtleField(),
      wellKnownField,
      nodeField("triple", "Triple (RDF-JSON)", JSON_TRIPLE, false),
    ],
    run: (v, api) => api.parse(v.turtle).contains(JSON.parse(v.triple)),
  },

  /* ---- querying ---- */
  "match": {
    help: "Query using Turtle syntax. Prefixes are resolved by the doc.",
    fields: [
      turtleField(),
      wellKnownField,
      termField("subject", "Subject", "ex:alice"),
      termField("predicate", "Predicate", "foaf:knows"),
      termField("object", "Object", ""),
    ],
    run: (v, api) =>
      api
        .parse(v.turtle)
        .match(v.subject || null, v.predicate || null, v.object || null),
  },
  "matchNodes": {
    help: "Exact RDF-JSON node matching (no Turtle term parsing).",
    fields: [
      turtleField(),
      wellKnownField,
      termField("subject", "Subject (RDF-JSON)", JSON_S),
      termField("predicate", "Predicate (RDF-JSON)  ", JSON_P),
      termField("object", "Object (RDF-JSON)", ""),
    ],
    run: (v, api) =>
      api
        .parse(v.turtle)
        .matchNodes(
          optJson(v.subject),
          optJson(v.predicate),
          optJson(v.object),
        ),
  },

  /* ---- mutation ---- */
  "add": {
    help: "Add a statement from three RDF-JSON nodes. Duplicates are ignored.",
    fields: [
      turtleField(),
      wellKnownField,
      termField("subject", "Subject (RDF-JSON)", JSON_S, false),
      termField("predicate", "Predicate (RDF-JSON)", JSON_P, false),
      termField("object", "Object (RDF-JSON)", `{"type":"uri","value":"http://example.org/robert"}`, false),
    ],
    run: (v, api) => {
      const d = api.parse(v.turtle);
      const added = d.add(
        JSON.parse(v.subject),
        JSON.parse(v.predicate),
        JSON.parse(v.object),
      );
      return `# added: ${added}\n${d.toTurtle()}`;
    },
  },
  "addTriple": {
    help: "Add a complete RDF-JSON triple.",
    fields: [
      turtleField(),
      wellKnownField,
      nodeField("triple", "Triple (RDF-JSON)", JSON_TRIPLE, false),
    ],
    run: (v, api) => {
      const d = api.parse(v.turtle);
      const added = d.addTriple(JSON.parse(v.triple));
      return `# added: ${added}\n${d.toTurtle()}`;
    },
  },
  "remove": {
    help: "Remove matching triples. Empty fields are wildcards.",
    fields: [
      turtleField(),
      wellKnownField,
      termField("subject", "Subject (RDF-JSON)", JSON_S),
      termField(
        "predicate",
        "Predicate (RDF-JSON)",
        `{"type":"uri","value":"http://xmlns.com/foaf/0.1/knows"}`,
      ),
      termField("object", "Object (RDF-JSON)", ""),
    ],
    run: (v, api) => {
      const d = api.parse(v.turtle);
      const n = d.remove(
        optJson(v.subject),
        optJson(v.predicate),
        optJson(v.object),
      );
      return `# removed: ${n}\n${d.toTurtle()}`;
    },
  },
  "clear": {
    help: "Remove everything from the doc.",
    fields: [turtleField(), wellKnownField],
    run: (v, api) => {
      const d = api.parse(v.turtle);
      const before = d.len();
      d.clear();
      return `# before: ${before}, after: ${d.len()}, isEmpty: ${d.isEmpty()}\n${d.toTurtle()}`;
    },
  },

  /* ---- set operations (two docs) ---- */
  "difference": {
    help: "A.difference(B): statements in A that are not in B.",
    fields: [
      turtleField("turtle", "Turtle A (base model)", TURTLE_A),
      turtleField("other", "Turtle B (to diff)", TURTLE_B),
      wellKnownField,
    ],
    run: (v, api) => api.parse(v.turtle).difference(api.parse(v.other)).toTurtle(),
  },
  "intersection": {
    help: "A.intersection(B): statements in both A and B.",
    fields: [
      turtleField("turtle", "Turtle A (base model)", TURTLE_A),
      turtleField("other", "Turtle B", TURTLE_B),
      wellKnownField,
    ],
    run: (v, api) =>
      api.parse(v.turtle).intersection(api.parse(v.other)).toTurtle(),
  },

};

/* ------------------------------------------------------------------ */
/* Helpers                                                            */
/* ------------------------------------------------------------------ */
function optJson(text) {
  const t = (text || "").trim();
  return t ? JSON.parse(t) : null;
}

function stringify(result) {
  if (typeof result === "string") return result;
  if (result === undefined) return "undefined";
  return JSON.stringify(
    result,
    (_, val) => {
      if (val instanceof Map) return Object.fromEntries(val);
      if (val instanceof Set) return [...val];
      return val;
    },
    2,
  );
}

// values typed by the user, kept per function so switching doesn't lose them
const state = {};

function getValues(fnName) {
  const values = {};
  for (const f of FUNCTIONS[fnName].fields) {
    values[f.id] = state[fnName]?.[f.id] ?? f.value ?? "";
  }
  return values;
}

function renderFields(fnName) {
  const fn = FUNCTIONS[fnName];
  const container = document.querySelector("#dynamicFields");
  container.innerHTML = "";
  document.querySelector("#fnHelp").textContent = fn.help || "";
  const values = getValues(fnName);
  state[fnName] ??= {};

  for (const f of fn.fields) {
    const wrap = document.createElement("div");
    wrap.className = "mb-2 align-items-center";

    const labelCol = document.createElement("div");
    labelCol.className = "col-auto";
    const label = document.createElement("label");
    label.htmlFor = `f_${f.id}`;
    label.className = "col-form-label fw-bold";
    label.textContent = f.label;
    labelCol.appendChild(label);

    const inputCol = document.createElement("div");
    inputCol.className = "col-auto";
    let input;
    if (f.type === "textarea") {
      input = document.createElement("textarea");
      input.rows = f.rows || 4;
    } else {
      input = document.createElement("input");
      input.type = f.type;
    }
    input.id = `f_${f.id}`;
    input.name = f.id;
    input.className = "form-control flat";
    if (!f.optional) input.required = true;
    input.value = values[f.id];
    state[fnName][f.id] = input.value;
    input.addEventListener("input", () => (state[fnName][f.id] = input.value));
    inputCol.appendChild(input);

    if (f.help) {
      const help = document.createElement("div");
      help.className = "form-text";
      help.textContent = f.help;
      inputCol.appendChild(help);
    }

    wrap.append(labelCol, inputCol);
    container.appendChild(wrap);
  }
}

function toggleForm(form, toggle) {
  for (const el of form.elements) el.readOnly = toggle;
  form.querySelector("#fnSelect").disabled = toggle;
  form.querySelector('button[type="submit"]').disabled = toggle;
}

/* ------------------------------------------------------------------ */
/* Main                                                               */
/* ------------------------------------------------------------------ */
async function run() {
  const form = document.querySelector("form");
  const select = document.querySelector("#fnSelect");
  const out = document.querySelector("#out");

  for (const name of Object.keys(FUNCTIONS)) {
    select.add(new Option(name, name));
  }
  renderFields(select.value);
  select.addEventListener("change", () => renderFields(select.value));

  toggleForm(form, true);
  let mod;
  try {
    mod = await loadWasmContext();
  } catch (e) {
    out.innerText = `Could not load wasm module (${WASM_MODULE_PATH}):\n${e}`;
    console.error(e);
    return;
  }
  toggleForm(form, false);

  form.addEventListener("submit", (e) => {
    e.preventDefault();
    const fnName = select.value;
    const values = getValues(fnName);
    const docs = [];
    const api = {
      mod,
      T: mod.TurtleDoc,
      track: (d) => (docs.push(d), d),
      parse: (text) =>
        api.track(mod.TurtleDoc.parse(text, values.wellKnownPrefix || null)),
    };
    try {
      out.innerText = stringify(FUNCTIONS[fnName].run(values, api));
    } catch (err) {
      out.innerText = `Error: ${err?.message ?? err}`;
      console.error(err);
    } finally {
      for (const d of docs) {
        try {
          d.free();
        } catch (_) {}
      }
    }
  });

  document.querySelector("#copyToClipboard").onclick = (e) => {
    e.preventDefault();
    navigator.clipboard.writeText(out.innerText);
    alert("Copied to clipboard");
  };

  document.querySelector("#issueLink").onclick = (e) => {
    e.preventDefault();
    const fnName = select.value;
    const values = getValues(fnName);
    const inputs = Object.entries(values)
      .map(([k, v]) => `### ${k}:\n\n\`\`\`\n${v}\n\`\`\``)
      .join("\n\n");
    const params = new URLSearchParams();
    params.append("title", `TurtleDoc bug: ${fnName}`);
    params.append("body", `### Function: \`${fnName}\`\n\n${inputs}\n`);
    const a = document.createElement("a");
    a.href = `${ISSUE_URL}?${params.toString()}`;
    a.target = "_blank";
    a.click();
  };
}

run();
