pub const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";
pub const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";
pub const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
pub const XSD_DECIMAL: &str = "http://www.w3.org/2001/XMLSchema#decimal";
pub const XSD_DOUBLE: &str = "http://www.w3.org/2001/XMLSchema#double";
pub const XSD_DATE: &str = "http://www.w3.org/2001/XMLSchema#date";
pub const XSD_DATE_TIME: &str = "http://www.w3.org/2001/XMLSchema#dateTime";
pub const XSD_TIME: &str = "http://www.w3.org/2001/XMLSchema#time";
#[allow(unused)]
pub const LANG_LITERAL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";
pub const NS_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
pub const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
pub const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
pub const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
pub const DEFAULT_WELL_KNOWN_PREFIX: &str = "http://example.org/.well-known/genid#";
pub static DATE_FORMATS: [&str; 9] = [
    "%Y-%m-%dT%H:%M:%S%.3f%Z",
    "%Y-%m-%dT%H:%M:%S%Z",
    "%Y-%m-%d%:z",
    "%Y-%m-%d",
    "%d-%m-%Y",
    "%Y-%d-%m",
    "%m-%d-%Y",
    "%m/%d/%Y",
    "%d/%m/%Y",
];
pub static TIME_FORMATS: [&str; 4] = ["%H:%M:%S%.3f%Z", "%H:%M:%S%Z", "%H:%M:%S", "%H:%M"];
pub const DEFAULT_DATE_TIME_FORMAT: &str = "%Y-%m-%dT%H:%M:%S%.3f%Z";
pub const DEFAULT_DATE_FORMAT: &str = "%Y-%m-%d";
pub const DEFAULT_TIME_FORMAT: &str = "%H:%M:%S%.3f%Z";
