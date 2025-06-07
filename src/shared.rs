pub const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";
pub const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";
pub const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
pub const XSD_DECIMAL: &str = "http://www.w3.org/2001/XMLSchema#decimal";
pub const XSD_DOUBLE: &str = "http://www.w3.org/2001/XMLSchema#double";
pub const XSD_DATE: &str = "http://www.w3.org/2001/XMLSchema#date";
pub const XSD_DATE_TIME: &str = "http://www.w3.org/2001/XMLSchema#dateTime";
pub const XSD_TIME: &str = "http://www.w3.org/2001/XMLSchema#time";
pub static LANG_LITERAL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";
pub static NS_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
pub static RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
pub static RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
pub static RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
pub static DEFAULT_WELL_KNOWN_PREFIX: &str = "http://example.org/.well-known/genid#";
pub static DATE_FORMATS: [&str; 12] = [
    "%Y-%m-%dT%H:%M:%S%.3f%Z",
    "%Y-%m-%dT%H:%M:%S%Z",
    "%Y-%m-%dT%H:%M:%S%:z",
    "%Y-%m-%d%:z",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d",
    "%d-%m-%Y",
    "%Y-%d-%m",
    "%m-%d-%Y",
    "%m/%d/%Y",
    "%d/%m/%Y",
    "%+",
];

pub static TIME_FORMATS: [&str; 4] = ["%H:%M:%S%.3f%Z", "%H:%M:%S%Z", "%H:%M:%S", "%H:%M"];
pub static DEFAULT_DATE_TIME_FORMAT: &str = "%Y-%m-%dT%H:%M:%S%.3f%Z";
pub static DEFAULT_DATE_FORMAT: &str = "%Y-%m-%d";
pub static DEFAULT_TIME_FORMAT: &str = "%H:%M:%S%.3f%Z";
