use serial_test::serial;
use test_case::test_case;

use crate::{tests::cmp_input_file, turtle::turtle_doc::reset_fake_uuid_gen};

const INPUT_DIR: &str = "examples/turtle_doc";

#[test_case("0001", None          , false ; "EQ: complex document with blank nodes, nested objects, etc")]
#[test_case("0002", None          , false ; "EQ: another complex document")]
#[test_case("0003", None          , false ; "EQ: could not parse completely")]
#[test_case("0004", Some("0004")  , false ; "DIFF: diff is buggy")]
#[test_case("0005", Some("0005")  , false ; "DIFF: simple diff")]
#[test_case("0006", None          , false ; "EQ: complex string with spaces")]
#[test_case("0007", None          , false ; "EQ: complex string with spaces but more complex")]
#[test_case("0008", None          , false ; "EQ: simple doc with comments etc")]
#[test_case("0009", None          , false ; "EQ: test simple blank nodes (unlabeled)")]
#[test_case("0010", None          , false ; "EQ: test simple xsd:time")]
#[test_case("0011", None          , false ; "EQ: test simple parsing")]
#[test_case("0012", None          , false ; "EQ: test simple parsing multi comments")]
#[test_case("0013", None          , false ; "EQ: test labeled bnode error")]
#[test_case("0014", None          , false ; "EQ: a bit of everything (list, bnode, nested unlabeled bnode etc)")]
#[test_case("0015", None          , false ; "EQ: test simple collections")]
#[test_case("0016", None          , false ; "EQ: test date 2000-01-12T12:13:14Z")]
#[test_case("0017", None          , false ; "EQ: test date 2002-10-10+13:00")]
#[test_case("0018", None          , false ; "EQ: test date 2002-10-10T00:00:00+13")]
#[test_case("0019", None          , false ; "EQ: test date 2002-10-09T11:00:00Z")]
#[test_case("0020", None          , false ; "EQ: test date 2002-10-10T00:00:00+05:00")]
#[test_case("0021", None          , false ; "EQ: test date 2002-10-09T19:00:00Z")]
#[test_case("0022", None          , false ; "EQ: test date 2002-09-29")]
#[test_case("0023", None          , false ; "EQ: test date 20-09-2021")]
#[test_case("0024", None          , false ; "EQ: test date 09/20/2021")]
#[test_case("0025", None          , false ; "EQ: test date 20/09/2012")]
#[test_case("0026", None          , false ; "EQ: test date 2023-08-30T10:31:00.080Z")]
#[test_case("0027", None          , true  ; "JSON: test simple json result with bnode")]
#[test_case("0028", None          , false  ; "EQ: The following Turtle document contains examples of all the different ways of writing IRIs in Turtle.")]
#[test_case("0029", None          , false  ; "EQ: Simple base example")]
#[test_case("0030", None          , false  ; "EQ: empty STRING_LITERAL_LONG_QUOTE")]
#[test_case("0031", None          , false  ; "EQ: alt quotes")]
#[serial]
fn test_turtle_doc(test_name: &str, diff_file: Option<&str>, output_json: bool) {
    reset_fake_uuid_gen();
    cmp_input_file(test_name, diff_file, output_json, INPUT_DIR, None);
}
