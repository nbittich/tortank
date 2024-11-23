use serial_test::serial;
use test_case::test_case;

use crate::{tests::cmp_input_file, turtle::turtle_doc::reset_fake_uuid_gen};

const INPUT_DIR: &str = "examples/turtle_doc";

#[test_case("0001", None            ; "EQ: complex document with blank nodes, nested objects, etc")]
#[test_case("0002", None            ; "EQ: another complex document")]
#[test_case("0003", None            ; "EQ: could not parse completely")]
#[test_case("0004", Some("0004")    ; "DIFF: diff is buggy")]
#[test_case("0005", Some("0005")    ; "DIFF: simple diff")]
#[test_case("0006", None            ; "EQ: complex string with spaces")]
#[test_case("0007", None            ; "EQ: complex string with spaces but more complex")]
#[serial]
fn test_turtle_doc(test_name: &str, diff_file: Option<&str>) {
    reset_fake_uuid_gen();
    cmp_input_file(test_name, diff_file, INPUT_DIR, None);
}
