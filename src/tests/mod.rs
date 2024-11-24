use std::{fs::File, io::BufReader, path::PathBuf, str::FromStr};

use crate::turtle::turtle_doc::{RdfJsonTriple, TurtleDoc};

const DEBUG_TTL: bool = false;
fn cmp_input_file(
    test_name: &str,
    diff_file: Option<&str>,
    output_json: bool,
    directory: &str,
    well_known_prefix: Option<String>,
) {
    println!("running {test_name}");
    let mut input_buf = String::new();
    let mut output_buf = String::new();
    let input = TurtleDoc::from_file(
        PathBuf::from_str(directory)
            .map(|p| p.join("input").join(format!("{test_name}.ttl")))
            .unwrap(),
        well_known_prefix.clone(),
        &mut input_buf,
    )
    .unwrap();

    if output_json {
        let f = File::open(
            PathBuf::from_str(directory)
                .map(|p| p.join("output").join(format!("{test_name}.json")))
                .unwrap(),
        )
        .unwrap();
        let output: Vec<RdfJsonTriple> = serde_json::from_reader(BufReader::new(f)).unwrap();
        assert_eq!(input.len(), output.len());
        let output_doc: TurtleDoc = (&output).try_into().unwrap();
        assert_eq!(input.difference(&output_doc).unwrap().len(), 0);
        let input: Vec<RdfJsonTriple> = (&input).into();
        assert_eq!(input, output);
        return;
    }

    let output = TurtleDoc::from_file(
        PathBuf::from_str(directory)
            .map(|p| p.join("output").join(format!("{test_name}.ttl")))
            .unwrap(),
        well_known_prefix.clone(),
        &mut output_buf,
    )
    .unwrap();
    if let Some(diff) = diff_file {
        let mut diff_buf = String::new();
        let diff = TurtleDoc::from_file(
            PathBuf::from_str(directory)
                .map(|p| p.join("diff").join(format!("{diff}.ttl")))
                .unwrap(),
            well_known_prefix,
            &mut diff_buf,
        )
        .unwrap();
        let input = input.difference(&diff).unwrap();
        assert!(!input.is_empty());
        assert_eq!(input.difference(&output).unwrap().len(), 0);
    } else {
        let diff = input.difference(&output).unwrap();
        if DEBUG_TTL {
            println!(
                "{}",
                input
                    .to_string()
                    .replace("\n", "<NEWLINE>")
                    .replace("\t", "<TAB>")
                    .replace(" ", "<SPACE>"),
            );
            println!("===");
            println!(
                "{}",
                output
                    .to_string()
                    .replace("\n", "<NEWLINE>")
                    .replace("\t", "<TAB>")
                    .replace(" ", "<SPACE>"),
            );
        }
        if !diff.is_empty() {
            println!("========== Differences ==========");
            println!("{diff}");
            println!("========== Differences ==========");
        }
        assert_eq!(diff.len(), 0);
        assert_eq!(input.len(), output.len());
    }
}
mod triple_common_parser_test_misc;
mod turtle_doc_test;
mod turtle_doc_test_misc;
mod turtle_parser_test_misc;
