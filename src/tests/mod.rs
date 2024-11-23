use std::{path::PathBuf, str::FromStr};

use crate::turtle::turtle_doc::TurtleDoc;

fn cmp_input_file(
    test_name: &str,
    diff_file: Option<&str>,
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
        assert_eq!(input.len(), output.len());
        assert_eq!(diff.len(), 0);
    }
}
mod turtle_doc_test;
