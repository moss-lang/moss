//! Every runnable example, compiled by the compiler written in Moss and
//! run, prints its golden output; and so does the oracle.

use moss_tests::{oracle_run, read, repo, run, runnable_examples};

#[test]
fn examples_match_their_goldens() {
    for name in runnable_examples() {
        let golden = read(&format!("tests/examples/stdout/{name}.txt"));
        assert_eq!(run(&format!("examples/{name}.moss"), &[]), golden, "{name}");
    }
}

/// The oracle runs each example too: the Rocq front end, elaborator and
/// interpreter agree with the golden (and so with the compiler).
#[test]
fn the_oracle_matches_the_goldens() {
    for name in runnable_examples() {
        let golden = read(&format!("tests/examples/stdout/{name}.txt"));
        assert_eq!(oracle_run(&format!("examples/{name}.moss"), &[]), golden, "{name}");
    }
}

/// The other direction of the listing: a golden once outlived its example
/// by a month because nothing looked.
#[test]
fn every_golden_belongs_to_an_example() {
    let stems = |dir: &str, ext: &str| -> std::collections::BTreeSet<String> {
        std::fs::read_dir(repo().join(dir))
            .unwrap()
            .map(|e| e.unwrap().path())
            .filter(|p| p.extension().is_some_and(|x| x == ext))
            .map(|p| p.file_stem().unwrap().to_str().unwrap().to_string())
            .collect()
    };
    assert_eq!(stems("tests/examples/stdout", "txt"), stems("examples", "moss"));
}
