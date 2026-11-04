//! Regenerate the Moss sources that are tables (see `moss_tests::gensrc`):
//! `cargo run -p moss-tests --bin gensrc`.

fn main() {
    for (rel, text) in moss_tests::gensrc::generated() {
        let path = moss_tests::repo().join(rel);
        std::fs::write(&path, text).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
        println!("{rel}");
    }
}
