use std::path::Path;

use crane_lex::new::*;

fn main() {
    let lexer = Lexer::new();
    let d = env!("CARGO_MANIFEST_DIR");
    let r = lexer.lex(Path::new(d).join("../test.cr").as_path());
    match r {
        Ok((tokens, errors)) => {
            if errors.len() > 0 {
                eprintln!("WARNINGS:");
                for e in errors {
                    eprintln!("{}", e);
                }
            }
            if let Some(tokens) = tokens {
                eprintln!("{:#?}", tokens);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }
}
