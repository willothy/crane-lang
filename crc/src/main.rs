use crane_lex as lex;
use crane_parse as parse;

use parse::package::{
    pass::{PrintPackage, PrintUnit},
    Package,
};

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let test = include_str!("../../test.cr");

    let lexer_res = lex::lex(test, "test")?;

    let errors = lexer_res.errors();

    if errors.len() > 0 {
        println!("Encountered lexer errors");
    }
    for error in errors {
        println!("{}", error);
    }

    if !lexer_res.has_output() {
        return Err(anyhow::anyhow!("Lexer failed"));
    }

    let tokens = lexer_res.into_output().unwrap();

    // println!(
    //     "{:#?}",
    //     tokens
    //         .iter() /* .rev().take(5).rev()*/
    //         .collect::<Vec<_>>()
    // );

    let mut package = Package::new();

    let parsed = parse::parse(tokens, &mut package, "test".to_owned())?;
    let errors = parsed.errors();
    if errors.len() > 0 {
        println!("Encountered parse errors");
    }

    for error in errors {
        println!("{}", error);
    }

    // Inspect the unit / package AST trees
    //
    // let id = parsed.into_output().unwrap();
    // let unit = package.unit(id).unwrap();
    // println!("{:#?}", unit);
    // println!("{:#?}", package);

    println!("Reconstructed from AST:\n");
    package.inspect(&mut PrintPackage, ());
    Ok(())
}
