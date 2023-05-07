use crane_lex as lex;
use crane_parse as parse;

use parse::package::Package;

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let test = include_str!("../../test.cr");

    let lexer_res = lex::lex_str(test, "test")?;

    let errors = lexer_res.errors();

    for error in errors {
        println!("{}", error);
    }

    if !lexer_res.has_output() {
        return Err(anyhow::anyhow!("Lexer failed"));
    }

    let tokens = lexer_res.into_output().unwrap();

    println!(
        "{:#?}",
        tokens.iter().rev().take(5).rev().collect::<Vec<_>>()
    );

    // use lex::IntoStream;
    // let mut stream = tokens.into_stream();
    // stream.fetch_tokens().for_each(|t| {
    //     println!("{:#?}", t);
    // });

    let mut package = Package::new();

    let mut parser = parse::Parser::new(tokens, &mut package);
    let parsed = parser.parse_unit("bingus".into(), None)?;
    let unit = package.unit(parsed).unwrap();

    println!("{:#?}", unit);
    package.dbg_print();
    Ok(())
}
