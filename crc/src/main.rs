use ariadne::Span;
use crane_lex as lex;
use crane_parse as parse;

use parse::package::Package;

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let test = include_str!("../../test.cr");
    let (tokens, errors) = lex::lex_str(test, "test")?;

    for error in errors {
        println!(
            "{} ({})",
            error,
            // error.span().,
            error.span().start()
        );
    }
    let Some(tokens) = tokens else {
        println!("No tokens");
        return Ok(());
    };

    // println!(
    //     "{:#?}",
    //     tokens.iter().rev().take(5).rev().collect::<Vec<_>>()
    // );

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
