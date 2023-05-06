use ariadne::Span;
use crane_lex as lex;
use crane_parse as parse;

use parse::Package;

fn main() -> anyhow::Result<()> {
    let test = "
struct Test {
    a: i32,
    b: ::test::Type
}

pub type Alias = self::Test

fn test_func() {

}

mod test {
    struct Type {
        a: i32,
        b: i32
    }
}";
    let lexer = lex::Lexer::new();
    let (tokens, errors) = lexer.lex_str(test)?;

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

    println!("{:#?}", tokens);

    let mut package = Package::new();

    let mut parser = parse::Parser::new(tokens, &mut package);
    let parsed = parser.parse_unit("bingus".into(), None)?;
    let unit = package.get_unit(parsed).unwrap();

    println!("{:#?}", unit);
    Ok(())
}
