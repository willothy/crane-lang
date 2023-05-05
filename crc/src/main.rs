use crane_lex as lex;
use crane_parse as parse;
use lex::Span;
use parse::Package;

fn main() -> anyhow::Result<()> {
    let test = Span::new(
        "

mod test {
    struct Type {
        a: i32,
        b: i32
    }
}
",
    );
    let (rest, tokens) = lex::tokenize(test)?;
    tokens.iter().for_each(|t| println!("{:?}", t.kind));

    let mut package = Package::new();

    let mut parser = parse::Parser::new(tokens, &mut package);
    let parsed = parser.parse_unit("root".into(), None)?;
    let unit = package.get_unit(parsed).unwrap();
    println!("{:#?}", package);
    Ok(())
}
