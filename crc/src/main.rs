use crane_lex as lex;
use crane_parse as parse;
use lex::Span;
use parse::Package;

fn main() -> anyhow::Result<()> {
    let test = Span::new(
        "
struct Test {
    a: i32,
    b: ::test::Type
}

pub type Alias = self::Test

mod test {
    struct Type {
        a: i32,
        b: i32
    }
}",
    );
    let (rest, tokens) = lex::tokenize(test)?;

    let mut package = Package::new();

    let mut parser = parse::Parser::new(tokens, &mut package);
    let parsed = parser.parse_unit("bingus".into(), None)?;
    let unit = package.get_unit(parsed).unwrap();
    println!("{:#?}", package);
    package.dbg_print();
    Ok(())
}
