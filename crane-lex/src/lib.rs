use std::path::PathBuf;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{anychar, digit0, digit1, hex_digit1, multispace0},
        is_alphabetic,
        streaming::oct_digit1,
    },
    combinator::{map, opt, verify},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
pub struct SourceFile {
    pub path: PathBuf,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Fn,       // fn
    Let,      // let
    If,       // if
    Else,     // else
    Return,   // return
    While,    // while
    For,      // for
    In,       // in
    Break,    // break
    Loop,     // loop
    Continue, // continue
    Mod,      // mod
    Struct,
    Type,
    Impl,
    As,
    Use,
    Super,
    Self_,
    Root, // struct
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    /// 1, -5, 109, etc.
    Int(isize),
    /// 1.0, 0.0005, 1., -1.1, etc
    Float(f64),
    /// "example string"
    String(String),
    /// 'c'
    Char(char),
    /// true | false
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assignment {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    XorAssign,
    AndAssign,
    OrAssign,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Comparison {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Logical {
    And,
    Or,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Bitwise {
    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,
    Xor,
    ShiftRight,
    ShiftLeft,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Arithmetic {
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Punctuation {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Semicolon,
    Colon,
    DoubleColon,
    Question,
    OpenAngle,
    CloseAngle,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    Arithmetic(Arithmetic),
    Bitwise(Bitwise),
    Logical(Logical),
    Comparison(Comparison),
    Assignment(Assignment),
    Punctuation(Punctuation),
    Visibility(Visibility),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Keyword(Keyword),
    Literal(Literal),
    Symbol(Symbol),
    Ident(String),
    Visibility(Visibility),
}

impl Token {
    pub fn same_kind(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Keyword(l), Token::Keyword(r)) => l == r,
            (Token::Literal(l), Token::Literal(r)) => match (l, r) {
                (Literal::Int(_), Literal::Int(_)) => true,
                (Literal::Float(_), Literal::Float(_)) => true,
                (Literal::String(_), Literal::String(_)) => true,
                (Literal::Char(_), Literal::Char(_)) => true,
                (Literal::Bool(_), Literal::Bool(_)) => true,
                _ => false,
            },
            (Token::Symbol(l), Token::Symbol(r)) => l == r,
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Visibility(_), Token::Visibility(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SpannedToken<'a> {
    pub kind: Token,
    pub span: Span<'a>,
}

impl<'a> SpannedToken<'a> {
    pub fn new(kind: Token, span: Span<'a>) -> Self {
        Self { kind, span }
    }
}

pub fn keyword(s: Span) -> IResult<Span, SpannedToken> {
    map(
        alt((
            tag("fn"),
            tag("let"),
            tag("if"),
            tag("else"),
            tag("return"),
            tag("while"),
            tag("for"),
            tag("in"),
            tag("break"),
            tag("loop"),
            tag("continue"),
            tag("mod"),
            tag("struct"),
            tag("type"),
            tag("root"),
            tag("self"),
            tag("super"),
            tag("use"),
            tag("as"),
            tag("impl"),
        )),
        |r: Span| {
            SpannedToken::new(
                Token::Keyword(match r.fragment() {
                    &"fn" => Keyword::Fn,
                    &"let" => Keyword::Let,
                    &"if" => Keyword::If,
                    &"else" => Keyword::Else,
                    &"return" => Keyword::Return,
                    &"while" => Keyword::While,
                    &"for" => Keyword::For,
                    &"in" => Keyword::In,
                    &"break" => Keyword::Break,
                    &"loop" => Keyword::Loop,
                    &"continue" => Keyword::Continue,
                    &"mod" => Keyword::Mod,
                    &"struct" => Keyword::Struct,
                    &"type" => Keyword::Type,
                    &"root" => Keyword::Root,
                    &"self" => Keyword::Self_,
                    &"super" => Keyword::Super,
                    &"use" => Keyword::Use,
                    &"as" => Keyword::As,
                    &"impl" => Keyword::Impl,
                    _ => unreachable!(),
                }),
                r,
            )
        },
    )(s)
}

pub fn integer(s: Span) -> IResult<Span, SpannedToken> {
    map(
        alt((
            map(preceded(tag("0x"), hex_digit1), |r: Span| {
                (r, isize::from_str_radix(r.fragment(), 16).unwrap())
            }),
            map(preceded(tag("0o"), oct_digit1), |r: Span| {
                (r, isize::from_str_radix(r.fragment(), 8).unwrap())
            }),
            map(digit1, |r: Span| {
                (r, r.fragment().parse::<isize>().unwrap())
            }),
        )),
        |(r, n)| SpannedToken::new(Token::Literal(Literal::Int(n)), r),
    )(s)
}

pub fn float(s: Span) -> IResult<Span, SpannedToken> {
    map(
        separated_pair(digit1, tag("."), digit0),
        |(m, d): (Span, Span)| {
            let r = format!("{}.{}", m.fragment(), d.fragment())
                .parse::<f64>()
                .unwrap();
            SpannedToken::new(Token::Literal(Literal::Float(r)), s)
        },
    )(s)
}

pub fn boolean(s: Span) -> IResult<Span, SpannedToken> {
    map(alt((tag("true"), tag("false"))), |r: Span| {
        SpannedToken::new(
            Token::Literal(Literal::Bool(match r.fragment() {
                &"true" => true,
                &"false" => false,
                _ => unreachable!(),
            })),
            r,
        )
    })(s)
}

pub fn string(s: Span) -> IResult<Span, SpannedToken> {
    map(
        delimited(
            tag("\""),
            many0(alt((
                map(
                    preceded(
                        tag("\\"),
                        alt((
                            map(tag("\""), |_| "\""),
                            map(tag("\\"), |_| "\\"),
                            map(tag("n"), |_| "\n"),
                            map(tag("t"), |_| "\t"),
                            map(tag("r"), |_| "\r"),
                            map(tag("0"), |_| "\0"),
                        )),
                    ),
                    |s| s.to_string(),
                ),
                map(verify(anychar, |c| *c != '\\' && *c != '"'), |c| {
                    c.to_string()
                }),
            ))),
            tag("\""),
        ),
        |r: Vec<String>| SpannedToken::new(Token::Literal(Literal::String(r.join(""))), s),
    )(s)
}

pub fn character(s: Span) -> IResult<Span, SpannedToken> {
    map(
        delimited(
            tag("'"),
            alt((
                map(
                    preceded(
                        tag("\\"),
                        alt((
                            map(tag("'"), |_| "'"),
                            map(tag("\\"), |_| "\\"),
                            map(tag("n"), |_| "\n"),
                            map(tag("t"), |_| "\t"),
                            map(tag("r"), |_| "\r"),
                            map(tag("0"), |_| "\0"),
                        )),
                    ),
                    |s| s.to_string(),
                ),
                map(verify(anychar, |c| *c != '\\' && *c != '\''), |c| {
                    c.to_string()
                }),
            )),
            tag("'"),
        ),
        |r: String| SpannedToken::new(Token::Literal(Literal::Char(r.chars().next().unwrap())), s),
    )(s)
}

pub fn literal(s: Span) -> IResult<Span, SpannedToken> {
    alt((integer, float, boolean, string, character))(s)
}

pub fn arithmetic(s: Span) -> IResult<Span, SpannedToken> {
    map(
        alt((tag("+"), tag("-"), tag("*"), tag("/"), tag("%"))),
        |r: Span| {
            SpannedToken::new(
                Token::Symbol(Symbol::Arithmetic(match r.fragment() {
                    &"+" => Arithmetic::Plus,
                    &"-" => Arithmetic::Minus,
                    &"*" => Arithmetic::Times,
                    &"/" => Arithmetic::Divide,
                    &"%" => Arithmetic::Mod,
                    _ => unreachable!(),
                })),
                r,
            )
        },
    )(s)
}

pub fn bitwise(s: Span) -> IResult<Span, SpannedToken> {
    map(alt((tag("^"), tag("&"), tag("|"), tag("~"))), |r: Span| {
        SpannedToken::new(
            Token::Symbol(Symbol::Bitwise(match r.fragment() {
                &"^" => Bitwise::Xor,
                &"&" => Bitwise::BitwiseAnd,
                &"|" => Bitwise::BitwiseOr,
                &"~" => Bitwise::BitwiseNot,
                _ => unreachable!(),
            })),
            r,
        )
    })(s)
}

pub fn logical(s: Span) -> IResult<Span, SpannedToken> {
    map(alt((tag("&&"), tag("||"))), |r: Span| {
        SpannedToken::new(
            Token::Symbol(Symbol::Logical(match r.fragment() {
                &"&&" => Logical::And,
                &"||" => Logical::Or,
                _ => unreachable!(),
            })),
            r,
        )
    })(s)
}

pub fn comparison(s: Span) -> IResult<Span, SpannedToken> {
    map(
        alt((
            tag("=="),
            tag("!="),
            tag("<="),
            tag(">="),
            tag("<"),
            tag(">"),
        )),
        |r: Span| {
            SpannedToken::new(
                Token::Symbol(Symbol::Comparison(match r.fragment() {
                    &"==" => Comparison::Equal,
                    &"!=" => Comparison::NotEqual,
                    &"<=" => Comparison::LessThanOrEqual,
                    &">=" => Comparison::GreaterThanOrEqual,
                    &"<" => Comparison::LessThan,
                    &">" => Comparison::GreaterThan,
                    _ => unreachable!(),
                })),
                r,
            )
        },
    )(s)
}

pub fn complex_assignment(s: Span) -> IResult<Span, SpannedToken> {
    map(
        alt((
            tag("+="),
            tag("-="),
            tag("*="),
            tag("/="),
            tag("%="),
            tag("^="),
            tag("&="),
            tag("|="),
        )),
        |r: Span| {
            SpannedToken::new(
                Token::Symbol(Symbol::Assignment(match r.fragment() {
                    &"+=" => Assignment::AddAssign,
                    &"-=" => Assignment::SubAssign,
                    &"*=" => Assignment::MulAssign,
                    &"/=" => Assignment::DivAssign,
                    &"%=" => Assignment::ModAssign,
                    &"^=" => Assignment::XorAssign,
                    &"&=" => Assignment::AndAssign,
                    &"|=" => Assignment::OrAssign,
                    _ => unreachable!(),
                })),
                r,
            )
        },
    )(s)
}

pub fn logical_not(s: Span) -> IResult<Span, SpannedToken> {
    map(tag("!"), |r: Span| {
        SpannedToken::new(Token::Symbol(Symbol::Logical(Logical::Not)), r)
    })(s)
}

pub fn simple_assignment(s: Span) -> IResult<Span, SpannedToken> {
    map(tag("="), |r| {
        SpannedToken::new(Token::Symbol(Symbol::Assignment(Assignment::Assign)), r)
    })(s)
}

pub fn punctuation(s: Span) -> IResult<Span, SpannedToken> {
    map(
        alt((
            tag("("),
            tag(")"),
            tag("{"),
            tag("}"),
            tag("["),
            tag("]"),
            tag(","),
            tag(";"),
            tag("::"),
            tag(":"),
        )),
        |r: Span| {
            SpannedToken::new(
                Token::Symbol(Symbol::Punctuation(match r.fragment() {
                    &"(" => Punctuation::OpenParen,
                    &")" => Punctuation::CloseParen,
                    &"{" => Punctuation::OpenBrace,
                    &"}" => Punctuation::CloseBrace,
                    &"[" => Punctuation::OpenBracket,
                    &"]" => Punctuation::CloseBracket,
                    &"<" => Punctuation::OpenAngle,
                    &">" => Punctuation::CloseAngle,
                    &"," => Punctuation::Comma,
                    &";" => Punctuation::Semicolon,
                    &"::" => Punctuation::DoubleColon,
                    &":" => Punctuation::Colon,
                    _ => unreachable!(),
                })),
                r,
            )
        },
    )(s)
}

pub fn symbol(s: Span) -> IResult<Span, SpannedToken> {
    let (rest, sym) = alt((
        comparison,
        logical,
        complex_assignment,
        bitwise,
        arithmetic,
        simple_assignment,
        punctuation,
    ))(s)?;

    Ok((rest, sym))
}

pub fn ident(input: Span) -> IResult<Span, SpannedToken> {
    use nom::character::complete::char;

    let (s, first) = alt((verify(anychar, |&c| c.is_alphabetic()), char('_')))(input)?;
    let (s, mut rest) = many0(alt((verify(anychar, |&c| c.is_alphanumeric()), char('_'))))(s)?;
    rest.insert(0, first);
    return Ok((
        s,
        SpannedToken {
            kind: Token::Ident(rest.into_iter().collect()),
            span: input,
        },
    ));
}

pub fn visibility(s: Span) -> IResult<Span, SpannedToken> {
    map(alt((tag("pub"),)), |r: Span| {
        SpannedToken::new(
            Token::Visibility(match r.fragment() {
                &"pub" => Visibility::Public,
                _ => unreachable!(),
            }),
            r,
        )
    })(s)
}

pub fn token(s: Span) -> IResult<Span, SpannedToken> {
    delimited(
        multispace0,
        alt((keyword, visibility, literal, ident, symbol)),
        multispace0,
    )(s)
}

pub fn tokenize(s: Span) -> IResult<Span, Vec<SpannedToken>> {
    many0(token)(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident() {
        let input = Span::new("_foo_bar");
        let expected = SpannedToken::new(Token::Ident("_foo_bar".to_string()), input);
        let actual = super::ident(input).unwrap().1;
        assert_eq!(expected, actual);
    }
}
