use crate::symbols;
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

pub struct Token<'a> {
    pub span: Span<'a>,
    pub kind: TokenKind,
}

pub enum TokenKind {
    Literal(Literal),
    Identifier(String),
    Keyword(Keyword),
    Symbol(Symbol),
    Whitespace(char),
}

pub enum Literal {
    Str(String),
    Char(char),
    Integer(usize),
    Float(f64),
    Bool(bool),
}

pub enum Keyword {
    Let,
    Fn,
    Loop,
    If,
    Else,
    Return,
    Break,
    Continue,
}

symbols! {
    Plus = "+",
    Minus = "-",
    Star = "*",
    Slash = "/",
    Backslash = "\\",
    At = "@",
    Percent = "%",
    Equal = "=",
    Bang = "!",
    And = "&",
    Pipe = "|",
    Caret = "^",
    Tilde = "~",
    Question = "?",
    Colon = ":",
    SemiColon = ";",
    Comma = ",",
    Period = ".",
    OpenParen = "(",
    CloseParen = ")",
    OpenBracket = "[",
    CloseBracket = "]",
    OpenBrace = "{",
    CloseBrace = "}",
    OpenAngle = "<",
    CloseAngle = ">",
}
