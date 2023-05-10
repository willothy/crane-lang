use anyhow::Result;
use chumsky::{
    error,
    input::{BoxedStream, Stream},
    text::whitespace,
};
use std::path::PathBuf;
use std::{fmt::Display, hash::Hash};

use chumsky::{span::Span as ChumskySpan, text::keyword};

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(k) => match k {
                Keyword::Fn => write!(f, "fn"),
                Keyword::Let => write!(f, "let"),
                Keyword::If => write!(f, "if"),
                Keyword::Else => write!(f, "else"),
                Keyword::Return => write!(f, "return"),
                Keyword::While => write!(f, "while"),
                Keyword::For => write!(f, "for"),
                Keyword::In => write!(f, "in"),
                Keyword::Break => write!(f, "break"),
                Keyword::Loop => write!(f, "loop"),
                Keyword::Continue => write!(f, "continue"),
                Keyword::Mod => write!(f, "mod"),
                Keyword::Struct => write!(f, "struct"),
                Keyword::Type => write!(f, "type"),
                Keyword::Impl => write!(f, "impl"),
                Keyword::As => write!(f, "as"),
                Keyword::Import => write!(f, "import"),
                Keyword::Super => write!(f, "super"),
                Keyword::Self_ => write!(f, "self"),
                Keyword::Root => write!(f, "root"),
                Keyword::Const => write!(f, "const"),
                Keyword::Static => write!(f, "static"),
            },
            Token::Literal(l) => match l {
                Literal::Int(i) => write!(f, "{}", i),
                Literal::Float(v) => write!(f, "{}", v),
                Literal::String(s) => write!(f, "\"{}\"", s),
                Literal::Char(c) => write!(f, "'{}'", c),
                Literal::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            },
            Token::Symbol(s) => match s {
                Symbol::Arithmetic(s) => match s {
                    Arithmetic::Plus => write!(f, "+"),
                    Arithmetic::Minus => write!(f, "-"),
                    Arithmetic::Times => write!(f, "*"),
                    Arithmetic::Divide => write!(f, "/"),
                    Arithmetic::Mod => write!(f, "%"),
                },
                Symbol::Bitwise(s) => match s {
                    Bitwise::And => write!(f, "&"),
                    Bitwise::Or => write!(f, "|"),
                    Bitwise::BitwiseNot => write!(f, "~"),
                    Bitwise::Xor => write!(f, "^"),
                    Bitwise::ShiftRight => write!(f, ">>"),
                    Bitwise::ShiftLeft => write!(f, "<<"),
                },
                Symbol::Logical(s) => match s {
                    Logical::And => write!(f, "&&"),
                    Logical::Or => write!(f, "||"),
                    Logical::Not => write!(f, "!"),
                },
                Symbol::Comparison(s) => match s {
                    Comparison::Equal => write!(f, "=="),
                    Comparison::NotEqual => write!(f, "!="),
                    Comparison::LessThan => write!(f, "<"),
                    Comparison::GreaterThan => write!(f, ">"),
                    Comparison::LessThanOrEqual => write!(f, "<="),
                    Comparison::GreaterThanOrEqual => write!(f, ">="),
                },
                Symbol::Assignment(s) => match s {
                    Assignment::Assign => write!(f, "="),
                    Assignment::AddAssign => write!(f, "+="),
                    Assignment::SubAssign => write!(f, "-="),
                    Assignment::MulAssign => write!(f, "*="),
                    Assignment::DivAssign => write!(f, "/="),
                    Assignment::ModAssign => write!(f, "%="),
                    Assignment::XorAssign => write!(f, "^="),
                    Assignment::AndAssign => write!(f, "&="),
                    Assignment::OrAssign => write!(f, "|="),
                    Assignment::ShlAssign => write!(f, "<<="),
                    Assignment::ShrAssign => write!(f, ">>="),
                },
                Symbol::Punctuation(s) => write!(
                    f,
                    "{}",
                    match s {
                        Punctuation::FatArrow => "=>",
                        Punctuation::OpenParen => "(",
                        Punctuation::CloseParen => ")",
                        Punctuation::OpenBrace => "{",
                        Punctuation::CloseBrace => "}",
                        Punctuation::OpenBracket => "[",
                        Punctuation::CloseBracket => "]",
                        Punctuation::Comma => ",",
                        Punctuation::Semicolon => ";",
                        Punctuation::Colon => ":",
                        Punctuation::DoubleColon => "::",
                        Punctuation::Question => "?",
                        Punctuation::RightArrow => "->",
                        Punctuation::Dot => ".",
                        Punctuation::Hash => "#",
                        Punctuation::Ellipsis => "...",
                        Punctuation::At => "@",
                    }
                ),
                Symbol::Visibility(s) => match s {
                    Visibility::Public => write!(f, "pub"),
                    Visibility::Private => write!(f, ""),
                },
            },
            Token::Ident(i) => write!(f, "{}", i),
            Token::Visibility(v) => write!(f, "{}", v),
            Token::Newline => write!(f, "\n"),
            Token::Primitive(p) => write!(f, "{}", p),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(v) => write!(f, "\"{}\"", v),
            Literal::Int(v) => write!(f, "{}", v),
            Literal::Float(v) => write!(f, "{}", v),
            Literal::Char(v) => write!(f, "{}", v),
            Literal::Bool(v) => write!(f, "{}", v),
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Fn => write!(f, "fn"),
            Keyword::Let => write!(f, "let"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Return => write!(f, "return"),
            Keyword::While => write!(f, "while"),
            Keyword::For => write!(f, "for"),
            Keyword::In => write!(f, "in"),
            Keyword::Break => write!(f, "break"),
            Keyword::Loop => write!(f, "loop"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Mod => write!(f, "mod"),
            Keyword::Struct => write!(f, "struct"),
            Keyword::Type => write!(f, "type"),
            Keyword::Impl => write!(f, "impl"),
            Keyword::As => write!(f, "as"),
            Keyword::Import => write!(f, "import"),
            Keyword::Super => write!(f, "super"),
            Keyword::Self_ => write!(f, "self"),
            Keyword::Root => write!(f, "root"),
            Keyword::Const => write!(f, "const"),
            Keyword::Static => write!(f, "static"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SourceFile {
    pub path: PathBuf,
}

#[derive(Debug, PartialEq, Clone, Hash)]
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
    Import,
    Super,
    Self_,
    Root,
    Const,
    Static,
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

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Bool,
    Char,
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::U8 => write!(f, "u8"),
            Primitive::U16 => write!(f, "u16"),
            Primitive::U32 => write!(f, "u32"),
            Primitive::U64 => write!(f, "u64"),
            Primitive::U128 => write!(f, "u128"),
            Primitive::I8 => write!(f, "i8"),
            Primitive::I16 => write!(f, "i16"),
            Primitive::I32 => write!(f, "i32"),
            Primitive::I64 => write!(f, "i64"),
            Primitive::I128 => write!(f, "i128"),
            Primitive::F32 => write!(f, "f32"),
            Primitive::F64 => write!(f, "f64"),
            Primitive::Bool => write!(f, "bool"),
            Primitive::Char => write!(f, "char"),
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Comparison {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Logical {
    And,
    Or,
    Not,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Bitwise {
    And,
    Or,
    BitwiseNot,
    Xor,
    ShiftRight,
    ShiftLeft,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Arithmetic {
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    RightArrow,
    FatArrow,
    Dot,
    Hash,
    Ellipsis,
    At,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Visibility {
    Public,
    Private,
}

impl std::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => write!(f, "pub "),
            Visibility::Private => write!(f, ""),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Symbol {
    Arithmetic(Arithmetic),
    Bitwise(Bitwise),
    Logical(Logical),
    Comparison(Comparison),
    Assignment(Assignment),
    Punctuation(Punctuation),
    Visibility(Visibility),
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Token {
    // Keywords
    Keyword(Keyword),
    Literal(Literal),
    Symbol(Symbol),
    Ident(String),
    Visibility(Visibility),
    Newline,
    Primitive(Primitive),
}

impl Token {
    pub fn same_kind(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Keyword(l), Token::Keyword(r)) => l == r,
            (Token::Literal(l), Token::Literal(r)) => matches!(
                (l, r),
                (Literal::Int(_), Literal::Int(_))
                    | (Literal::Float(_), Literal::Float(_))
                    | (Literal::String(_), Literal::String(_))
                    | (Literal::Char(_), Literal::Char(_))
                    | (Literal::Bool(_), Literal::Bool(_))
            ),
            (Token::Symbol(l), Token::Symbol(r)) => l == r,
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Visibility(_), Token::Visibility(_)) => true,
            _ => false,
        }
    }

    pub fn is_unary_op(&self) -> bool {
        match self {
            Token::Symbol(Symbol::Logical(Logical::Not)) => true,
            Token::Symbol(Symbol::Bitwise(Bitwise::BitwiseNot)) => true,
            Token::Symbol(Symbol::Arithmetic(Arithmetic::Minus)) => true,
            // Deref
            Token::Symbol(Symbol::Arithmetic(Arithmetic::Times)) => true,
            // Ref
            Token::Symbol(Symbol::Bitwise(Bitwise::And)) => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> u8 {
        use self::Arithmetic::*;
        use self::Bitwise::*;
        use self::Comparison::*;
        use self::Symbol::*;
        use Token::Symbol;
        match self {
            Symbol(Assignment(_)) => 0,
            // Comparison operators have the highest precedence
            Symbol(Comparison(Equal))
            | Symbol(Comparison(NotEqual))
            | Symbol(Comparison(LessThan))
            | Symbol(Comparison(GreaterThan))
            | Symbol(Comparison(LessThanOrEqual))
            | Symbol(Comparison(GreaterThanOrEqual)) => 1,
            // Logical operators have high precedence but lower than comparison
            Symbol(Logical(self::Logical::And)) | Symbol(Logical(self::Logical::Or)) => 2,
            // Arithmetic operators have medium precedence
            Symbol(Arithmetic(Plus)) | Symbol(Arithmetic(Minus)) => 3,
            // Multiplicative operators have higher precedence than additive ones
            Symbol(Arithmetic(Times)) | Symbol(Arithmetic(Divide)) | Symbol(Arithmetic(Mod)) => 4,
            // Bitwise operators have lower precedence than arithmetic ones
            Symbol(Bitwise(And)) | Symbol(Bitwise(Xor)) | Symbol(Bitwise(Or)) => 5,
            // Bit-shift operators have lower precedence than bitwise ones
            Symbol(Bitwise(ShiftLeft)) | Symbol(Bitwise(ShiftRight)) => 6,
            // Parentheses and other operators not listed have the lowest precedence
            _ => 7,
        }
    }

    pub fn is_binary_op(&self) -> bool {
        match self {
            Token::Symbol(Symbol::Arithmetic(_)) => true,
            Token::Symbol(Symbol::Bitwise(_)) => true,
            Token::Symbol(Symbol::Comparison(_)) => true,
            Token::Symbol(Symbol::Logical(_)) => true,
            // Token::Symbol(Symbol::Assignment(_)) => true,
            _ => false,
        }
    }

    pub fn is_assignment_op(&self) -> bool {
        matches!(self, Token::Symbol(Symbol::Assignment(_)))
    }

    pub fn op_type(&self) -> Option<OperatorType> {
        self.try_into().ok()
    }
}

impl TryFrom<&Token> for OperatorType {
    type Error = anyhow::Error;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Symbol(Symbol::Assignment(_)) => Ok(OperatorType::Assignment),
            Token::Symbol(Symbol::Arithmetic(Arithmetic::Plus | Arithmetic::Minus)) => {
                Ok(OperatorType::Additive)
            }
            Token::Symbol(Symbol::Arithmetic(
                Arithmetic::Times | Arithmetic::Divide | Arithmetic::Mod,
            )) => Ok(OperatorType::Multiplicative),
            Token::Symbol(Symbol::Comparison(Comparison::Equal | Comparison::NotEqual)) => {
                Ok(OperatorType::Equality)
            }
            Token::Symbol(Symbol::Comparison(
                Comparison::LessThan
                | Comparison::GreaterThan
                | Comparison::LessThanOrEqual
                | Comparison::GreaterThanOrEqual,
            )) => Ok(OperatorType::Relational),
            Token::Symbol(Symbol::Logical(Logical::And)) => Ok(OperatorType::LogicalAnd),
            Token::Symbol(Symbol::Logical(Logical::Or)) => Ok(OperatorType::LogicalOr),
            Token::Symbol(Symbol::Logical(Logical::Not)) => Ok(OperatorType::LogicalNot),
            Token::Symbol(Symbol::Bitwise(_)) => Ok(OperatorType::Bitwise),
            Token::Keyword(Keyword::As) => Ok(OperatorType::Cast),
            igl => Err(anyhow::anyhow!("Token is not an operator: {:?}", igl)),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OperatorType {
    Assignment,
    Additive,
    Multiplicative,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Equality,
    Relational,
    Bitwise,
    Cast,
}

impl TryFrom<&str> for Keyword {
    fn try_from(value: &str) -> Result<Self, ()> {
        Ok(match value {
            "fn" => Keyword::Fn,
            "let" => Keyword::Let,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "return" => Keyword::Return,
            "while" => Keyword::While,
            "for" => Keyword::For,
            "in" => Keyword::In,
            "break" => Keyword::Break,
            "loop" => Keyword::Loop,
            "continue" => Keyword::Continue,
            "mod" => Keyword::Mod,
            "struct" => Keyword::Struct,
            "type" => Keyword::Type,
            "root" => Keyword::Root,
            "self" => Keyword::Self_,
            "super" => Keyword::Super,
            "import" => Keyword::Import,
            "as" => Keyword::As,
            "impl" => Keyword::Impl,
            "const" => Keyword::Const,
            "static" => Keyword::Static,
            _ => Err(())?,
        })
    }

    type Error = ();
}

use std::{
    ops::Range,
    path::Path,
    sync::{Arc, RwLock},
};

use ariadne::FileCache;
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    source: Arc<PathBuf>,
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(source: Arc<PathBuf>, start: usize, end: usize) -> Self {
        Span { source, start, end }
    }

    pub fn from_range(source: Arc<PathBuf>, range: Range<usize>) -> Self {
        Span {
            source,
            start: range.start,
            end: range.end,
        }
    }

    pub fn source(&self) -> Arc<PathBuf> {
        self.source.clone()
    }
}

impl chumsky::span::Span for Span {
    type Context = Arc<PathBuf>;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Span {
            source: context,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        self.source.clone()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl ariadne::Span for Span {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        self.source.as_ref()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T, S> {
    pub span: S,
    pub value: T,
}

impl<T, S> Spanned<T, S> {
    pub fn split(self) -> (T, S) {
        (self.value, self.span)
    }
}

pub trait SplitSpanned<T, S> {
    fn split_spanned(self) -> (Vec<T>, Vec<S>);
}

impl<T, S> SplitSpanned<T, S> for Vec<Spanned<T, S>> {
    fn split_spanned(self) -> (Vec<T>, Vec<S>) {
        self.into_iter().map(|s| (s.value, s.span)).unzip()
    }
}

pub trait IntoStream<'a, I> {
    fn into_stream(self) -> BoxedStream<'a, I>;
}

impl<'a, T: 'a> IntoStream<'a, T> for Vec<T> {
    fn into_stream(self) -> BoxedStream<'a, T> {
        let iter: Box<dyn Iterator<Item = _>> = Box::new(self.into_iter());
        let stream = Stream::from_iter(iter);
        let boxed = BoxedStream::boxed(stream);
        boxed
    }
}

pub type Sources = Arc<RwLock<FileCache>>;

pub type LexerError<'src> = error::Rich<'src, char>; //error::Simple<'src, char>; //

pub type LexerCtx = ();
pub type LexerExtra<'src> = extra::Full<LexerError<'src>, LexerState, LexerCtx>;

pub type LexerResult<'src> = ParseResult<Vec<Spanned<Token, Span>>, LexerError<'src>>;

pub struct LexerState {
    source_id: Arc<PathBuf>,
}

impl LexerState {
    pub fn new(source_id: Arc<PathBuf>) -> Self {
        LexerState { source_id }
    }
}

pub fn lex<'src>(source: &'src str, source_id: impl AsRef<str>) -> Result<LexerResult<'src>> {
    let mut state = LexerState::new(Arc::new(PathBuf::from(source_id.as_ref())));
    Ok(lexer().parse_with_state(source, &mut state))
}

pub fn kw<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    choice((
        keyword("fn").map(|_| Keyword::Fn),
        keyword("let").map(|_| Keyword::Let),
        keyword("if").map(|_| Keyword::If),
        keyword("else").map(|_| Keyword::Else),
        keyword("return").map(|_| Keyword::Return),
        keyword("while").map(|_| Keyword::While),
        keyword("for").map(|_| Keyword::For),
        keyword("in").map(|_| Keyword::In),
        keyword("break").map(|_| Keyword::Break),
        keyword("loop").map(|_| Keyword::Loop),
        keyword("continue").map(|_| Keyword::Continue),
        keyword("mod").map(|_| Keyword::Mod),
        keyword("struct").map(|_| Keyword::Struct),
        keyword("type").map(|_| Keyword::Type),
        keyword("root").map(|_| Keyword::Root),
        keyword("self").map(|_| Keyword::Self_),
        keyword("super").map(|_| Keyword::Super),
        keyword("import").map(|_| Keyword::Import),
        keyword("as").map(|_| Keyword::As),
        keyword("impl").map(|_| Keyword::Impl),
        keyword("const").map(|_| Keyword::Const),
        keyword("static").map(|_| Keyword::Static),
    ))
    .map_with_state(
        move |kw: Keyword, span: SimpleSpan, state: &mut LexerState| Spanned {
            span: Span {
                source: state.source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Keyword(kw),
        },
    )
}

pub fn vis<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    keyword("pub").map_with_state(move |_, span: SimpleSpan, state: &mut LexerState| Spanned {
        span: Span {
            source: state.source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Visibility(Visibility::Public),
    })
}

pub fn bool_literal<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>>
{
    choice((
        keyword("true").map(|_| Literal::Bool(true)),
        keyword("false").map(|_| Literal::Bool(false)),
    ))
    .map_with_state(
        move |lit: Literal, span: SimpleSpan, state: &mut LexerState| Spanned {
            span: Span {
                source: state.source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(lit),
        },
    )
}

pub fn punctuation<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    choice((
        just("{").map(|_| Punctuation::OpenBrace),
        just("}").map(|_| Punctuation::CloseBrace),
        just("[").map(|_| Punctuation::OpenBracket),
        just("]").map(|_| Punctuation::CloseBracket),
        just("(").map(|_| Punctuation::OpenParen),
        just(")").map(|_| Punctuation::CloseParen),
        just("#").map(|_| Punctuation::Hash),
        just("@").map(|_| Punctuation::At),
        just(";").map(|_| Punctuation::Semicolon),
        just(",").map(|_| Punctuation::Comma),
        just("?").map(|_| Punctuation::Question),
    ))
    .map_with_state(
        move |x: Punctuation, span: SimpleSpan, state: &mut LexerState| Spanned {
            span: Span {
                source: state.source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Symbol(Symbol::Punctuation(x)),
        },
    )
}

pub fn ident<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    text::ident().map_with_state(
        move |ident: &'src str, span: SimpleSpan, state: &mut LexerState| Spanned {
            span: Span {
                source: state.source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Ident(ident.to_owned()),
        },
    )
}

pub fn symbol<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    choice((
        choice((
            just("=>").map(|_| Symbol::Punctuation(Punctuation::FatArrow)),
            just("->").map(|_| Symbol::Punctuation(Punctuation::RightArrow)),
            just("==").map(|_| Symbol::Comparison(Comparison::Equal)),
            just("!=").map(|_| Symbol::Comparison(Comparison::NotEqual)),
            just("&&").map(|_| Symbol::Logical(Logical::And)),
            just("||").map(|_| Symbol::Logical(Logical::Or)),
            just("+=").map(|_| Symbol::Assignment(Assignment::AddAssign)),
            just("-=").map(|_| Symbol::Assignment(Assignment::SubAssign)),
            just("*=").map(|_| Symbol::Assignment(Assignment::MulAssign)),
            just("/=").map(|_| Symbol::Assignment(Assignment::DivAssign)),
            just("%=").map(|_| Symbol::Assignment(Assignment::ModAssign)),
            just("|=").map(|_| Symbol::Assignment(Assignment::OrAssign)),
            just("^=").map(|_| Symbol::Assignment(Assignment::XorAssign)),
            just("<<=").map(|_| Symbol::Assignment(Assignment::ShlAssign)),
            just(">>=").map(|_| Symbol::Assignment(Assignment::ShrAssign)),
        )),
        choice((
            just("<<").map(|_| Symbol::Bitwise(Bitwise::ShiftLeft)),
            just(">>").map(|_| Symbol::Bitwise(Bitwise::ShiftRight)),
            just("<=").map(|_| Symbol::Comparison(Comparison::LessThanOrEqual)),
            just(">=").map(|_| Symbol::Comparison(Comparison::GreaterThanOrEqual)),
            just("=").map(|_| Symbol::Assignment(Assignment::Assign)),
            just("+").map(|_| Symbol::Arithmetic(Arithmetic::Plus)),
            just("-").map(|_| Symbol::Arithmetic(Arithmetic::Minus)),
            just("*").map(|_| Symbol::Arithmetic(Arithmetic::Times)),
            just("/").map(|_| Symbol::Arithmetic(Arithmetic::Divide)),
            just("!").map(|_| Symbol::Logical(Logical::Not)),
            just("<").map(|_| Symbol::Comparison(Comparison::LessThan)),
            just(">").map(|_| Symbol::Comparison(Comparison::GreaterThan)),
            just("&").map(|_| Symbol::Bitwise(Bitwise::And)),
            just("|").map(|_| Symbol::Bitwise(Bitwise::Or)),
            just("^").map(|_| Symbol::Bitwise(Bitwise::Xor)),
            just("...").map(|_| Symbol::Punctuation(Punctuation::Ellipsis)),
            just(".").map(|_| Symbol::Punctuation(Punctuation::Dot)),
            just("::").map(|_| Symbol::Punctuation(Punctuation::DoubleColon)),
            just(":").map(|_| Symbol::Punctuation(Punctuation::Colon)),
        )),
    ))
    .map_with_state(
        move |sym, span: SimpleSpan, state: &mut LexerState| Spanned {
            span: Span {
                source: state.source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Symbol(sym),
        },
    )
}

pub fn int_literal<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    text::int(10).map_with_state(
        move |num: &'src str, span: SimpleSpan, state: &mut LexerState| Spanned {
            span: Span {
                source: state.source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::Int(num.parse().unwrap())),
        },
    )
}

pub fn float_literal<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>>
{
    text::int(10)
        .then_ignore(just('.'))
        .then(text::int(10).or_not())
        .map(move |(a, b)| format!("{}.{}", a, b.unwrap_or("")))
        .map_with_state(
            move |num: String, span: SimpleSpan, state: &mut LexerState| Spanned {
                span: Span {
                    source: state.source_id.clone(),
                    start: span.start(),
                    end: span.end(),
                },
                value: Token::Literal(Literal::Float(num.parse().unwrap())),
            },
        )
}

pub fn str_literal<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    just("\"")
        .then(any().repeated())
        .then_ignore(just("\""))
        .map_with_state(
            move |(s, _), span: SimpleSpan, state: &mut LexerState| Spanned {
                span: Span {
                    source: state.source_id.clone(),
                    start: span.start(),
                    end: span.end(),
                },
                value: Token::Literal(Literal::String(s.to_owned())),
            },
        )
}

pub fn char_literal<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>>
{
    just('\'')
        .ignore_then(none_of('\''))
        .then_ignore(just('\''))
        .map_with_state(
            move |s: char, span: SimpleSpan, state: &mut LexerState| Spanned {
                span: Span {
                    source: state.source_id.clone(),
                    start: span.start(),
                    end: span.end(),
                },
                value: Token::Literal(Literal::Char(s)),
            },
        )
}

pub fn primitive<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    choice((
        just("bool").map(|_| Token::Primitive(Primitive::Bool)),
        just("char").map(|_| Token::Primitive(Primitive::Char)),
        just("f32").map(|_| Token::Primitive(Primitive::F32)),
        just("f64").map(|_| Token::Primitive(Primitive::F64)),
        just("i8").map(|_| Token::Primitive(Primitive::I8)),
        just("i16").map(|_| Token::Primitive(Primitive::I16)),
        just("i32").map(|_| Token::Primitive(Primitive::I32)),
        just("i64").map(|_| Token::Primitive(Primitive::I64)),
        just("u8").map(|_| Token::Primitive(Primitive::U8)),
        just("u16").map(|_| Token::Primitive(Primitive::U16)),
        just("u32").map(|_| Token::Primitive(Primitive::U32)),
        just("u64").map(|_| Token::Primitive(Primitive::U64)),
    ))
    .map_with_state(|value, span: SimpleSpan, state: &mut LexerState| Spanned {
        value,
        span: Span::new(state.source_id.clone(), span.start(), span.end()),
    })
}

pub fn token<'src>() -> impl Parser<'src, &'src str, Spanned<Token, Span>, LexerExtra<'src>> {
    choice((
        primitive(),
        punctuation(),
        vis(),
        kw(),
        symbol(),
        ident(),
        bool_literal(),
        str_literal(),
        char_literal(),
        float_literal(),
        int_literal(),
    ))
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token, Span>>, LexerExtra<'src>> {
    let comment = just("#").then(none_of('\n').repeated()).padded();
    token()
        .padded_by(whitespace().or(comment.repeated()))
        // .padded()
        // .padded_by(whitespace().or(comment.repeated()))
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .repeated()
        .collect::<Vec<Spanned<Token, Span>>>()
        // Run all the way to end
        .then_ignore(end())
}
