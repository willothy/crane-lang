use anyhow::Result;
use chumsky::input::{BoxedStream, Stream};
use std::hash::Hash;
use std::path::PathBuf;

use chumsky::{span::Span as ChumskySpan, text::keyword};

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
            "use" => Keyword::Use,
            "as" => Keyword::As,
            "impl" => Keyword::Impl,
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

use ariadne::{Cache, FileCache};
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
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn split(self) -> (T, Span) {
        (self.value, self.span)
    }
}

pub trait SplitSpanned<T> {
    fn split_spanned(self) -> Vec<(T, Span)>;
}

impl SplitSpanned<Token> for Vec<Spanned<Token>> {
    fn split_spanned(self) -> Vec<(Token, Span)> {
        self.into_iter()
            .map(|t| -> (Token, Span) { t.split() })
            .collect()
    }
}

pub trait IntoStream<I> {
    fn into_stream<'a>(self) -> BoxedStream<'a, I>;
}

impl IntoStream<(Token, Span)> for Vec<Spanned<Token>> {
    fn into_stream<'a>(self) -> BoxedStream<'a, (Token, Span)> {
        let iter: Box<dyn Iterator<Item = _>> = Box::new(self.split_spanned().into_iter());
        let stream = Stream::from_iter(iter);
        let boxed = BoxedStream::boxed(stream);
        boxed
    }
}

pub type Sources = Arc<RwLock<FileCache>>;

pub type LexerOutput<'a> = ParseResult<Vec<Spanned<Token>>, EmptyErr>;

pub fn lex_str(source: &str, source_id: impl AsRef<str>) -> Result<LexerOutput> {
    let s = source.to_string();
    lex(&s, Arc::new(PathBuf::from(source_id.as_ref())))
}

pub fn lex_file(files: Sources, path: &Path) -> Result<LexerOutput, anyhow::Error> {
    let path = path.canonicalize().map_err(|e| anyhow::anyhow!("{}", e))?;
    let mut files = files
        .write()
        .map_err(|_| anyhow::anyhow!("Failed to lock file cache"))?;
    let source = files
        .fetch(path.as_ref())
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;
    let s = source.chars().collect();
    lex(&s, Arc::new(path))
}

pub fn kw2<'src>() -> impl Parser<'src, &'src str, Keyword> {
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
        keyword("use").map(|_| Keyword::Use),
        keyword("as").map(|_| Keyword::As),
        keyword("impl").map(|_| Keyword::Impl),
    ))
}

pub fn kw<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    let source_id = source_id.clone();
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
        keyword("use").map(|_| Keyword::Use),
        keyword("as").map(|_| Keyword::As),
        keyword("impl").map(|_| Keyword::Impl),
    ))
    .map_with_span(move |kw: Keyword, span: SimpleSpan| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Keyword(kw),
    })
}

pub fn vis<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    let source_id = source_id.clone();
    keyword("pub").map_with_span(move |_, span: SimpleSpan| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Visibility(Visibility::Public),
    })
}

pub fn bool_literal<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    choice((
        keyword("true").map(|_| Literal::Bool(true)),
        keyword("false").map(|_| Literal::Bool(false)),
    ))
    .map_with_span(move |lit: Literal, span: SimpleSpan| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Literal(lit),
    })
}

pub fn punctuation<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    let source_id = source_id.clone();
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
    .map_with_span(move |x: Punctuation, span: SimpleSpan| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Symbol(Symbol::Punctuation(x)),
    })
}

pub fn ident<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    text::ident().map_with_span(move |ident: &'src str, span: SimpleSpan| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Ident(ident.to_owned()),
    })
}

pub fn symbol<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    let source_id = source_id.clone();

    choice((
        choice((
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
    .map_with_span(move |sym, span: SimpleSpan| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Symbol(sym),
    })
}

pub fn int_literal<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    let source_id = source_id.clone();
    text::int(10)
        .map_with_span(move |num: &'src str, span: SimpleSpan| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::Int(num.parse().unwrap())),
        })
        .then_ignore(any().filter(|c: &char| c.is_whitespace()))
}

pub fn float_literal<'src>(
    source_id: Arc<PathBuf>,
) -> impl Parser<'src, &'src str, Spanned<Token>> {
    let source_id = source_id.clone();
    text::int(10)
        .then_ignore(just('.'))
        .then(text::int(10).or_not())
        .map(move |(a, b)| format!("{}.{}", a, b.unwrap_or("")))
        .map_with_span(move |num: String, span: SimpleSpan| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::Float(num.parse().unwrap())),
        })
}

pub fn str_literal<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    let source_id = source_id.clone();
    just("\"")
        .then(any().repeated())
        .then_ignore(just("\""))
        .map_with_span(move |(s, _), span: SimpleSpan| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::String(s.to_owned())),
        })
}

pub fn char_literal<'src>(source_id: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    just('\'')
        .ignore_then(none_of('\''))
        .then_ignore(just('\''))
        .map_with_span(move |s: char, span: SimpleSpan| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::Char(s)),
        })
}

pub fn token<'src>(s: Arc<PathBuf>) -> impl Parser<'src, &'src str, Spanned<Token>> {
    choice((
        vis(s.clone()),
        kw(s.clone()),
        punctuation(s.clone()),
        symbol(s.clone()),
        ident(s.clone()),
        bool_literal(s.clone()),
        str_literal(s.clone()),
        char_literal(s.clone()),
        float_literal(s.clone()),
        int_literal(s.clone()),
    ))
}

pub fn lexer<'src>(s: Arc<PathBuf>) -> impl Parser<'src, &'src str, Vec<Spanned<Token>>> {
    let comment = just("//").then(none_of('\n').repeated()).padded();
    token(s)
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .repeated()
        .collect::<Vec<Spanned<Token>>>()
        // Run all the way to end
        .then_ignore(end())
}

pub fn lex<'src>(
    source: &'src String,
    source_id: Arc<PathBuf>,
) -> Result<ParseResult<Vec<Spanned<Token>>, EmptyErr>> {
    Ok(lexer(source_id).parse(source)) //.parse_recovery_verbose(source))
}
