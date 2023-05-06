use std::hash::Hash;
use std::path::PathBuf;

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
        match self {
            Token::Symbol(Symbol::Assignment(_)) => true,
            _ => false,
        }
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

use ariadne::{Cache, FileCache, Source};
use chumsky::{
    prelude::*,
    text::{keyword, Character},
};

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

impl chumsky::Span for Span {
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

pub type Sources = Arc<RwLock<FileCache>>;

pub fn lex_str(
    source: &str,
    source_id: impl AsRef<str>,
) -> Result<
    (
        Option<Vec<Spanned<Token>>>,
        Vec<chumsky::error::Simple<char>>,
    ),
    anyhow::Error,
> {
    let source = Source::from(source);
    lex(&source, PathBuf::from(source_id.as_ref()).as_path())
}

pub fn lex_file(
    files: Sources,
    path: &Path,
) -> Result<
    (
        Option<Vec<Spanned<Token>>>,
        Vec<chumsky::error::Simple<char>>,
    ),
    anyhow::Error,
> {
    let path = path.canonicalize().map_err(|e| anyhow::anyhow!("{}", e))?;
    let mut files = files
        .write()
        .map_err(|_| anyhow::anyhow!("Failed to lock file cache"))?;
    let source = files
        .fetch(path.as_ref())
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;
    lex(source, path.as_ref())
}

pub fn kw<'a>(source_id: &Arc<PathBuf>) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
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
    .map_with_span(move |kw: Keyword, span: Range<usize>| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Keyword(kw),
    })
}

pub fn vis(source_id: &Arc<PathBuf>) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    keyword("pub").map_with_span(move |_, span: Range<usize>| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Visibility(Visibility::Public),
    })
}

pub fn bool_literal(
    source_id: &Arc<PathBuf>,
) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    choice((
        keyword("true").map(|_| Literal::Bool(true)),
        keyword("false").map(|_| Literal::Bool(false)),
    ))
    .map_with_span(move |lit: Literal, span: Range<usize>| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Literal(lit),
    })
}

pub fn punctuation(
    source_id: &Arc<PathBuf>,
) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    one_of::<_, _, Simple<char>>("{}[]()#@;,?").map_with_span(move |x: char, span: Range<usize>| {
        Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Symbol(Symbol::Punctuation(match x {
                '{' => Punctuation::OpenBrace,
                '}' => Punctuation::CloseBrace,
                '[' => Punctuation::OpenBracket,
                ']' => Punctuation::CloseBracket,
                '(' => Punctuation::OpenParen,
                ')' => Punctuation::CloseParen,
                '#' => Punctuation::Hash,
                '@' => Punctuation::At,
                ';' => Punctuation::Semicolon,
                ',' => Punctuation::Comma,
                '?' => Punctuation::Question,
                _ => unreachable!(),
            })),
        }
    })
}

pub fn ident(source_id: &Arc<PathBuf>) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    text::ident::<char, Simple<char>>().map_with_span(move |ident, span: Range<usize>| Spanned {
        span: Span {
            source: source_id.clone(),
            start: span.start(),
            end: span.end(),
        },
        value: Token::Ident(ident),
    })
}

pub fn symbol(source_id: &Arc<PathBuf>) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    one_of::<_, _, Simple<char>>("+-*/=!<>&|^:.")
        .repeated()
        .at_least(1)
        .try_map(move |x: Vec<char>, span| {
            Ok(Spanned {
                span: Span {
                    source: source_id.clone(),
                    start: span.start(),
                    end: span.end(),
                },
                value: Token::Symbol(match x.into_iter().collect::<String>().as_str() {
                    "==" => Symbol::Comparison(Comparison::Equal),
                    "!=" => Symbol::Comparison(Comparison::NotEqual),
                    "&&" => Symbol::Logical(Logical::And),
                    "||" => Symbol::Logical(Logical::Or),
                    "+=" => Symbol::Assignment(Assignment::AddAssign),
                    "-=" => Symbol::Assignment(Assignment::SubAssign),
                    "*=" => Symbol::Assignment(Assignment::MulAssign),
                    "/=" => Symbol::Assignment(Assignment::DivAssign),
                    "%=" => Symbol::Assignment(Assignment::ModAssign),
                    "&=" => Symbol::Assignment(Assignment::AndAssign),
                    "|=" => Symbol::Assignment(Assignment::OrAssign),
                    "^=" => Symbol::Assignment(Assignment::XorAssign),
                    "<<=" => Symbol::Assignment(Assignment::ShlAssign),
                    ">>=" => Symbol::Assignment(Assignment::ShrAssign),
                    ">>" => Symbol::Bitwise(Bitwise::ShiftRight),
                    "<<" => Symbol::Bitwise(Bitwise::ShiftLeft),
                    "<=" => Symbol::Comparison(Comparison::LessThanOrEqual),
                    ">=" => Symbol::Comparison(Comparison::GreaterThanOrEqual),
                    "=" => Symbol::Assignment(Assignment::Assign),
                    "+" => Symbol::Arithmetic(Arithmetic::Plus),
                    "-" => Symbol::Arithmetic(Arithmetic::Minus),
                    "*" => Symbol::Arithmetic(Arithmetic::Times),
                    "/" => Symbol::Arithmetic(Arithmetic::Divide),
                    "!" => Symbol::Logical(Logical::Not),
                    "<" => Symbol::Comparison(Comparison::LessThan),
                    ">" => Symbol::Comparison(Comparison::GreaterThan),
                    "&" => Symbol::Bitwise(Bitwise::And),
                    "|" => Symbol::Bitwise(Bitwise::Or),
                    "^" => Symbol::Bitwise(Bitwise::Xor),
                    "..." => Symbol::Punctuation(Punctuation::Ellipsis),
                    "." => Symbol::Punctuation(Punctuation::Dot),
                    "::" => Symbol::Punctuation(Punctuation::DoubleColon),
                    ":" => Symbol::Punctuation(Punctuation::Colon),
                    igl => {
                        return Err(chumsky::error::Simple::custom(
                            span,
                            format!("Invalid symbol {}", igl),
                        ))
                    }
                }),
            })
        })
}

pub fn int_literal(
    source_id: &Arc<PathBuf>,
) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    text::int(10)
        .map_with_span(move |num: String, span: Range<usize>| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::Int(num.parse().unwrap())),
        })
        .then_ignore(filter(|c: &char| c.is_whitespace()))
}

pub fn float_literal(
    source_id: &Arc<PathBuf>,
) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    text::int(10)
        .then_ignore(just('.'))
        .then(text::int(10).or_not())
        .map(move |(a, b)| format!("{}.{}", a, b.unwrap_or("".to_owned())))
        .map_with_span(move |num: String, span: Range<usize>| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::Float(num.parse().unwrap())),
        })
}

pub fn str_literal(
    source_id: &Arc<PathBuf>,
) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map_with_span(move |s: Vec<char>, span: Range<usize>| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::String(s.into_iter().collect())),
        })
}

pub fn char_literal(
    source_id: &Arc<PathBuf>,
) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    let source_id = source_id.clone();
    just::<_, _, Simple<char>>('\'')
        .ignore_then(none_of('\''))
        .then_ignore(just('\''))
        .map_with_span(move |s: char, span: Range<usize>| Spanned {
            span: Span {
                source: source_id.clone(),
                start: span.start(),
                end: span.end(),
            },
            value: Token::Literal(Literal::Char(s)),
        })
}

pub fn token(s: &Arc<PathBuf>) -> impl Parser<char, Spanned<Token>, Error = Simple<char>> {
    choice((
        vis(s),
        kw(s),
        punctuation(s),
        symbol(s),
        ident(s),
        bool_literal(s),
        str_literal(s),
        char_literal(s),
        float_literal(s),
        int_literal(s),
    ))
}

pub fn lexer(s: &Arc<PathBuf>) -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let comment = just("//").then(none_of('\n').repeated()).padded();
    token(&s)
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .repeated()
        // Run all the way to end
        .then_ignore(end())
}

pub fn lex(
    source: &Source,
    source_id: &Path,
) -> Result<
    (
        Option<Vec<Spanned<Token>>>,
        Vec<chumsky::error::Simple<char>>,
    ),
    anyhow::Error,
> {
    let source_id = Arc::new(source_id.to_owned());

    let source = source.chars().collect::<String>();

    Ok(lexer(&source_id).parse_recovery_verbose(source))
}
