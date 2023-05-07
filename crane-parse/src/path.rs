use std::fmt::Display;

use chumsky::{
    input::BoxedStream,
    primitive::{choice, just},
    IterParser, Parser,
};
use crane_lex::Token;

use crate::ParserExtra;

#[derive(Debug, PartialEq)]
pub struct TypeName {
    pub path: ItemPath,
    pub ptr_depth: usize,
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.ptr_depth {
            write!(f, "*")?;
        }
        write!(f, "{}", self.path)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PathPart {
    Root,
    Self_,
    Super,
    External,
    Named(String),
}

impl<T: AsRef<str> + Into<String>> From<T> for PathPart {
    fn from(s: T) -> Self {
        match s.as_ref() {
            "root" => PathPart::Root,
            "self" => PathPart::Self_,
            "super" => PathPart::Super,
            "::" => PathPart::External,
            _ => PathPart::Named(s.into()),
        }
    }
}

impl Display for PathPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathPart::Root => write!(f, "root"),
            PathPart::Self_ => write!(f, "self"),
            PathPart::Super => write!(f, "super"),
            PathPart::External => write!(f, ""),
            PathPart::Named(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ItemPath {
    pub parts: Vec<PathPart>,
}

impl From<Vec<PathPart>> for ItemPath {
    fn from(parts: Vec<PathPart>) -> Self {
        Self { parts }
    }
}

pub fn path<'src>(
    min: usize,
) -> impl Parser<'src, BoxedStream<'src, Token>, ItemPath, ParserExtra<'src>> {
    choice((
        just(Token::Symbol(crane_lex::Symbol::Punctuation(
            crane_lex::Punctuation::DoubleColon,
        )))
        .map(|_| PathPart::External),
        just(Token::Keyword(crane_lex::Keyword::Root)).map(|_| PathPart::Root),
        just(Token::Keyword(crane_lex::Keyword::Self_)).map(|_| PathPart::Self_),
        just(Token::Keyword(crane_lex::Keyword::Super)).map(|_| PathPart::Super),
        chumsky::primitive::any()
            .filter(|t| matches!(t, Token::Ident(_)))
            .map(|t| {
                PathPart::Named(match t {
                    Token::Ident(s) => s,
                    _ => unreachable!(),
                })
            }),
    ))
    .then_ignore(
        just(Token::Symbol(crane_lex::Symbol::Punctuation(
            crane_lex::Punctuation::DoubleColon,
        )))
        .or_not(),
    )
    .then(
        choice((
            just(Token::Keyword(crane_lex::Keyword::Super)).map(|_| PathPart::Super),
            chumsky::primitive::any()
                .filter(|t| matches!(t, Token::Ident(_)))
                .map(|t| {
                    PathPart::Named(match t {
                        Token::Ident(s) => s,
                        _ => unreachable!(),
                    })
                }),
        ))
        .separated_by(just(Token::Symbol(crane_lex::Symbol::Punctuation(
            crane_lex::Punctuation::DoubleColon,
        ))))
        .at_least(min)
        .collect::<Vec<_>>(),
    )
    .map(|(first, rest)| ItemPath::from(std::iter::once(first).chain(rest).collect::<Vec<_>>()))
}

impl ItemPath {
    pub fn new() -> Self {
        Self { parts: Vec::new() }
    }

    pub fn push(&mut self, part: PathPart) {
        self.parts.push(part);
    }

    pub fn pop(&mut self) -> Option<PathPart> {
        self.parts.pop()
    }

    pub fn insert(&mut self, index: usize, part: PathPart) {
        self.parts.insert(index, part);
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn len(&self) -> usize {
        self.parts.len()
    }

    pub fn is_absolute(&self) -> bool {
        !self.parts.is_empty() && self.parts[0] == PathPart::Root
    }

    pub fn is_relative(&self) -> bool {
        !self.parts.is_empty()
            && matches!(
                self.parts[0],
                PathPart::Named(_) | PathPart::Self_ | PathPart::Super
            )
    }

    pub fn is_external(&self) -> bool {
        !self.parts.is_empty() && self.parts[0] == PathPart::Root
    }

    /// Ensure the path is valid (no root:: or self:: after start, not just root / self / super)
    pub fn validate(&self) -> anyhow::Result<()> {
        enum Kind {
            Extern,
            Absolute,
            Relative(bool),
        }
        let mut kind: Kind = Kind::Relative(false);
        self.parts.iter().enumerate().try_for_each(|(idx, p)| {
            if idx > 0 {
                if let PathPart::Root | PathPart::Self_ | PathPart::External = p {
                    return Err(anyhow::anyhow!(
                        "Invalid path: {} at position {}",
                        p,
                        idx + 1
                    ));
                }
            } else {
                kind = match p {
                    PathPart::Root => Kind::Absolute,
                    PathPart::External => Kind::Extern,
                    PathPart::Self_ => Kind::Relative(false),
                    PathPart::Super => Kind::Relative(true),
                    PathPart::Named(_) => Kind::Relative(false),
                }
            }
            match kind {
                Kind::Extern => {}
                Kind::Absolute => {}
                Kind::Relative(is_super) if idx > 0 && !is_super => {
                    if let PathPart::Super = p {
                        return Err(anyhow::anyhow!(
                            "Invalid path: {} at position {}",
                            p,
                            idx + 1
                        ));
                    }
                }
                Kind::Relative(_) => {}
            }
            Ok(())
        })
    }
}

impl Display for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, part) in self.parts.iter().enumerate() {
            if idx > 0 {
                write!(f, "::")?;
            }
            write!(f, "{}", part)?;
        }
        Ok(())
    }
}
