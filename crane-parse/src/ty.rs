use std::fmt::Display;

use chumsky::{
    primitive::{any, choice, just},
    recursive::recursive,
    IterParser, Parser,
};
use crane_lex::{self as lex, Arithmetic, Keyword, Primitive, Punctuation, Symbol, Token};
use lex::Literal;

use crate::{
    kw, math,
    path::{path, ItemPath},
    punc, ParserExtra, ParserStream,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Signature {
    Function {
        params: Vec<Signature>,
        ret_ty: Option<Box<Signature>>,
    },
    Primitive(Primitive),
    Pointer(Box<Signature>),
    Array(Box<Signature>, usize),
    Tuple(Vec<Signature>),
    Name(ItemPath),
}

impl Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Signature::Function { params, ret_ty } => {
                write!(f, "fn(")?;
                for (i, ty) in params.iter().enumerate() {
                    write!(f, "{}", ty)?;
                    if i != params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
                if let Some(ty) = ret_ty {
                    write!(f, " -> {}", ty)?;
                }
                Ok(())
            }
            Signature::Primitive(p) => write!(f, "{}", p),
            Signature::Pointer(ptr) => write!(f, "*{}", ptr),
            Signature::Array(t, len) => write!(f, "[{}; {}]", t, len),
            Signature::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    write!(f, "{}", ty)?;
                    if i != types.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Signature::Name(name) => write!(f, "{}", name),
        }
    }
}

pub fn parser<'src>() -> impl Parser<'src, ParserStream<'src>, Signature, ParserExtra<'src>> {
    recursive(|signature| {
        choice((
            math!(Times)
                .ignore_then(signature.clone())
                .map(|ty| Signature::Pointer(Box::new(ty)))
                .boxed(),
            any().filter(|v| matches!(v, Token::Primitive(_))).map(|v| {
                Signature::Primitive(match v {
                    Token::Primitive(p) => p,
                    _ => unreachable!(),
                })
            }),
            signature
                .clone()
                .then(
                    punc!(Semicolon).ignore_then(
                        any()
                            .filter(|v| matches!(v, Token::Literal(Literal::Int(_))))
                            .map(|v| match v {
                                Token::Literal(Literal::Int(i)) => i as usize,
                                _ => unreachable!(),
                            }),
                    ),
                )
                .delimited_by(punc!(OpenBracket), punc!(CloseBracket))
                .map(|(ty, len)| Signature::Array(Box::new(ty), len))
                .boxed(),
            signature
                .clone()
                .separated_by(punc!(Comma))
                .collect::<Vec<Signature>>()
                .delimited_by(punc!(OpenParen), punc!(CloseParen))
                .map(|types| Signature::Tuple(types))
                .boxed(),
            path(0).map(Signature::Name).boxed(),
            kw!(Fn).ignore_then(
                signature
                    .clone()
                    .separated_by(punc!(Comma))
                    .collect::<Vec<Signature>>()
                    .delimited_by(punc!(OpenParen), punc!(CloseParen))
                    .then(punc!(RightArrow).ignore_then(signature.clone()).or_not())
                    .map(|(params, ret_ty)| Signature::Function {
                        params,
                        ret_ty: ret_ty.map(Box::new),
                    })
                    .boxed(),
            ),
        ))
    })
}
