use anyhow::{Context, Result};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::error::ErrorKind;
use nom::sequence::{preceded, terminated};
use nom::IResult;
use std::io::{BufRead, BufReader, Read};
use std::mem::MaybeUninit;
use std::ptr::null_mut;

use crate::token::{Span, Symbol, Token, TokenKind};

struct Tokenizer<'a>(std::marker::PhantomData<&'a ()>);

impl<'a> Tokenizer<'a> {
    fn braces(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        alt((
            tag("("),
            tag(")"),
            tag("["),
            tag("]"),
            tag("{"),
            tag("}"),
            tag("<"),
            tag(">"),
        ))(i)
    }

    fn delimiters(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        alt((tag(":"), tag(";"), tag(","), tag(".")))(i)
    }

    fn misc(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        alt((tag("@"), tag(r"\")))(i)
    }

    pub fn symbol(i: Span<'a>) -> IResult<Span<'a>, Token> {
        let (rest, sym) = alt((
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("%"),
            tag("="),
            tag("!"),
            tag("&"),
            tag("|"),
            tag("^"),
            tag("~"),
            tag("?"),
            Self::delimiters,
            Self::braces,
            Self::misc,
        ))(i)?;
        Ok((
            rest,
            Token {
                span: sym.clone(),
                kind: TokenKind::Symbol(Symbol::try_from(sym.to_string()).unwrap()),
            },
        ))
    }

    // pub fn keyword(i: Span<'a>) -> IResult<Span<'a>, Token> {}
}
