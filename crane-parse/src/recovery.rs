use chumsky::{
    primitive::any,
    recovery::{nested_delimiters, via_parser, Strategy},
    Parser,
};

use crate::{punc, unit::NodeId, ParserState, ParserStream};

pub struct Fallback;

impl Fallback {
    pub fn via_parens<'src>(
    ) -> impl Strategy<'src, ParserStream<'src>, NodeId, crate::ParserExtra<'src>> {
        via_parser(nested_delimiters(
            punc!(@OpenParen),
            punc!(@CloseParen),
            [
                (punc!(@OpenBracket), punc!(@CloseBracket)),
                (punc!(@OpenBrace), punc!(@CloseBrace)),
            ],
            |_span| NodeId::default(),
        ))
    }

    pub fn via_braces<'src>(
    ) -> impl Strategy<'src, ParserStream<'src>, NodeId, crate::ParserExtra<'src>> {
        via_parser(nested_delimiters(
            punc!(@OpenBrace),
            punc!(@CloseBrace),
            [
                (punc!(@OpenParen), punc!(@CloseParen)),
                (punc!(@OpenBracket), punc!(@CloseBracket)),
            ],
            |_span| NodeId::default(),
        ))
    }

    pub fn via_brackets<'src>(
    ) -> impl Strategy<'src, ParserStream<'src>, NodeId, crate::ParserExtra<'src>> {
        via_parser(nested_delimiters(
            punc!(@OpenBracket),
            punc!(@CloseBracket),
            [
                (punc!(@OpenParen), punc!(@CloseParen)),
                (punc!(@OpenBrace), punc!(@CloseBrace)),
            ],
            |_span| NodeId::default(),
        ))
    }

    pub fn via_semicolon<'src>(// p: impl Parser<'src, ParserStream<'src>, NodeId, crate::ParserExtra<'src>>,
    ) -> impl Strategy<'src, ParserStream<'src>, NodeId, crate::ParserExtra<'src>> {
        // skip_until(any().ignored(), punc!(Semicolon).ignored(), || {
        //     NodeId::default()
        // })
        via_parser(
            any().then_ignore(punc!(Semicolon).rewind()).map_with_state(
                |_, _, state: &mut ParserState| state.current_unit_mut().new_error(),
            ),
        )
    }

    pub fn error<'src>() -> impl Strategy<'src, ParserStream<'src>, NodeId, crate::ParserExtra<'src>>
    {
        via_parser(
            any().map_with_state(|_, _, state: &mut ParserState| {
                state.current_unit_mut().new_error()
            }),
        )
    }

    pub fn via_default<'src>(
    ) -> impl Strategy<'src, ParserStream<'src>, NodeId, crate::ParserExtra<'src>> {
        via_parser(nested_delimiters(
            punc!(@OpenParen),
            punc!(@CloseParen),
            [
                (punc!(@OpenBracket), punc!(@CloseBracket)),
                (punc!(@OpenBrace), punc!(@CloseBrace)),
            ],
            |_span| NodeId::default(),
        ))
    }
}
