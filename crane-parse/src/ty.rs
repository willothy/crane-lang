use crane_lex::Primitive;

use crate::path::TypeName;

#[derive(Debug, PartialEq, Clone)]
pub enum Signature {
    Function {
        params: Vec<(String, TypeName)>,
        ret_ty: Option<TypeName>,
    },
    Primitive(Primitive),
    Pointer(Box<Signature>),
    Array(Box<Signature>, usize),
    Tuple(Vec<Signature>),
}

// pub fn parse<'src>() -> impl Parser<'src, ParserStream<'src, Token>, ParserExtra> {
//     let primitive = lex::primitive();
//
//     let signature = choice!(
//         primitive.map(Signature::Primitive),
//         just(Token::Star)
//             .and(signature)
//             .map(|(_, ty)| Signature::Pointer(Box::new(ty))),
//         just(Token::OpenBracket)
//             .and(signature)
//             .and(just(Token::Semicolon))
//             .and(just(Token::CloseBracket))
//             .map(|(((ty, len), _), _)| Signature::Array(Box::new(ty), len)),
//         just(Token::OpenParen)
//             .and(signature.sep_by(just(Token::Comma)))
//             .and(just(Token::CloseParen))
//             .map(|((_, tys), _)| Signature::Tuple(tys)),
//         just(Token::Keyword(Keyword::Fn)).then(just(Token::OpenParen))
//     );
//
//     signature
// }
