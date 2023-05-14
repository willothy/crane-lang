use anyhow::Result;
use chumsky::input::BoxedStream;
use chumsky::prelude::Rich;
use chumsky::primitive::{any, choice, just};
use chumsky::recursive::recursive;
use chumsky::span::SimpleSpan;
use chumsky::{select, IterParser, ParseResult, Parser};
use lex::{IntoStream, Span, SplitSpanned};
use recovery::Fallback;

use crane_lex as lex;
use crane_lex::{Keyword, Spanned, Token, Visibility};

pub mod expr;
pub mod item;
pub mod ops;
pub mod package;
pub mod path;
pub mod recovery;
pub mod ty;
pub mod unit;

use expr::Expr;
use item::Item;
use ops::{AssignOp, BinaryOp, UnaryOp};
use package::ASTPackage;
use path::{ItemPath, PathPart};
use ty::Signature;
use unit::{ASTUnit, NodeId, UnitId};

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Item(Item),
    Expr(Expr),
    Error,
}

pub type ParserError<'src> = chumsky::error::Rich<'src, Token>;

pub type ParserCtx = ();

#[derive(Debug)]
pub struct ParserState<'src> {
    pub package: &'src mut ASTPackage,
    pub unit_stack: Vec<UnitId>,
}

impl<'src> ParserState<'src> {
    fn new(package: &'src mut ASTPackage) -> ParserState<'src> {
        ParserState {
            package,
            unit_stack: Vec::new(),
        }
    }

    fn current_unit_id(&self) -> UnitId {
        *self.unit_stack.last().unwrap()
    }

    fn current_unit_mut(&mut self) -> &mut ASTUnit {
        self.package.unit_mut(self.current_unit_id()).unwrap()
    }
}

pub type ParserExtra<'src> = chumsky::extra::Full<ParserError<'src>, ParserState<'src>, ParserCtx>;

pub type ParserResult<'src> = ParseResult<UnitId, ParserError<'src>>;

pub type ParserStream<'src> = BoxedStream<'src, Token>;

#[macro_export]
macro_rules! kw {
    ($id:ident) => {
        chumsky::primitive::just(crane_lex::Token::Keyword(crane_lex::Keyword::$id))
    };
    (@$id:ident) => {
        crane_lex::Token::Keyword(crane_lex::Keyword::$id)
    };
}

#[macro_export]
macro_rules! punc {
    ($id:ident) => {
        chumsky::primitive::just(crane_lex::Token::Symbol(crane_lex::Symbol::Punctuation(
            crane_lex::Punctuation::$id,
        )))
    };
    (@$id:ident) => {
        crane_lex::Token::Symbol(crane_lex::Symbol::Punctuation(crane_lex::Punctuation::$id))
    };
}

#[macro_export]
macro_rules! math {
    ($id:ident) => {
        chumsky::primitive::just(crane_lex::Token::Symbol(crane_lex::Symbol::Arithmetic(
            crane_lex::Arithmetic::$id,
        )))
    };
    (@$id:ident) => {
        crane_lex::Token::Symbol(crane_lex::Symbol::Arithmetic(crane_lex::Arithmetic::$id))
    };
}

#[macro_export]
macro_rules! cmp {
    ($id:ident) => {
        chumsky::primitive::just(crane_lex::Token::Symbol(crane_lex::Symbol::Comparison(
            crane_lex::Comparison::$id,
        )))
    };
    (@$id:ident) => {
        crane_lex::Token::Symbol(crane_lex::Symbol::Comparison(crane_lex::Comparison::$id))
    };
}

#[macro_export]
macro_rules! assign {
    ($id:ident) => {
        chumsky::primitive::just(crane_lex::Token::Symbol(crane_lex::Symbol::Assignment(
            crane_lex::Assignment::$id,
        )))
    };
    (@$id:ident) => {
        crane_lex::Token::Symbol(crane_lex::Symbol::Assignment(crane_lex::Assignment::$id))
    };
}

#[macro_export]
macro_rules! logical {
    ($id:ident) => {
        chumsky::primitive::just(crane_lex::Token::Symbol(crane_lex::Symbol::Logical(
            crane_lex::Logical::$id,
        )))
    };
    (@$id:ident) => {
        crane_lex::Token::Symbol(crane_lex::Symbol::Logical(crane_lex::Logical::$id))
    };
}

macro_rules! bit {
    ($id:ident) => {
        chumsky::primitive::just(crane_lex::Token::Symbol(crane_lex::Symbol::Bitwise(
            crane_lex::Bitwise::$id,
        )))
    };
    (@$id:ident) => {
        crane_lex::Token::Symbol(crane_lex::Symbol::Bitwise(crane_lex::Bitwise::$id))
    };
}

fn ident_str<'src>() -> impl Parser<'src, ParserStream<'src>, String, ParserExtra<'src>> {
    any()
        .filter(|t| matches!(t, Token::Ident(_)))
        .map(|t| match t {
            Token::Ident(i) => i,
            _ => unreachable!(),
        })
}

fn params<'src>(
) -> impl Parser<'src, ParserStream<'src>, Vec<(String, Signature)>, ParserExtra<'src>> {
    ident_str()
        .then_ignore(punc!(Colon))
        .then(typename())
        .separated_by(punc!(Comma))
        .at_least(0)
        .collect()
}

fn literal<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    any()
        .filter(|t| matches!(t, Token::Literal(_)))
        .map(|t| match t {
            Token::Literal(l) => l,
            _ => unreachable!(),
        })
        .map_with_state(|literal, _span, state: &mut ParserState| {
            let unit_id = state.current_unit_id();
            let unit = state.package.unit_mut(unit_id).unwrap();
            let expr = unit.new_expr(Expr::Literal(literal));
            expr
        })
}

fn additive<'src>() -> impl Parser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        math!(@Plus) => BinaryOp::Add,
        math!(@Minus) => BinaryOp::Sub,
    }
}

fn multiplicative<'src>() -> impl Parser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        math!(@Times) => BinaryOp::Mul,
        math!(@Divide) => BinaryOp::Div,
        math!(@Mod) => BinaryOp::Mod,
    }
}

fn bitshift<'src>() -> impl Parser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        bit!(@ShiftLeft) => BinaryOp::ShiftLeft,
        bit!(@ShiftRight) => BinaryOp::ShiftRight,
    }
}

fn relational<'src>() -> impl Parser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        cmp!(@LessThan) => BinaryOp::Lt,
        cmp!(@LessThanOrEqual) => BinaryOp::Leq,
        cmp!(@GreaterThan) => BinaryOp::Gt,
        cmp!(@GreaterThanOrEqual) => BinaryOp::Geq,
    }
}

fn equality<'src>() -> impl Parser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        cmp!(@Equal) => BinaryOp::Eq,
        cmp!(@NotEqual) => BinaryOp::Neq,
    }
}

fn let_lhs_expr<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> + Clone,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    choice((
        // struct destructuring
        struct_init(expr.clone()),
        // tuple destructuring
        tuple(expr.clone()),
        // fixed-size array destructuring
        list(expr.clone()),
        // Standard name
        ident(),
    ))
}

fn r#let<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> + Clone,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    kw!(Let)
        .ignore_then(let_lhs_expr(expr.clone()))
        .then(punc!(Colon).ignore_then(typename()).or_not())
        .then(assign!(Assign).ignore_then(expr).or_not())
        .map_with_state(|((lhs, ty), value), _span, state: &mut ParserState| {
            state
                .current_unit_mut()
                .new_expr(Expr::Let { lhs, ty, value })
        })
}

fn r#continue<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    kw!(Continue).map_with_state(|_, _span, state: &mut ParserState| {
        state.current_unit_mut().new_expr(Expr::Continue)
    })
}

fn r#break<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>>,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    kw!(Break)
        .ignore_then(expr.or_not())
        .map_with_state(|value, _span, state: &mut ParserState| {
            state.current_unit_mut().new_expr(Expr::Break { value })
        })
}

fn r#return<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>>,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    kw!(Return).ignore_then(expr.or_not()).map_with_state(
        |value, _span, state: &mut ParserState| {
            state.current_unit_mut().new_expr(Expr::Return { value })
        },
    )
}

fn r#loop<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>>,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    kw!(Loop)
        .ignore_then(expr)
        .map_with_state(|body, _span, state: &mut ParserState| {
            state.current_unit_mut().new_expr(Expr::Loop { body })
        })
}

fn r#ident<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    select! { Token::Ident(ident) => ident }.map_with_state(
        |name, _span, state: &mut ParserState| state.current_unit_mut().new_expr(Expr::Ident(name)),
    )
}

fn list<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>>,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    expr.separated_by(punc!(Comma))
        .at_least(0)
        .collect::<Vec<NodeId>>()
        .delimited_by(punc!(OpenBracket), punc!(CloseBracket))
        .map_with_state(|exprs, _span, state: &mut ParserState| {
            state.current_unit_mut().new_expr(Expr::List { exprs })
        })
    // .recover_with(Fallback::via_brackets())
}

fn tuple<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>>,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    expr.separated_by(punc!(Comma))
        .at_least(1)
        .allow_trailing()
        .collect::<Vec<NodeId>>()
        .delimited_by(punc!(OpenParen), punc!(CloseParen))
        .map_with_state(|exprs, _span, state: &mut ParserState| {
            state.current_unit_mut().new_expr(Expr::Tuple { exprs })
        })
    // .recover_with(Fallback::via_parens())
}

fn closure<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> + Clone,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    kw!(Fn)
        .ignore_then(params().delimited_by(punc!(OpenParen), punc!(CloseParen)))
        .then(punc!(RightArrow).ignore_then(typename()).or_not())
        .then(
            punc!(FatArrow)
                .ignore_then(expr.clone())
                .or(block(expr.clone()).recover_with(Fallback::via_braces())),
        )
        .map_with_state(
            |((params, ret_ty), body): ((Vec<_>, Option<_>), NodeId),
             _span,
             state: &mut ParserState| {
                state.current_unit_mut().new_expr(Expr::Closure {
                    params,
                    ret_ty,
                    body,
                })
            },
        )
}

fn scope_resolution<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    path::path(1).map_with_state(|path, _span, state| {
        state
            .current_unit_mut()
            .new_expr(Expr::ScopeResolution { path })
    })
}

fn struct_init<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> + Clone,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    path::path(1)
        .or(ident_str().map(|id| ItemPath::from(vec![PathPart::Named(id)])))
        .then(
            ident_str()
                .then_ignore(punc!(Colon))
                .then(expr.clone())
                .separated_by(punc!(Comma))
                .allow_trailing()
                .collect::<Vec<(String, NodeId)>>()
                .delimited_by(punc!(OpenBrace), punc!(CloseBrace)),
        )
        .map_with_state(|(ty, fields), _span, state: &mut ParserState| {
            state
                .current_unit_mut()
                .new_expr(Expr::StructInit { ty, fields })
        })
}

fn expr<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    recursive(|expr| {
        let r#if = recursive(|r#if| {
            kw!(If)
                .ignore_then(expr.clone())
                .then(expr.clone())
                .then(kw!(Else).ignore_then(expr.clone().or(r#if)).or_not())
                .map_with_state(|((cond, then), r#else), _span, state: &mut ParserState| {
                    state
                        .current_unit_mut()
                        .new_expr(Expr::If { cond, then, r#else })
                })
        });

        let r#while = kw!(While)
            .ignore_then(expr.clone())
            .then(expr.clone())
            .map_with_state(|(cond, body), _span, state: &mut ParserState| {
                state
                    .current_unit_mut()
                    .new_expr(Expr::While { cond, body })
            });

        let atom = choice((
            literal(),
            struct_init(expr.clone()),
            scope_resolution(),
            ident(),
            block(expr.clone()),
            r#loop(expr.clone()),
            r#if,
            r#while,
            list(expr.clone()),
            closure(expr.clone()),
            tuple(expr.clone()),
            expr.clone()
                .delimited_by(punc!(OpenParen), punc!(CloseParen))
                .recover_with(Fallback::via_parens()),
        ))
        // Attempt to recover anything that looks like a parenthesised expression but contains errors
        .recover_with(Fallback::via_parens())
        // Attempt to recover anything that looks like a list but contains errors
        .recover_with(Fallback::via_brackets())
        // Attempt to recover anything that looks like a block but contains errors
        .recover_with(Fallback::via_braces())
        // .recover_with(Fallback::via_semicolon())
        .boxed();

        let items = expr
            .clone()
            .separated_by(punc!(Comma))
            .allow_trailing()
            .collect::<Vec<NodeId>>();

        let paren_list = items
            .delimited_by(punc!(OpenParen), punc!(CloseParen))
            .repeated();

        let call = atom.foldl_with_state(paren_list, |f, args, state: &mut ParserState| {
            state
                .current_unit_mut()
                .new_expr(Expr::Call { callee: f, args })
        });

        let index = expr
            .clone()
            .delimited_by(punc!(OpenBracket), punc!(CloseBracket))
            .recover_with(Fallback::via_brackets());

        let index_access = call
            .clone()
            .foldl_with_state(
                index.repeated(),
                |object, member, state: &mut ParserState| {
                    state.current_unit_mut().new_expr(Expr::MemberAccess {
                        object,
                        member,
                        computed: true,
                    })
                },
            )
            .boxed();

        let field_access = index_access
            .foldl_with_state(
                punc!(Dot).ignore_then(expr.clone()).repeated(),
                |lhs, rhs, state: &mut ParserState| {
                    state.current_unit_mut().new_expr(Expr::MemberAccess {
                        object: lhs,
                        member: rhs,
                        computed: false,
                    })
                },
            )
            .boxed();

        let unary_op = math!(Minus)
            .map(|_| UnaryOp::Neg)
            .or(logical!(Not).map(|_| UnaryOp::Not))
            .or(bit!(And).map(|_| UnaryOp::Ref))
            .or(math!(Times).map(|_| UnaryOp::Deref));

        let unary = recursive(|unary| {
            unary_op
                .then(unary)
                .map_with_state(|(op, operand), _span, state: &mut ParserState| {
                    state
                        .current_unit_mut()
                        .new_expr(Expr::UnaryOp { op, operand })
                })
                .or(field_access)
        })
        .boxed();

        let bin_parsers = [
            multiplicative().boxed(),
            additive().boxed(),
            bitshift().boxed(),
            relational().boxed(),
            equality().boxed(),
            bit!(And).to(BinaryOp::BitwiseAnd).boxed(),
            bit!(Xor).to(BinaryOp::Xor).boxed(),
            bit!(Or).to(BinaryOp::BitwiseOr).boxed(),
            logical!(And).to(BinaryOp::And).boxed(),
            logical!(Or).to(BinaryOp::Or).boxed(),
        ];

        let assignment_op = choice((
            assign!(Assign).to(AssignOp::Assign),
            assign!(AddAssign).to(AssignOp::AddAssign),
            assign!(SubAssign).to(AssignOp::SubAssign),
            assign!(MulAssign).to(AssignOp::MulAssign),
            assign!(DivAssign).to(AssignOp::DivAssign),
            assign!(ModAssign).to(AssignOp::ModAssign),
            assign!(AndAssign).to(AssignOp::AndAssign),
            assign!(OrAssign).to(AssignOp::OrAssign),
            assign!(XorAssign).to(AssignOp::XorAssign),
            assign!(ShlAssign).to(AssignOp::ShlAssign),
            assign!(ShrAssign).to(AssignOp::ShrAssign),
        ))
        .boxed();

        let assignment = unary
            .clone()
            .foldl_with_state(
                assignment_op.clone().then(unary).repeated(),
                |lhs, (op, rhs), state: &mut ParserState| {
                    state
                        .current_unit_mut()
                        .new_expr(Expr::Assignment { lhs, op, rhs })
                },
            )
            .boxed();

        let mut binary = assignment.boxed();

        for op in bin_parsers {
            binary = binary
                .clone()
                .foldl_with_state(
                    op.clone().then(binary).repeated(),
                    |lhs, (op, rhs), state: &mut ParserState| {
                        state
                            .current_unit_mut()
                            .new_expr(Expr::BinaryOp { lhs, op, rhs })
                    },
                )
                .boxed();
        }

        let cast_op = just(Token::Keyword(Keyword::As)).to(BinaryOp::Cast).boxed();
        let cast = binary
            .clone()
            .foldl_with_state(
                cast_op.then(typename()).repeated(),
                |expr, (_, ty), state: &mut ParserState| {
                    state.current_unit_mut().new_expr(Expr::Cast { expr, ty })
                },
            )
            .boxed();

        cast
    })
    .recover_with(Fallback::via_parens())
    .recover_with(Fallback::via_braces())
    .recover_with(Fallback::via_brackets())
}

fn stmt<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> + Clone,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    choice((
        r#let(expr.clone()),
        r#return(expr.clone()),
        r#continue(),
        r#break(expr.clone()),
        expr.clone(),
    ))
}

fn block<'src>(
    expr: impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> + Clone,
) -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    stmt(expr)
        .recover_with(Fallback::via_semicolon())
        .separated_by(punc!(Semicolon))
        .at_least(0)
        .collect::<Vec<NodeId>>()
        .then(punc!(Semicolon).or_not())
        .delimited_by(punc!(OpenBrace), punc!(CloseBrace))
        .map_with_state(
            |(mut exprs, result): (Vec<NodeId>, _), _span, state: &mut ParserState| {
                // If there's no trailing semicolon, the last expr should be implicitly yielded
                if let (Some(last), None) = (exprs.last_mut(), result) {
                    *last = state.current_unit_mut().make_result(*last);
                }
                state.current_unit_mut().new_expr(Expr::Block { exprs })
            },
        )
    // .recover_with(Fallback::via_braces())
}

fn vis<'src>() -> impl Parser<'src, ParserStream<'src>, Visibility, ParserExtra<'src>> {
    just(Token::Visibility(Visibility::Public))
        .or_not()
        .map(|v| {
            if v.is_some() {
                Visibility::Public
            } else {
                Visibility::Private
            }
        })
}

mod parsers {
    pub mod unit {

        use super::super::*;

        fn func_def<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
            vis()
                .then_ignore(kw!(Fn))
                .then(ident_str())
                .then(
                    params()
                        .delimited_by(punc!(OpenParen), punc!(CloseParen))
                        .map_err(|e| Rich::custom(*e.span(), "Unclosed parenthesis")),
                )
                .then(punc!(RightArrow).ignore_then(typename()).or_not())
                .then(block(expr().boxed()))
                .map_with_state(
                    |((((vis, name), params), ret_ty), body): (
                        (
                            ((Visibility, String), Vec<(String, Signature)>),
                            Option<Signature>,
                        ),
                        NodeId,
                    ),
                     _span: SimpleSpan,
                     state: &mut ParserState| {
                        state
                            .package
                            .unit_mut(state.current_unit_id().clone())
                            .unwrap()
                            .new_item(
                                name.clone(),
                                Item::FunctionDef {
                                    vis,
                                    name,
                                    params,
                                    ret_ty,
                                    body,
                                },
                            )
                    },
                )
        }

        fn type_def<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
            vis()
                .then_ignore(kw!(Type))
                .then(ident_str())
                .then_ignore(assign!(Assign))
                .then(typename())
                .map_with_state(
                    move |((vis, name), ty), _span: SimpleSpan, state: &mut ParserState| {
                        state
                            .package
                            .unit_mut(state.unit_stack.last().copied().unwrap())
                            .unwrap()
                            .new_item(name.clone(), Item::TypeDef { vis, name, ty })
                    },
                )
        }

        fn struct_def<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
            vis()
                .then_ignore(kw!(Struct))
                .then(ident_str())
                .then_ignore(punc!(OpenBrace))
                .then(params())
                .then_ignore(punc!(CloseBrace))
                .map_with_state(|((vis, name), fields), _span, state: &mut ParserState| {
                    state
                        .package
                        .unit_mut(state.current_unit_id().clone())
                        .unwrap()
                        .new_item(name.clone(), Item::StructDef { vis, name, fields })
                })
        }

        fn const_def<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
            vis()
                .then_ignore(kw!(Const))
                .then(ident_str())
                .then_ignore(punc!(Colon))
                .then(typename())
                .then_ignore(assign!(Assign))
                .then(expr())
                .map_with_state(
                    |(((vis, name), ty), value), _span: SimpleSpan, state: &mut ParserState| {
                        state
                            .package
                            .unit_mut(state.current_unit_id().clone())
                            .unwrap()
                            .new_item(
                                name.clone(),
                                Item::ConstDef {
                                    vis,
                                    ty,
                                    name,
                                    value,
                                },
                            )
                    },
                )
        }

        fn static_def<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
            vis()
                .then_ignore(kw!(Static))
                .then(ident_str())
                .then_ignore(punc!(Colon))
                .then(typename())
                .then_ignore(assign!(Assign))
                .then(expr())
                .map_with_state(
                    |(((vis, name), ty), value), _span: SimpleSpan, state: &mut ParserState| {
                        state
                            .package
                            .unit_mut(state.current_unit_id().clone())
                            .unwrap()
                            .new_item(
                                name.clone(),
                                Item::StaticDef {
                                    vis,
                                    ty,
                                    name,
                                    value,
                                },
                            )
                    },
                )
        }

        fn import<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
            vis()
                .then(kw!(Import).ignore_then(path::path(1)))
                .map_with_state(|(vis, path), _span, state: &mut ParserState| {
                    state
                        .package
                        .unit_mut(state.current_unit_id().clone())
                        .unwrap()
                        .new_item(
                            path.name().unwrap().to_owned(),
                            Item::Import {
                                vis,
                                path: path.clone(),
                            },
                        )
                })
        }

        fn item<'src>() -> impl Parser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
            type_def()
                .then_ignore(punc!(Semicolon))
                .or(import().then_ignore(punc!(Semicolon)))
                .or(func_def())
                .or(struct_def())
                .or(const_def().then_ignore(punc!(Semicolon)))
                .or(static_def().then_ignore(punc!(Semicolon)))
        }

        pub fn unit<'src>() -> impl Parser<'src, ParserStream<'src>, UnitId, ParserExtra<'src>> {
            item()
                .recover_with(Fallback::via_braces())
                .recover_with(Fallback::via_parens())
                .recover_with(Fallback::via_brackets())
                .repeated()
                .collect()
                .map_with_state(|_: Vec<NodeId>, _, state| state.unit_stack.pop().unwrap())
        }
    }
}

fn typename<'src>() -> impl Parser<'src, ParserStream<'src>, Signature, ParserExtra<'src>> {
    ty::parser()
}

pub fn parse<'src>(
    tokens: Vec<Spanned<Token, Span>>,
    package: &mut ASTPackage,
    name: String,
) -> Result<ParseResult<UnitId, ParserError>> {
    let root = ASTUnit::new(name, None);
    let mut state = ParserState::new(package);
    let unit_id = state.package.add_unit(root);
    state.unit_stack.push(unit_id);
    state.package.set_root(unit_id);

    let parser = parsers::unit::unit();
    let input = tokens.split_spanned().0.into_stream();
    Ok(parser.parse_with_state(input, &mut state))
}
