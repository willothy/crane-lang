use anyhow::Result;
use chumsky::input::BoxedStream;
use chumsky::primitive::{any, choice, just};
use chumsky::recovery::{nested_delimiters, via_parser};
use chumsky::recursive::recursive;
use chumsky::span::SimpleSpan;
use chumsky::{select, IterParser, ParseResult, Parser as ChumskyParser};
use lex::{Bitwise, IntoStream, Span, SplitSpanned};
use slotmap::new_key_type;

use crane_lex as lex;
use crane_lex::{
    Arithmetic, Assignment, Comparison, Keyword, Logical, Punctuation, Spanned, Symbol, Token,
    Visibility,
};

pub mod expr;
pub mod item;
pub mod ops;
pub mod package;
pub mod path;
pub mod unit;

use expr::Expr;
use item::Item;
use ops::{BinaryOp, UnaryOp};
use package::Package;
use path::TypeName;
use unit::{NodeId, Unit, UnitId};

new_key_type! {
    pub struct TypeId;
}

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
    pub package: &'src mut Package,
    pub unit_stack: Vec<UnitId>,
}

impl<'src> ParserState<'src> {
    fn new(package: &'src mut Package) -> ParserState<'src> {
        ParserState {
            package,
            unit_stack: Vec::new(),
        }
    }

    fn current_unit_id(&self) -> UnitId {
        *self.unit_stack.last().unwrap()
    }

    fn current_unit_mut(&mut self) -> &mut Unit {
        self.package.unit_mut(self.current_unit_id()).unwrap()
    }
}

pub type ParserExtra<'src> = chumsky::extra::Full<ParserError<'src>, ParserState<'src>, ParserCtx>;

pub type ParserResult<'src> = ParseResult<UnitId, ParserError<'src>>;

pub type ParserStream<'src> = BoxedStream<'src, Token>;

#[macro_export]
macro_rules! kw {
    ($id:ident) => {
        just(Token::Keyword(Keyword::$id))
    };
    (@$id:ident) => {
        Token::Keyword(Keyword::$id)
    };
}

#[macro_export]
macro_rules! punc {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Punctuation(Punctuation::$id)))
    };
    (@$id:ident) => {
        Token::Symbol(Symbol::Punctuation(Punctuation::$id))
    };
}

#[macro_export]
macro_rules! math {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Arithmetic(Arithmetic::$id)))
    };
    (@$id:ident) => {
        Token::Symbol(Symbol::Arithmetic(Arithmetic::$id))
    };
}

#[macro_export]
macro_rules! cmp {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Comparison(Comparison::$id)))
    };
    (@$id:ident) => {
        Token::Symbol(Symbol::Comparison(Comparison::$id))
    };
}

#[macro_export]
macro_rules! assign {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Assignment(Assignment::$id)))
    };
    (@$id:ident) => {
        Token::Symbol(Symbol::Assignment(Assignment::$id))
    };
}

#[macro_export]
macro_rules! logical {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Logical(Logical::$id)))
    };
    (@$id:ident) => {
        Token::Symbol(Symbol::Logical(Logical::$id))
    };
}

macro_rules! bit {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Bitwise(Bitwise::$id)))
    };
    (@$id:ident) => {
        Token::Symbol(Symbol::Bitwise(Bitwise::$id))
    };
}

fn ident_str<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, String, ParserExtra<'src>> {
    any()
        .filter(|t| matches!(t, Token::Ident(_)))
        .map(|t| match t {
            Token::Ident(i) => i,
            _ => unreachable!(),
        })
}

fn params<'src>(
) -> impl ChumskyParser<'src, ParserStream<'src>, Vec<(String, TypeName)>, ParserExtra<'src>> {
    ident_str()
        .then_ignore(punc!(Colon))
        .then(typename())
        .separated_by(punc!(Comma))
        .at_least(0)
        .collect()
}

fn literal<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
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

fn additive<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        math!(@Plus) => BinaryOp::Add,
        math!(@Minus) => BinaryOp::Sub,
    }
}

fn multiplicative<'src>(
) -> impl ChumskyParser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        math!(@Times) => BinaryOp::Mul,
        math!(@Divide) => BinaryOp::Div,
        math!(@Mod) => BinaryOp::Mod,
    }
}

fn bitshift<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        bit!(@ShiftLeft) => BinaryOp::ShiftLeft,
        bit!(@ShiftRight) => BinaryOp::ShiftRight,
    }
}

fn relational<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        cmp!(@LessThan) => BinaryOp::Lt,
        cmp!(@LessThanOrEqual) => BinaryOp::Leq,
        cmp!(@GreaterThan) => BinaryOp::Gt,
        cmp!(@GreaterThanOrEqual) => BinaryOp::Geq,
    }
}

fn equality<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, BinaryOp, ParserExtra<'src>> {
    select! {
        cmp!(@Equal) => BinaryOp::Eq,
        cmp!(@NotEqual) => BinaryOp::Neq,
    }
}

fn expr<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    just(Token::Newline).or_not().ignore_then(recursive(|expr| {
        let r#let = kw!(Let)
            .ignore_then(ident_str())
            .then_ignore(punc!(Colon))
            .then(typename())
            .then(assign!(Assign).ignore_then(expr.clone()).or_not())
            .map_with_state(|((name, ty), value), _span, state: &mut ParserState| {
                state
                    .current_unit_mut()
                    .new_expr(Expr::Let { name, ty, value })
            });

        let r#loop = kw!(Loop)
            .ignore_then(expr.clone())
            .map_with_state(|body, _span, state| {
                state.current_unit_mut().new_expr(Expr::Loop { body })
            });

        let ident = select! { Token::Ident(ident) => ident }.map_with_state(
            |name, _span, state: &mut ParserState| {
                state.current_unit_mut().new_expr(Expr::Ident(name))
            },
        );

        let list = expr
            .clone()
            .separated_by(punc!(Comma))
            .at_least(0)
            .collect::<Vec<NodeId>>()
            .delimited_by(punc!(OpenBracket), punc!(CloseBracket))
            .map_with_state(|exprs, _span, state: &mut ParserState| {
                state.current_unit_mut().new_expr(Expr::List { exprs })
            });

        let block = expr
            .clone()
            .repeated()
            .at_least(0)
            .collect::<Vec<NodeId>>()
            .delimited_by(punc!(OpenBrace), punc!(CloseBrace))
            // .recover_with(via_parser(any().map_with_state(
            //     |_, _, state: &mut ParserState| vec![state.current_unit_mut().add_error()],
            // )))
            .map_with_state(|exprs, _span, state: &mut ParserState| {
                state.current_unit_mut().new_expr(Expr::Block { exprs })
            });

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

        let scope_resolution = path::path(1).map_with_state(|path, _span, state| {
            state
                .current_unit_mut()
                .new_expr(Expr::ScopeResolution { path })
        });

        // let closure = kw!(Fn)
        //     .ignore_then(params().delimited_by(punc!(OpenParen), punc!(CloseParen)))
        //     .then(punc!(RightArrow).ignore_then(typename()).or_not())
        //     .then(
        //         bit!(Or).ignore_then(expr.clone()).or(block.clone()),
        //         // expr.clone()
        //         // .delimited_by(punc!(OpenBracket), punc!(CloseBracket))
        //     )
        //     .map_with_state(
        //         |((params, ret_ty), body): ((Vec<_>, Option<_>), NodeId),
        //          _span,
        //          state: &mut ParserState| {
        //             state.current_unit_mut().new_expr(Expr::Closure {
        //                 params,
        //                 ret_ty,
        //                 body,
        //             })
        //         },
        //     );

        let r#continue = kw!(Continue).map_with_state(|_, _span, state: &mut ParserState| {
            state.current_unit_mut().new_expr(Expr::Continue)
        });

        let r#break = kw!(Break).then(expr.clone().or_not()).map_with_state(
            |(_, value), _span, state: &mut ParserState| {
                state.current_unit_mut().new_expr(Expr::Break { value })
            },
        );

        let atom = choice((
            literal(),
            scope_resolution,
            ident,
            block,
            r#loop,
            r#let,
            r#if,
            r#while,
            r#continue,
            r#break,
            list,
            expr.clone()
                .delimited_by(punc!(OpenParen), punc!(CloseParen)),
        ))
        // Attempt to recover anything that looks like a parenthesised expression but contains errors
        .recover_with(via_parser(nested_delimiters(
            punc!(@OpenParen),
            punc!(@CloseParen),
            [
                (punc!(@OpenBracket), punc!(@CloseBracket)),
                (punc!(@OpenBrace), punc!(@CloseBrace)),
            ],
            |_span| NodeId::default(),
        )))
        // Attempt to recover anything that looks like a list but contains errors
        .recover_with(via_parser(nested_delimiters(
            punc!(@OpenBracket),
            punc!(@CloseBracket),
            [
                (punc!(@OpenParen), punc!(@CloseParen)),
                (punc!(@OpenBrace), punc!(@CloseBrace)),
            ],
            |_span| NodeId::default(),
        )))
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
                .or(call)
        });

        let bin_parsers = [
            multiplicative().boxed(),
            additive().boxed(),
            bitshift().boxed(),
            relational().boxed(),
            equality().boxed(),
            bit!(And).map(|_| BinaryOp::BitwiseAnd).boxed(),
            bit!(Xor).map(|_| BinaryOp::Xor).boxed(),
            bit!(Or).map(|_| BinaryOp::BitwiseOr).boxed(),
            logical!(And).map(|_| BinaryOp::And).boxed(),
            logical!(Or).map(|_| BinaryOp::Or).boxed(),
        ];

        let mut binary = unary.boxed();

        for parser in bin_parsers {
            binary = binary
                .clone()
                .foldl_with_state(
                    parser.then(binary).repeated(),
                    |lhs, (op, rhs), state: &mut ParserState| {
                        state
                            .current_unit_mut()
                            .new_expr(Expr::BinaryOp { lhs, op, rhs })
                    },
                )
                .boxed();
        }

        binary
    }))
}

fn block<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    punc!(OpenBrace)
        .ignore_then(expr().repeated().at_least(0).collect::<Vec<NodeId>>())
        .then_ignore(punc!(CloseBrace))
        .map_with_state(|stmts, _span, state: &mut ParserState| {
            let unit_id = state.current_unit_id();
            let unit = state.package.unit_mut(unit_id).unwrap();
            let block = unit.new_expr(Expr::Block { exprs: stmts });
            block
        })
}

fn vis<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, Visibility, ParserExtra<'src>> {
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

fn func_def<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    vis()
        .then_ignore(kw!(Fn))
        .then(ident_str())
        .then_ignore(punc!(OpenParen))
        .then(params())
        .then_ignore(punc!(CloseParen))
        .then(punc!(RightArrow).ignore_then(typename()).or_not())
        .then(bit!(Or).ignore_then(expr()).or(block()))
        .map_with_state(
            |((((vis, name), params), ret_ty), body): (
                (
                    ((Visibility, String), Vec<(String, TypeName)>),
                    Option<TypeName>,
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

fn type_def<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
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

fn struct_def<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
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

fn const_def<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
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

fn static_def<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
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

fn item<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    just(Token::Newline).or_not().ignore_then(
        type_def()
            .or(func_def())
            .or(struct_def())
            .or(const_def())
            .or(static_def()),
    )
}

fn typename<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, TypeName, ParserExtra<'src>> {
    math!(Times).repeated().foldr(
        path::path(0).map(|p| TypeName {
            path: p,
            ptr_depth: 0,
        }),
        |_, p: TypeName| TypeName {
            path: p.path,
            ptr_depth: p.ptr_depth + 1,
        },
    )
}

fn unit<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, UnitId, ParserExtra<'src>> {
    item()
        .repeated()
        .collect()
        .map_with_state(|_: Vec<NodeId>, _, state| state.unit_stack.pop().unwrap())
}

pub fn parse<'src>(
    tokens: Vec<Spanned<Token, Span>>,
    package: &mut Package,
    name: String,
) -> Result<ParseResult<UnitId, ParserError>> {
    let root = Unit::new(name, None);
    let mut state = ParserState::new(package);
    let unit_id = state.package.add_unit(root);
    state.unit_stack.push(unit_id);
    state.package.set_root(unit_id);

    let parser = unit();
    let input = tokens.split_spanned().0.into_stream();
    Ok(parser.parse_with_state(input, &mut state))
}
