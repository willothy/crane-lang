use anyhow::Result;
use chumsky::input::BoxedStream;
use chumsky::primitive::{any, just};
use chumsky::recovery::{nested_delimiters, via_parser};
use chumsky::recursive::recursive;
use chumsky::span::SimpleSpan;
use chumsky::{select, IterParser, ParseResult, Parser as ChumskyParser};
use lex::{IntoStream, Span, SplitSpanned};
use slotmap::new_key_type;

use crane_lex as lex;
use crane_lex::{Assignment, Keyword, Punctuation, Spanned, Symbol, Token, Visibility};

pub mod expr;
pub mod item;
pub mod ops;
pub mod package;
pub mod path;
pub mod unit;

use expr::Expr;
use item::Item;
use ops::BinaryOp;
use package::Package;
use path::{ItemPath, PathPart};
use unit::{NodeId, Unit, UnitId};

new_key_type! {
    pub struct TypeId;
}

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Item(Item),
    Expr(Expr),
}

pub type ParserError<'src> = chumsky::error::Simple<'src, Token>; //error::Rich<'src, E>;

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

#[allow(unused)]
macro_rules! kw {
    ($id:ident) => {
        just(Token::Keyword(Keyword::$id))
    };
}

#[allow(unused)]
macro_rules! punc {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Punctuation(Punctuation::$id)))
    };
}

#[allow(unused)]
macro_rules! math {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Arithmetic(Arithmetic::$id)))
    };
}

#[allow(unused)]
macro_rules! cmp {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Comparison(Comparison::$id)))
    };
}

#[allow(unused)]
macro_rules! assign {
    ($id:ident) => {
        just(Token::Symbol(Symbol::Assignment(Assignment::$id)))
    };
    [$id:ident] => {
        Token::Symbol(Symbol::Assignment(Assignment::$id))
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
) -> impl ChumskyParser<'src, ParserStream<'src>, Vec<(String, ItemPath)>, ParserExtra<'src>> {
    ident_str()
        .then_ignore(punc!(Colon))
        .then(path::path())
        .separated_by(punc!(Comma))
        .at_least(0)
        .collect::<Vec<(String, ItemPath)>>()
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

fn expr<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    recursive(|expr| {
        let r#let = just(Token::Keyword(Keyword::Let))
            .ignore_then(ident_str())
            .then_ignore(punc!(Colon))
            .then(path::path())
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

        let atom = literal()
            .or(ident)
            .or(block)
            .or(r#loop)
            .or(r#let)
            .or(r#if)
            .or(r#while)
            .or(list)
            .or(expr
                .clone()
                .delimited_by(punc!(OpenParen), punc!(CloseParen)))
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::Symbol(Symbol::Punctuation(Punctuation::OpenParen)),
                Token::Symbol(Symbol::Punctuation(Punctuation::CloseParen)),
                [
                    (
                        Token::Symbol(Symbol::Punctuation(Punctuation::OpenBracket)),
                        Token::Symbol(Symbol::Punctuation(Punctuation::CloseBracket)),
                    ),
                    (
                        Token::Symbol(Symbol::Punctuation(Punctuation::OpenBrace)),
                        Token::Symbol(Symbol::Punctuation(Punctuation::CloseBrace)),
                    ),
                ],
                |_span| NodeId::default(),
            )))
            // Attempt to recover anything that looks like a list but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::Symbol(Symbol::Punctuation(Punctuation::OpenBracket)),
                Token::Symbol(Symbol::Punctuation(Punctuation::CloseBracket)),
                [
                    (
                        Token::Symbol(Symbol::Punctuation(Punctuation::OpenParen)),
                        Token::Symbol(Symbol::Punctuation(Punctuation::CloseParen)),
                    ),
                    (
                        Token::Symbol(Symbol::Punctuation(Punctuation::OpenBrace)),
                        Token::Symbol(Symbol::Punctuation(Punctuation::CloseBrace)),
                    ),
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

        let multiplicative = just(Token::Symbol(Symbol::Arithmetic(lex::Arithmetic::Times)))
            .map(|_| BinaryOp::Mul)
            .or(
                just(Token::Symbol(Symbol::Arithmetic(lex::Arithmetic::Divide)))
                    .map(|_| BinaryOp::Div),
            )
            .or(
                just(Token::Symbol(Symbol::Arithmetic(lex::Arithmetic::Mod)))
                    .map(|_| BinaryOp::Mod),
            );
        let product = call.clone().foldl_with_state(
            multiplicative.then(call).repeated(),
            |lhs, (op, rhs), state| {
                state
                    .current_unit_mut()
                    .new_expr(Expr::BinaryOp { lhs, op, rhs })
            },
        );

        let additive = just(Token::Symbol(Symbol::Arithmetic(lex::Arithmetic::Plus)))
            .map(|_| BinaryOp::Add)
            .or(
                just(Token::Symbol(Symbol::Arithmetic(lex::Arithmetic::Minus)))
                    .map(|_| BinaryOp::Sub),
            );
        let sum = product.clone().foldl_with_state(
            additive.then(product).repeated(),
            |lhs, (op, rhs), state| {
                state
                    .current_unit_mut()
                    .new_expr(Expr::BinaryOp { lhs, op, rhs })
            },
        );

        let equality = just(Token::Symbol(Symbol::Comparison(lex::Comparison::Equal)))
            .map(|_| BinaryOp::Eq)
            .or(
                just(Token::Symbol(Symbol::Comparison(lex::Comparison::NotEqual)))
                    .map(|_| BinaryOp::Neq),
            );
        let cmp_equality =
            sum.clone()
                .foldl_with_state(equality.then(sum).repeated(), |lhs, (op, rhs), state| {
                    state
                        .current_unit_mut()
                        .new_expr(Expr::BinaryOp { lhs, op, rhs })
                });

        let relational = just(Token::Symbol(Symbol::Comparison(lex::Comparison::LessThan)))
            .map(|_| BinaryOp::Lt)
            .or(just(Token::Symbol(Symbol::Comparison(
                lex::Comparison::LessThanOrEqual,
            )))
            .map(|_| BinaryOp::Leq))
            .or(just(Token::Symbol(Symbol::Comparison(
                lex::Comparison::GreaterThan,
            )))
            .map(|_| BinaryOp::Gt))
            .or(just(Token::Symbol(Symbol::Comparison(
                lex::Comparison::GreaterThanOrEqual,
            )))
            .map(|_| BinaryOp::Geq));
        let cmp_relational = cmp_equality.clone().foldl_with_state(
            relational.then(cmp_equality).repeated(),
            |lhs, (op, rhs), state| {
                state
                    .current_unit_mut()
                    .new_expr(Expr::BinaryOp { lhs, op, rhs })
            },
        );

        cmp_relational
    })
}

fn block<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    punc!(OpenBrace)
        .ignore_then(expr().repeated().at_least(0).collect::<Vec<NodeId>>())
        .then_ignore(just(Token::Symbol(Symbol::Punctuation(
            Punctuation::CloseBrace,
        ))))
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
        .then_ignore(just(Token::Keyword(Keyword::Fn)))
        .then(ident_str())
        .then_ignore(just(Token::Symbol(Symbol::Punctuation(
            Punctuation::OpenParen,
        ))))
        .then(params())
        .then_ignore(just(Token::Symbol(Symbol::Punctuation(
            Punctuation::CloseParen,
        ))))
        .then(punc!(RightArrow).ignore_then(path::path()).or_not())
        .then(block())
        .map_with_state(
            |((((vis, name), params), ret_ty), body): (
                (
                    ((Visibility, String), Vec<(String, ItemPath)>),
                    Option<ItemPath>,
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
                            name: ItemPath::from(vec![PathPart::Named(name)]),
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
        .then_ignore(just(Token::Keyword(Keyword::Type)))
        .then(ident_str())
        .then_ignore(assign!(Assign))
        .then(path::path())
        .map_with_state(
            move |((vis, name), ty), _span: SimpleSpan, state: &mut ParserState| {
                state
                    .package
                    .unit_mut(state.unit_stack.last().copied().unwrap())
                    .unwrap()
                    .new_item(
                        name.clone(),
                        Item::TypeDef {
                            vis,
                            name: ItemPath::from(vec![PathPart::Named(name)]),
                            ty,
                        },
                    )
            },
        )
}

fn struct_def<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    vis()
        .then_ignore(just(Token::Keyword(Keyword::Struct)))
        .then(ident_str())
        .then_ignore(just(Token::Symbol(Symbol::Punctuation(
            Punctuation::OpenBrace,
        ))))
        .then(params())
        .then_ignore(just(Token::Symbol(Symbol::Punctuation(
            Punctuation::CloseBrace,
        ))))
        .map_with_state(|((vis, name), fields), _span, state: &mut ParserState| {
            state
                .package
                .unit_mut(state.current_unit_id().clone())
                .unwrap()
                .new_item(
                    name.clone(),
                    Item::StructDef {
                        vis,
                        name: ItemPath::from(vec![PathPart::Named(name)]),
                        fields,
                    },
                )
        })
}

fn const_def<'src>() -> impl ChumskyParser<'src, ParserStream<'src>, NodeId, ParserExtra<'src>> {
    vis()
        .then_ignore(just(Token::Keyword(Keyword::Const)))
        .then(ident_str())
        .then_ignore(just(Token::Symbol(Symbol::Punctuation(Punctuation::Colon))))
        .then(path::path())
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
        .then_ignore(just(Token::Keyword(Keyword::Static)))
        .then(ident_str())
        .then_ignore(just(Token::Symbol(Symbol::Punctuation(Punctuation::Colon))))
        .then(path::path())
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
    type_def()
        .or(func_def())
        .or(struct_def())
        .or(const_def())
        .or(static_def())
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
