// use std::collections::HashMap;
//
// use proc_macro::TokenStream;
// use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
// use quote::quote;
// use syn::token::{And, Brace, Colon, Fn, Paren, RArrow};
// use syn::{
//     parse, parse2, parse_macro_input, punctuated::Punctuated, token::Comma, Data::Enum,
//     Data::Struct, DataEnum, DataStruct, DeriveInput, Field, Fields::Named, FieldsNamed,
//     FieldsUnnamed, Ident, LitChar, Path, Type, TypePath, Variant,
// };
// use syn::{
//     Block, Expr, ExprCall, FnArg, Generics, ItemFn, Pat, PatType, PathSegment, Stmt, TypeReference,
// };
//
// // pub trait Sym {
// //     fn parse<'a>(i: Span<'a>) -> IResult<Span<'a>, Self>;
// // }
//
// #[derive(Debug)]
// enum Symbol {
//     Plus,
//     Minus,
//     Times,
//     Divide,
// }
//
// trait Chunks<T>
// where
//     T: IntoIterator,
// {
//     fn chunks<const N: usize>(self) -> Vec<T>;
// }
//
// impl<T> Chunks<Vec<T>> for Vec<T> {
//     fn chunks<const N: usize>(self) -> Vec<Vec<T>> {
//         let mut iter = self.into_iter();
//         let mut res = vec![];
//         let mut curr = vec![];
//         while let Some(el) = iter.next() {
//             curr.push(el);
//             if curr.len() == N {
//                 res.push(std::mem::replace(&mut curr, vec![]));
//             }
//         }
//         if curr.len() > 0 {
//             res.push(curr);
//         }
//         res
//     }
// }
//
// #[proc_macro_derive(Symbol)]
// pub fn sym(input: TokenStream) -> TokenStream {
//     let DeriveInput { ident, data, .. } = parse_macro_input!(input as DeriveInput);
//     let e: DataEnum = if let Enum(d) = data {
//         d
//     } else {
//         panic!("Expected enum");
//     };
//
//     let mut sym_impl = TokenStream2::new();
//
//     let mut symbols = HashMap::new();
//     let variants = e.variants;
//     for variant in variants.iter() {
//         let Some(s) = variant.attrs.iter().find(|a| {
// 			let segments = a.path.segments.clone();
// 			return segments.len() == 1 && segments.first().unwrap().ident.to_string().as_str() == "sym"
// 		}) else {
// 			continue;
// 		};
//         let tokens = s.tokens.clone();
//         let s: LitChar = parse2(tokens).unwrap();
//         let k = variant.ident.clone();
//         symbols.insert(k, s);
//     }
//
//     let chunks = symbols.into_iter().collect::<Vec<_>>().chunks::<8>();
//     for (id, chunk) in chunks.into_iter().enumerate() {
//         let func = ItemFn {
//             attrs: vec![],
//             vis: syn::Visibility::Inherited,
//             sig: syn::Signature {
//                 constness: None,
//                 asyncness: None,
//                 unsafety: None,
//                 abi: None,
//                 fn_token: Fn::default(),
//                 ident: Ident::new(&format!("__sym__parse__{}", id), Span::call_site()),
//                 generics: Generics::default(),
//                 paren_token: Paren::default(),
//                 inputs: Punctuated::from_iter([FnArg::Typed(PatType {
//                     attrs: vec![],
//                     pat: Box::new(Pat::Verbatim(TokenStream2::new())),
//                     colon_token: Colon::default(),
//                     ty: Box::new(Type::Reference(TypeReference {
//                         and_token: And::default(),
//                         lifetime: None,
//                         mutability: None,
//                         elem: Box::new(Type::Path(TypePath {
//                             qself: None,
//                             path: Path {
//                                 leading_colon: None,
//                                 segments: Punctuated::from_iter([PathSegment {
//                                     ident: Ident::new("str", Span::call_site()),
//                                     arguments: syn::PathArguments::None,
//                                 }]),
//                             },
//                         })),
//                     })),
//                 })]),
//                 variadic: None,
//                 output: syn::ReturnType::Type(
//                     RArrow::default(),
//                     Box::new(Type::Path(TypePath {
//                         qself: None,
//                         path: Path {
//                             leading_colon: None,
//                             segments: Punctuated::from_iter([PathSegment {
//                                 ident: Ident::new("nom", Span::call_site()),
//                                 arguments: syn::PathArguments::None,
//                             }]),
//                         },
//                     })),
//                 ),
//             },
//             block: Box::new(Block {
//                 brace_token: Brace::default(),
//                 stmts: vec![Stmt::Expr(Expr::Call(ExprCall {
//                     attrs: vec![],
//                     func: Box::new(Expr::Call(todo!())),
//                     args: Punctuated::from_iter([Expr::Verbatim(TokenStream2::from_iter(
//                         Punctuated::from_iter([Ident::new("input", Span::call_site())]),
//                     ))]),
//                     paren_token: Paren::default(),
//                 }))],
//             }),
//         };
//     }
//
//     quote! {
//         impl<'a> Sym for #ident {
//             fn parse<'a>(i: Span<'a>) -> IResult<Span<'a str>, Self> {
//
//             }
//         }
//     }
//     .into()
// }
