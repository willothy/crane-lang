use std::{cell::RefCell, fmt::Write, rc::Rc};

use crane_lex::Primitive;
use crane_parse::package::{pass::Inspect, ASTPackage};

use crate::{Context, TypeId};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct StructType {
    pub fields: Vec<(String, TypeId)>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionType {
    pub params: Vec<TypeId>,
    pub ret: Option<TypeId>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Unit,
    Primitive(Primitive),
    Function(FunctionType),
    Struct(StructType),
    Pointer(TypeId),
    Array(TypeId, usize),
    Tuple(Vec<TypeId>),
    Inferred,
}

impl Type {
    pub fn print(&self, ctx: Rc<RefCell<Context>>, writer: &mut dyn Write) {
        match self {
            Type::Inferred => write!(writer, "_").unwrap(),
            Type::Unit => write!(writer, "()").unwrap(),
            Type::Primitive(p) => write!(writer, "{}", p).unwrap(),
            Type::Function(FunctionType { params, ret }) => {
                write!(writer, "fn(").unwrap();
                for (i, param) in params.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ", ").unwrap();
                    }
                    ctx.borrow()
                        .get_type(*param)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
                write!(writer, ")").unwrap();
                if let Some(ret) = ret {
                    ctx.borrow()
                        .get_type(*ret)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
            }
            Type::Struct(StructType { fields }) => {
                write!(writer, "struct {{ ").unwrap();
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ", ").unwrap();
                    }
                    write!(writer, "{}: ", name).unwrap();
                    ctx.borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
                write!(writer, " }}").unwrap();
            }
            Type::Pointer(inner) => {
                write!(writer, "*").unwrap();
                ctx.borrow()
                    .get_type(*inner)
                    .unwrap()
                    .print(ctx.clone(), writer);
            }
            Type::Array(ty, len) => {
                write!(writer, "[").unwrap();
                ctx.borrow()
                    .get_type(*ty)
                    .unwrap()
                    .print(ctx.clone(), writer);
                write!(writer, "; {}]", len).unwrap();
            }
            Type::Tuple(types) => {
                write!(writer, "(").unwrap();
                for (i, ty) in types.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ", ").unwrap();
                    }
                    ctx.borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
                write!(writer, ")").unwrap();
            }
        }
    }
}

/// A pass that resolves as many types in the AST as possible, *before* MIR construction.
/// After this pass, the AST should be in a state where it can be converted to MIR.
/// A final type resolution pass will be run on MIR after its construction, and after this point
/// all unknown types should be resolved - any that are not should be reported as errors.
pub struct TypeResolutionPass {
    ctx: Rc<RefCell<Context>>,
}

impl Inspect for TypeResolutionPass {
    type Scope = ASTPackage;

    type Input = ();

    fn inspect(&mut self, scope: &Self::Scope, input: Self::Input) {
        todo!()
    }
}
