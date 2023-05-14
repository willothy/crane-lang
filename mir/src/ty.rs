use std::{cell::RefCell, fmt::Write, rc::Rc};

use crane_lex::Primitive;

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
}

impl Type {
    pub fn print(&self, ctx: Rc<RefCell<Context>>, writer: &mut dyn Write) {
        match self {
            Type::Unit => write!(writer, "()").unwrap(),
            Type::Primitive(p) => write!(writer, "{}", p).unwrap(),
            Type::Function(FunctionType { params, ret }) => {
                write!(writer, "fn(");
                for (i, param) in params.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ", ");
                    }
                    ctx.borrow()
                        .get_type(*param)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
                write!(writer, ")");
                if let Some(ret) = ret {
                    ctx.borrow()
                        .get_type(*ret)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
            }
            Type::Struct(StructType { fields }) => {
                write!(writer, "struct {{ ");
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ", ");
                    }
                    write!(writer, "{}: ", name);
                    ctx.borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
                write!(writer, " }}");
            }
            Type::Pointer(inner) => {
                write!(writer, "*");
                ctx.borrow()
                    .get_type(*inner)
                    .unwrap()
                    .print(ctx.clone(), writer);
            }
            Type::Array(ty, len) => {
                write!(writer, "[");
                ctx.borrow()
                    .get_type(*ty)
                    .unwrap()
                    .print(ctx.clone(), writer);
                write!(writer, "; {}]", len);
            }
            Type::Tuple(types) => {
                write!(writer, "(");
                for (i, ty) in types.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ", ");
                    }
                    ctx.borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(ctx.clone(), writer);
                }
                write!(writer, ")");
            }
        }
    }
}
