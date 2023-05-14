use std::{cell::RefCell, rc::Rc};

use crane_lex::Primitive;

use crate::{MIRContext, TypeId};

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
    pub fn print(&self, ctx: Rc<RefCell<MIRContext>>) {
        match self {
            Type::Unit => eprint!("()"),
            Type::Primitive(p) => eprint!("{}", p),
            Type::Function(FunctionType { params, ret }) => {
                eprint!("fn(");
                for (i, param) in params.iter().enumerate() {
                    if i != 0 {
                        eprint!(", ");
                    }
                    ctx.borrow().get_type(*param).unwrap().print(ctx.clone());
                }
                eprint!(")");
                if let Some(ret) = ret {
                    ctx.borrow().get_type(*ret).unwrap().print(ctx.clone());
                }
            }
            Type::Struct(StructType { fields }) => {
                eprint!("struct {{ ");
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i != 0 {
                        eprint!(", ");
                    }
                    eprint!("{}: ", name);
                    ctx.borrow().get_type(*ty).unwrap().print(ctx.clone());
                }
                eprint!(" }}");
            }
            Type::Pointer(inner) => {
                eprint!("*");
                ctx.borrow().get_type(*inner).unwrap().print(ctx.clone());
            }
            Type::Array(ty, len) => {
                eprint!("[");
                ctx.borrow().get_type(*ty).unwrap().print(ctx.clone());
                eprint!("; {}]", len);
            }
            Type::Tuple(types) => {
                eprint!("(");
                for (i, ty) in types.iter().enumerate() {
                    if i != 0 {
                        eprint!(", ");
                    }
                    ctx.borrow().get_type(*ty).unwrap().print(ctx.clone());
                }
                eprint!(")");
            }
        }
    }
}
