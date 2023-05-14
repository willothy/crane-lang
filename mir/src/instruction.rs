use std::fmt::Display;

use crate::{MIRBlockId, MIRContext, MIRNodeId, MIRPackage, MIRUnitId, TypeId};
use crane_lex as lex;
use crane_parse::unit::Unit;
use slotmap::Key;

#[derive(Debug)]
pub enum MIRNode {
    Instruction { inst: MIRInstruction },
    Termination { inst: MIRTermination },
    Value { value: MIRValue },
}

#[derive(Debug)]
pub enum MIRValue {
    /// The result of an instruction
    Result {
        ty: TypeId,
        /// Instruction id
        value: MIRNodeId,
    },
    /// A literal value
    Literal { ty: TypeId, value: lex::Literal },
    /// A struct literal
    StructInit {
        ty: TypeId,
        fields: Vec<(String, MIRNodeId)>,
    },
    /// An array literal
    ArrayInit {
        ty: TypeId,
        elements: Vec<MIRNodeId>,
    },
    /// A tuple literal
    TupleInit {
        ty: TypeId,
        elements: Vec<MIRNodeId>,
    },
    // TODO: Function Ptr
}

impl MIRValue {
    pub fn print(&self, ctx: &MIRContext, pkg: &MIRPackage, unit: MIRUnitId) {
        match self {
            MIRValue::Result { ty, value } => {
                let ty = ctx.get_type(*ty).unwrap();
                ty.print(ctx);
                eprint!("%{:?}", value.data());
            }
            MIRValue::Literal { ty, value } => {
                let ty = ctx.get_type(*ty).unwrap();
                ty.print(ctx);
                eprint!(" {}", value);
            }
            MIRValue::StructInit { ty, fields } => {
                let ty = ctx.get_type(*ty).unwrap();
                ty.print(ctx);

                eprintln!(" {{");
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i != 0 {
                        eprintln!(",");
                    }
                    eprint!("{}: ", name);
                    match pkg.unit(unit).unwrap().node(*value).unwrap() {
                        MIRNode::Value { value } => {
                            value.print(ctx, pkg, unit);
                        }
                        _ => panic!(),
                    }
                }
            }
            MIRValue::ArrayInit { ty, elements } => {
                let ty = ctx.get_type(*ty).unwrap();
                ty.print(ctx);

                eprintln!(" [");
                for (i, value) in elements.iter().enumerate() {
                    if i != 0 {
                        eprintln!(",");
                    }
                    match pkg.unit(unit).unwrap().node(*value).unwrap() {
                        MIRNode::Value { value } => {
                            value.print(ctx, pkg, unit);
                        }
                        _ => panic!(),
                    }
                }
                eprintln!("]");
            }
            MIRValue::TupleInit { ty, elements } => {
                let ty = ctx.get_type(*ty).unwrap();
                ty.print(ctx);

                eprintln!(" (");
                for (i, value) in elements.iter().enumerate() {
                    if i != 0 {
                        eprintln!(",");
                    }
                    match pkg.unit(unit).unwrap().node(*value).unwrap() {
                        MIRNode::Value { value } => {
                            value.print(ctx, pkg, unit);
                        }
                        _ => panic!(),
                    }
                }
                eprintln!(")");
            }
        }
    }
}

#[derive(Debug)]
pub enum MIRTermination {
    Return {
        // value
        value: Option<MIRNodeId>,
    },
    Branch {
        // Value
        cond: MIRNodeId,
        // Block id
        then: MIRBlockId,
        // Block id
        r#else: MIRBlockId,
    },
    Jump {
        target: MIRBlockId,
    },
    JumpIf {
        // Value
        cond: MIRNodeId,
        then: MIRBlockId,
    },
}

#[derive(Debug)]
pub enum MIRInstruction {
    Alloc {
        ty: TypeId,
    },
    Load {
        // Instruction id (result)
        ptr: MIRNodeId,
    },
    Store {
        // Instruction id (result)
        ptr: MIRNodeId,
        // Instruction id (result)
        value: MIRNodeId,
    },
    Call {
        callee: MIRNodeId,
        args: Vec<MIRNodeId>,
    },
    IndexAccess {
        // Instruction id (result)
        value: MIRNodeId,
        // Instruction id (result)
        index: MIRNodeId,
    },
    TupleAccess {
        // Instruction id (result)
        value: MIRNodeId,
        index: usize,
    },
    FieldAccess {
        // Instruction id (result)
        value: MIRNodeId,
        field: String,
    },
    Add {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Sub {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Mul {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Div {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Rem {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Neg {
        // Instruction id (result)
        value: MIRNodeId,
    },
    Not {
        // Instruction id (result)
        value: MIRNodeId,
    },
    BitAnd {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    BitOr {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    BitXor {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    BitNot {
        // Instruction id (result)
        value: MIRNodeId,
    },
    Shl {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Shr {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Eq {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Neq {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Lt {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Leq {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Gt {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Geq {
        // Instruction id (result)
        lhs: MIRNodeId,
        // Instruction id (result)
        rhs: MIRNodeId,
    },
    Cast {
        value: MIRNodeId,
        ty: TypeId,
    },
}
