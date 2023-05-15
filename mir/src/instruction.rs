use std::{cell::RefCell, fmt::Write, rc::Rc};

use crate::{BlockId, Context, MIRPackage, TypeId, UnitId, ValueId};
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
        value: ValueId,
    },
    /// A literal value
    Literal { ty: TypeId, value: lex::Literal },
    /// A struct literal
    StructInit {
        ty: TypeId,
        fields: Vec<(String, ValueId)>,
    },
    /// An array literal
    ArrayInit { ty: TypeId, elements: Vec<ValueId> },
    /// A tuple literal
    TupleInit { ty: TypeId, elements: Vec<ValueId> },
    // TODO: Function Ptr
}

impl MIRValue {
    pub fn ty(&self) -> TypeId {
        match self {
            MIRValue::Result { ty, .. } => *ty,
            MIRValue::Literal { ty, .. } => *ty,
            MIRValue::StructInit { ty, .. } => *ty,
            MIRValue::ArrayInit { ty, .. } => *ty,
            MIRValue::TupleInit { ty, .. } => *ty,
        }
    }

    pub fn print(
        &self,
        ctx: Rc<RefCell<Context>>,
        pkg: &MIRPackage,
        unit: UnitId,
        writer: &mut dyn Write,
    ) {
        match self {
            MIRValue::Result { ty, value } => {
                let _ctx = ctx.borrow();
                let ty = _ctx.get_type(*ty).unwrap();
                ty.print(ctx.clone(), writer);
                write!(writer, "%{:?}", value.data()).unwrap();
            }
            MIRValue::Literal { ty, value } => {
                let _ctx = ctx.borrow();
                let ty = _ctx.get_type(*ty).unwrap();
                ty.print(ctx.clone(), writer);
                write!(writer, " {}", value).unwrap();
            }
            MIRValue::StructInit { ty, fields } => {
                let _ctx = ctx.borrow();
                let ty = _ctx.get_type(*ty).unwrap();
                ty.print(ctx.clone(), writer);

                write!(writer, " {{").unwrap();
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ",").unwrap();
                    }
                    write!(writer, "{}: ", name).unwrap();
                    match pkg.unit(unit).unwrap().node(*value).unwrap() {
                        MIRNode::Value { value } => {
                            value.print(ctx.clone(), pkg, unit, writer);
                        }
                        _ => panic!(),
                    }
                }
            }
            MIRValue::ArrayInit { ty, elements } => {
                let _ctx = ctx.borrow();
                let ty = _ctx.get_type(*ty).unwrap();
                ty.print(ctx.clone(), writer);

                write!(writer, " [").unwrap();
                for (i, value) in elements.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ",").unwrap();
                    }
                    match pkg.unit(unit).unwrap().node(*value).unwrap() {
                        MIRNode::Value { value } => {
                            value.print(ctx.clone(), pkg, unit, writer);
                        }
                        _ => panic!(),
                    }
                }
                write!(writer, "]").unwrap();
            }
            MIRValue::TupleInit { ty, elements } => {
                let _ctx = ctx.borrow();
                let ty = _ctx.get_type(*ty).unwrap();
                ty.print(ctx.clone(), writer);

                write!(writer, " (").unwrap();
                for (i, value) in elements.iter().enumerate() {
                    if i != 0 {
                        write!(writer, ",").unwrap();
                    }
                    match pkg.unit(unit).unwrap().node(*value).unwrap() {
                        MIRNode::Value { value } => {
                            value.print(ctx.clone(), pkg, unit, writer);
                        }
                        _ => panic!(),
                    }
                }
                write!(writer, ")").unwrap();
            }
        }
    }
}

#[derive(Debug)]
pub enum MIRTermination {
    Return {
        // value
        value: Option<ValueId>,
    },
    Branch {
        // Value
        cond: ValueId,
        // Block id
        then: BlockId,
        // Block id
        r#else: BlockId,
    },
    Jump {
        target: BlockId,
    },
    JumpIf {
        // Value
        cond: ValueId,
        then: BlockId,
    },
}

#[derive(Debug)]
pub enum MIRInstruction {
    Alloc {
        ty: TypeId,
    },
    Load {
        // Instruction id (result)
        ptr: ValueId,
    },
    Store {
        // Instruction id (result)
        ptr: ValueId,
        // Instruction id (result)
        value: ValueId,
    },
    Call {
        callee: ValueId,
        args: Vec<ValueId>,
    },
    IndexAccess {
        // Instruction id (result)
        value: ValueId,
        // Instruction id (result)
        index: ValueId,
    },
    TupleAccess {
        // Instruction id (result)
        value: ValueId,
        index: usize,
    },
    FieldAccess {
        // Instruction id (result)
        value: ValueId,
        field: String,
    },
    Add {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Sub {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Mul {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Div {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Rem {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Neg {
        // Instruction id (result)
        value: ValueId,
    },
    Not {
        // Instruction id (result)
        value: ValueId,
    },
    BitAnd {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    BitOr {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    BitXor {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    BitNot {
        // Instruction id (result)
        value: ValueId,
    },
    Shl {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Shr {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Eq {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Neq {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Lt {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Leq {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Gt {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Geq {
        // Instruction id (result)
        lhs: ValueId,
        // Instruction id (result)
        rhs: ValueId,
    },
    Cast {
        value: ValueId,
        ty: TypeId,
    },
}
