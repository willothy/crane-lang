use slotmap::{new_key_type, SlotMap};

use crane_parse::{package::Package, unit::Unit};

new_key_type! {
    pub struct TypeId;
    pub struct MIRUnitId;
    pub struct MIRNodeId;
}

pub enum Type {}

pub enum MIRItem {
    Function { ty: TypeId, body: MIRNodeId },
    Struct { fields: Vec<(String, TypeId)> },
    Const { ty: TypeId, value: MIRNodeId },
    Static { ty: TypeId, value: MIRNodeId },
}

pub enum MIRNode {
    Item(MIRItem),
    Block {
        body: Vec<MIRNodeId>,
        terminator: Option<MIRNodeId>,
    },
    Instruction(MIRInstruction),
    Termination(MIRTermination),
}

pub enum MIRTermination {
    Return {
        // Instruction id (result)
        value: Option<MIRNodeId>,
    },
    Branch {
        // Instruction id (result)
        cond: MIRNodeId,
        // Block id
        then: MIRNodeId,
        // Block id
        r#else: MIRNodeId,
    },
    Jump {
        // Block id
        target: MIRNodeId,
    },
    JumpIf {
        // Instruction id (result)
        cond: MIRNodeId,
        // Block id
        then: MIRNodeId,
    },
}

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
    Literal {
        ty: TypeId,
        value: crane_lex::Literal,
    },
    StructInit {
        ty: TypeId,
        fields: Vec<(String, MIRNodeId)>,
    },
    ArrayInit {
        ty: TypeId,
        elements: Vec<MIRNodeId>,
    },
    TupleInit {
        ty: TypeId,
        elements: Vec<MIRNodeId>,
    },
    FieldAccess {
        // Instruction id (result)
        value: MIRNodeId,
        field: String,
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
}

pub type TypeStore = SlotMap<TypeId, Type>;

/// The MIRUnit will be a control-flow graph structure instead of a tree
/// However, thanks to generics and `Unit`'s arena-based architecture, the same `Unit` struct can be reused.
pub type MIRUnit = Unit<MIRUnitId, MIRNodeId, MIRNode, TypeStore>;

pub type MIRPackage = Package<MIRUnitId, MIRNodeId, MIRNode, TypeStore>;
