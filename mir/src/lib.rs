use slotmap::{new_key_type, SlotMap};

use crane_parse::{
    ty::Signature,
    unit::{Unit, UnitId},
};

new_key_type! {
    pub struct TypeId;
    pub struct NodeId;
}

pub enum Type {}

pub enum Item {
    Function(NodeId),
    Struct(NodeId),
    Enum(NodeId),
}

pub enum Node {
    //     Item(Item),
    //     Expr(Expr),
}

type TypeCache = SlotMap<TypeId, Type>;

/// The MIRUnit will be a control-flow graph structure instead of a tree
/// However, thanks to generics and `Unit`'s arena-based architecture, the same `Unit` struct can be reused.
type MIRUnit = Unit<NodeId, Node, TypeCache>;
