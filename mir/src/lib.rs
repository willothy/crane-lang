use std::{collections::HashMap, hash::Hash};

use instruction::MIRNode;
use interner::Interner;
use linked_hash_map::LinkedHashMap;
use slotmap::{new_key_type, SlotMap};

#[allow(unused)]
use crane_lex as lex;
use crane_parse as parse;
use parse::package::Package;
use ty::Type;

pub mod builder;
pub mod graphviz;
pub mod instruction;
pub mod interner;
pub mod scope;
pub mod transformation;
pub mod ty;

new_key_type! {
    pub struct TypeId;
    pub struct UnitId;
    pub struct BlockId;
    pub struct ValueId;
    pub struct ItemId;
}

#[derive(Debug)]
pub enum Item {
    Function {
        name: String,
        ty: TypeId,
        body: LinkedHashMap<String, BlockId>,
    },
    Struct {
        name: String,
        fields: LinkedHashMap<String, TypeId>,
    },
    // TODO:
    // Const { ty: TypeId, value: MIRNodeId },
    // Static { ty: TypeId, value: MIRNodeId },
}

impl Item {
    pub fn is_function(&self) -> bool {
        matches!(self, Item::Function { .. })
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Item::Struct { .. })
    }
}

#[derive(Debug)]
pub struct Block {
    body: Vec<ValueId>,
    termination: Option<ValueId>,
    pub name: String,
}

/// The MIRUnit is a control-flow graph structure
/// Each node is an instruction or immediate value (literal)
#[derive(Debug)]
pub struct Unit {
    pub name: String,
    pub parent: Option<UnitId>,
    pub blocks: SlotMap<BlockId, Block>,
    pub nodes: SlotMap<ValueId, MIRNode>,
    pub items: SlotMap<ItemId, Item>,
    pub members: HashMap<String, ItemId>,
}

impl Unit {
    pub fn new(name: String, parent: Option<UnitId>) -> Self {
        Self {
            name,
            parent,
            blocks: SlotMap::with_key(),
            nodes: SlotMap::with_key(),
            items: SlotMap::with_key(),
            members: HashMap::new(),
        }
    }
}

impl parse::unit::Unit<UnitId, ValueId, ItemId, MIRNode> for Unit {
    fn name(&self) -> &str {
        &self.name
    }

    fn parent(&self) -> Option<UnitId> {
        self.parent
    }

    fn node(&self, id: ValueId) -> Option<&MIRNode> {
        self.nodes.get(id)
    }

    fn node_mut(&mut self, id: ValueId) -> Option<&mut MIRNode> {
        self.nodes.get_mut(id)
    }

    fn new_node(&mut self, node: MIRNode) -> ValueId {
        self.nodes.insert(node)
    }

    fn members(&self) -> &std::collections::HashMap<String, ItemId> {
        &self.members
    }

    fn members_mut(&mut self) -> &mut std::collections::HashMap<String, ItemId> {
        &mut self.members
    }
}

pub type MIRPackage = Package<UnitId, Unit, ValueId, MIRNode, ItemId>;

pub struct Context {
    types: Interner<TypeId, Type>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            types: Interner::new(),
        }
    }

    pub fn intern_type(&mut self, ty: Type) -> TypeId {
        self.types.get_or_intern(ty)
    }

    pub fn get_type(&self, id: TypeId) -> Option<&Type> {
        self.types.get(id)
    }
}
