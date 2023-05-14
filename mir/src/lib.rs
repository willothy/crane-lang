use std::{collections::HashMap, fmt::Display, hash::Hash};

use instruction::{MIRInstruction, MIRNode, MIRTermination};
use interner::Interner;
use lex::Primitive;
use linked_hash_map::LinkedHashMap;
use slotmap::{new_key_type, Key, SlotMap};

use crane_lex as lex;
use crane_parse as parse;
use parse::{package::Package, unit::Unit};
use ty::Type;

pub mod builder;
pub mod instruction;
pub mod interner;
pub mod ty;

new_key_type! {
    pub struct TypeId;
    pub struct MIRUnitId;
    pub struct MIRBlockId;
    pub struct MIRNodeId;
    pub struct MIRItemId;
}

#[derive(Debug)]
pub enum MIRItem {
    Function {
        ty: TypeId,
        body: LinkedHashMap<String, MIRBlockId>,
    },
    Struct {
        fields: LinkedHashMap<String, TypeId>,
    },
    // TODO:
    // Const { ty: TypeId, value: MIRNodeId },
    // Static { ty: TypeId, value: MIRNodeId },
}

#[derive(Debug)]
pub struct MIRBlock {
    body: Vec<MIRNodeId>,
    termination: Option<MIRNodeId>,
}

/// The MIRUnit is a control-flow graph structure
/// Each node is an instruction or immediate value (literal)
#[derive(Debug)]
pub struct MIRUnit {
    pub name: String,
    pub parent: Option<MIRUnitId>,
    pub blocks: SlotMap<MIRBlockId, MIRBlock>,
    pub nodes: SlotMap<MIRNodeId, MIRNode>,
    pub items: SlotMap<MIRItemId, MIRItem>,
    pub members: HashMap<String, MIRItemId>,
}

impl MIRUnit {
    pub fn new(name: String, parent: Option<MIRUnitId>) -> Self {
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

impl Unit<MIRUnitId, MIRNodeId, MIRItemId, MIRNode> for MIRUnit {
    fn name(&self) -> &str {
        &self.name
    }

    fn parent(&self) -> Option<MIRUnitId> {
        self.parent
    }

    fn node(&self, id: MIRNodeId) -> Option<&MIRNode> {
        self.nodes.get(id)
    }

    fn node_mut(&mut self, id: MIRNodeId) -> Option<&mut MIRNode> {
        self.nodes.get_mut(id)
    }

    fn new_node(&mut self, node: MIRNode) -> MIRNodeId {
        self.nodes.insert(node)
    }

    fn members(&self) -> &std::collections::HashMap<String, MIRItemId> {
        &self.members
    }

    fn members_mut(&mut self) -> &mut std::collections::HashMap<String, MIRItemId> {
        &mut self.members
    }
}

pub type MIRPackage = Package<MIRUnitId, MIRUnit, MIRNodeId, MIRNode, MIRItemId>;

pub struct MIRContext {
    types: Interner<TypeId, Type>,
}

impl MIRContext {
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
