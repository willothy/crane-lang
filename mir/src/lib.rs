use std::{collections::HashMap, fmt::Display, hash::Hash};

use interner::Interner;
use lex::Primitive;
use linked_hash_map::LinkedHashMap;
use slotmap::{new_key_type, Key, SlotMap};

use crane_lex as lex;
use crane_parse as parse;
use parse::{package::Package, unit::Unit};

pub mod interner;

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

#[derive(Debug)]
pub enum MIRNode {
    Instruction { ty: TypeId, inst: MIRInstruction },
    Termination { ty: TypeId, inst: MIRTermination },
    Value { ty: TypeId, value: MIRValue },
}

#[derive(Debug)]
pub enum MIRValue {
    Result {
        // Instruction id
        value: MIRNodeId,
    },
    Literal {
        // Literal
        value: lex::Literal,
    },
}

impl Display for MIRValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRValue::Result { value } => write!(f, "%{:?}", value.data()),
            MIRValue::Literal { value } => write!(f, "{}", value),
        }
    }
}

#[derive(Debug)]
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
    Literal {
        ty: TypeId,
        value: lex::Literal,
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

/// The MIRUnit will be a control-flow graph structure instead of a tree
/// Each node is an item or instruction
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

#[derive(Debug)]
pub enum InsertPoint {
    Before(MIRNodeId),
    After(MIRNodeId),
    Start,
    End,
}

#[derive(Debug)]
pub struct InsertInfo {
    pub unit: MIRUnitId,
    pub block: MIRBlockId,
    pub point: InsertPoint,
}

pub struct MIRBuilder {
    pub package: MIRPackage,
    pub insert_point: Option<InsertInfo>,
}

impl MIRBuilder {
    pub fn new() -> Self {
        Self {
            package: MIRPackage::new(),
            insert_point: None,
        }
    }

    pub fn create_unit(
        &mut self,
        _ctx: &mut MIRContext,
        name: String,
        parent: Option<MIRUnitId>,
    ) -> MIRUnitId {
        let unit = self.package.add_unit(MIRUnit::new(name, parent));
        if parent.is_none() {
            self.package.set_root(unit);
        }
        unit
    }

    pub fn get_unit(&self, id: MIRUnitId) -> Option<&MIRUnit> {
        self.package.unit(id)
    }

    pub fn get_unit_mut(&mut self, id: MIRUnitId) -> Option<&mut MIRUnit> {
        self.package.unit_mut(id)
    }

    pub fn get_or_insert_function(
        &mut self,
        ctx: &mut MIRContext,
        name: String,
        ty: Type,
    ) -> MIRItemId {
        let unit = self
            .package
            .unit_mut(self.insert_point.as_ref().unwrap().unit)
            .unwrap();

        let fn_ty = ctx.types.get_or_intern(ty);

        let item = MIRItem::Function {
            ty: fn_ty,
            body: LinkedHashMap::new(),
        };

        let item_id = unit.items.insert(item);
        unit.members_mut().insert(name, item_id);

        item_id
    }

    pub fn set_insert_point(&mut self, unit: MIRUnitId, block: MIRBlockId, point: InsertPoint) {
        self.insert_point = Some(InsertInfo { unit, block, point });
    }

    pub fn set_insert_block(&mut self, unit: MIRUnitId, block: MIRBlockId) {
        self.insert_point = Some(InsertInfo {
            unit,
            block,
            point: InsertPoint::End,
        });
    }

    pub fn get_block_by_name(&self, func: MIRItemId, name: String) -> Option<&MIRBlockId> {
        let insert_unit = self.insert_point.as_ref().unwrap().unit;
        let unit = self.package.unit(insert_unit)?;
        let func = unit.items.get(func);
        if let Some(MIRItem::Function { body, .. }) = func {
            body.get(&name)
        } else {
            None
        }
    }

    pub fn insert_block(&mut self, func: MIRItemId, name: String) -> MIRBlockId {
        let insert_point = self.insert_point.as_ref().unwrap();
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let func = unit.items.get_mut(func).unwrap();
        let block = unit.blocks.insert(MIRBlock {
            body: vec![],
            termination: None,
        });

        if let MIRItem::Function { body, .. } = func {
            body.insert(name, block);
        }
        block
    }

    pub fn insert_instruction(&mut self, instruction: MIRNode) -> MIRNodeId {
        let insert_point = self.insert_point.as_ref().unwrap();
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let id = unit.new_node(instruction);
        let block = unit.blocks.get_mut(insert_point.block).unwrap();

        block.body.push(id);
        id
    }

    pub fn insert_value(&mut self, val: MIRNode) -> MIRNodeId {
        let insert_point = self.insert_point.as_ref().unwrap();
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let id = unit.new_node(val);
        id
    }

    pub fn insert_termination(&mut self, termination: MIRTermination) -> MIRNodeId {
        let insert_point = self.insert_point.as_ref().unwrap();
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let node = unit.new_node(MIRNode::Termination {
            ty: TypeId::default(),
            inst: termination,
        });
        let block = unit.blocks.get_mut(insert_point.block).unwrap();

        if block.termination.is_some() {
            panic!("Block already has a termination");
        } else {
            block.termination = Some(node);
        }
        node
    }

    pub fn print_value(&self, _ctx: &MIRContext, node_id: MIRNodeId) {
        let unit = self
            .package
            .unit(self.insert_point.as_ref().unwrap().unit)
            .unwrap();
        let node = unit.nodes.get(node_id).unwrap();
        #[allow(unused)]
        match node {
            MIRNode::Instruction { ty, inst } => eprint!("%{:?}", node_id.data()),
            MIRNode::Termination { ty, inst } => eprint!("%{:?}", node_id.data()),
            MIRNode::Value { ty, value } => eprint!("{}", value),
        }
    }

    pub fn print_node(&self, ctx: &MIRContext, node: MIRNodeId) {
        let unit = self
            .package
            .unit(self.insert_point.as_ref().unwrap().unit)
            .unwrap();
        let node = unit.nodes.get(node).unwrap();
        #[allow(unused)]
        match node {
            MIRNode::Instruction { ty, inst } => match inst {
                MIRInstruction::Add { lhs, rhs } => {
                    eprint!("add ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Sub { lhs, rhs } => {
                    eprint!("sub ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Mul { lhs, rhs } => {
                    eprint!("mul ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Div { lhs, rhs } => {
                    eprint!("div ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Rem { lhs, rhs } => {
                    eprint!("mod ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Neg { value } => {
                    eprint!("neg ");
                    self.print_node(ctx, *value);
                    eprintln!()
                }
                MIRInstruction::Not { value } => todo!(),
                MIRInstruction::BitAnd { lhs, rhs } => todo!(),
                MIRInstruction::BitOr { lhs, rhs } => todo!(),
                MIRInstruction::BitXor { lhs, rhs } => todo!(),
                MIRInstruction::BitNot { value } => todo!(),
                MIRInstruction::Shl { lhs, rhs } => todo!(),
                MIRInstruction::Shr { lhs, rhs } => todo!(),
                MIRInstruction::Eq { lhs, rhs } => todo!(),
                MIRInstruction::Neq { lhs, rhs } => todo!(),
                MIRInstruction::Lt { lhs, rhs } => todo!(),
                MIRInstruction::Leq { lhs, rhs } => todo!(),
                MIRInstruction::Gt { lhs, rhs } => todo!(),
                MIRInstruction::Geq { lhs, rhs } => todo!(),
                MIRInstruction::Alloc { ty } => todo!(),
                MIRInstruction::Load { ptr } => todo!(),
                MIRInstruction::Store { ptr, value } => todo!(),
                MIRInstruction::Call { callee, args } => todo!(),
                MIRInstruction::Literal { ty, value } => todo!(),
                MIRInstruction::StructInit { ty, fields } => todo!(),
                MIRInstruction::ArrayInit { ty, elements } => todo!(),
                MIRInstruction::TupleInit { ty, elements } => todo!(),
                MIRInstruction::FieldAccess { value, field } => todo!(),
                MIRInstruction::IndexAccess { value, index } => todo!(),
                MIRInstruction::TupleAccess { value, index } => todo!(),
            },
            MIRNode::Termination { ty, inst } => match inst {
                MIRTermination::Return { value } => {
                    eprint!("  return ");
                    if let Some(value) = value {
                        self.print_value(ctx, *value);
                    }
                    eprintln!()
                }
                MIRTermination::Branch { cond, then, r#else } => {
                    eprintln!("  branch: {:?} {:?} {:?}", cond, then, r#else)
                }
                MIRTermination::Jump { target } => eprintln!("  jump {:?}", target),
                MIRTermination::JumpIf { cond, then } => {
                    eprintln!("  jump_if: {:?} {:?}", cond, then)
                }
            },
            MIRNode::Value { ty, value } => eprint!("{}", value),
        }
    }

    pub fn print_block(&self, ctx: &MIRContext, block: MIRBlockId) {
        let insert_point = self.insert_point.as_ref().unwrap();
        let unit = self.package.unit(insert_point.unit).unwrap();
        let block = unit.blocks.get(block).unwrap();

        for node in &block.body {
            eprint!("  %{:?} = ", node.data());
            self.print_node(ctx, *node);
        }
        if let Some(termination) = &block.termination {
            self.print_node(ctx, *termination)
        }
    }

    pub fn print_function(&self, ctx: &MIRContext, func: MIRItemId) {
        let insert_point = self.insert_point.as_ref().unwrap();
        let unit = self.package.unit(insert_point.unit).unwrap();
        let func = unit.items.get(func).unwrap();

        if let MIRItem::Function { body, ty } = func {
            eprintln!("fn {:?}:", ctx.types.get(*ty).unwrap());
            for (name, block) in body {
                eprintln!("{}:", name);
                self.print_block(ctx, *block);
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::MIRContext;

    #[test]
    fn basic() {
        let mut ctx = MIRContext::new();
        let mut builder = MIRBuilder::new();
        let unit = builder.create_unit(&mut ctx, "test".to_string(), None);
        builder.set_insert_point(unit, MIRBlockId::default(), InsertPoint::End);
        let func = builder.get_or_insert_function(
            &mut ctx,
            "test_func".to_string(),
            Type::Function(FunctionType {
                params: vec![],
                ret: None,
            }),
        );
        let entry = builder.insert_block(func, "entry".to_string());
        builder.insert_point.as_mut().unwrap().block = entry;

        let lhs = builder.insert_value(MIRNode::Value {
            ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
            value: MIRValue::Literal {
                value: lex::Literal::Int(5),
            },
        });
        let rhs = builder.insert_value(MIRNode::Value {
            ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
            value: MIRValue::Literal {
                value: lex::Literal::Int(50),
            },
        });
        let res = builder.insert_instruction(MIRNode::Instruction {
            ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
            inst: MIRInstruction::Add { lhs, rhs },
        });
        let retval = builder.insert_instruction(MIRNode::Instruction {
            ty: ctx.types.get_or_intern(Type::Unit),
            inst: MIRInstruction::Add { lhs: res, rhs },
        });
        let _ret = builder.insert_termination(MIRTermination::Return {
            value: Some(retval),
        });
        builder.print_function(&ctx, func);
        // assert!(false)
    }
}

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
