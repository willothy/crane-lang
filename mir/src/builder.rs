use crane_parse::unit::Unit;
use linked_hash_map::LinkedHashMap;
use slotmap::Key;

use crate::{
    instruction::{MIRInstruction, MIRNode, MIRTermination},
    ty::Type,
    MIRBlock, MIRBlockId, MIRContext, MIRItem, MIRItemId, MIRNodeId, MIRPackage, MIRUnit,
    MIRUnitId, TypeId,
};

#[derive(Debug, Default)]
pub enum InsertPoint {
    Before(MIRNodeId),
    After(MIRNodeId),
    Start,
    #[default]
    End,
}

#[derive(Debug, Default)]
pub struct InsertInfo {
    pub unit: MIRUnitId,
    pub block: MIRBlockId,
    pub point: InsertPoint,
}
pub struct MIRBuilder {
    pub package: MIRPackage,
    pub insert_point: InsertInfo,
}

impl MIRBuilder {
    pub fn new() -> Self {
        Self {
            package: MIRPackage::new(),
            insert_point: InsertInfo {
                ..Default::default()
            },
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
        let unit = self.package.unit_mut(self.insert_point.unit).unwrap();

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
        self.insert_point = InsertInfo { unit, block, point };
    }

    pub fn set_insert_block(&mut self, unit: MIRUnitId, block: MIRBlockId) {
        self.insert_point = InsertInfo {
            unit,
            block,
            point: InsertPoint::End,
        };
    }

    pub fn get_block_by_name(&self, func: MIRItemId, name: String) -> Option<&MIRBlockId> {
        let insert_unit = self.insert_point.unit;
        let unit = self.package.unit(insert_unit)?;
        let func = unit.items.get(func);
        if let Some(MIRItem::Function { body, .. }) = func {
            body.get(&name)
        } else {
            None
        }
    }

    pub fn insert_block(&mut self, func: MIRItemId, name: String) -> MIRBlockId {
        let insert_point = &self.insert_point;
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
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let id = unit.new_node(instruction);
        let block = unit.blocks.get_mut(insert_point.block).unwrap();

        block.body.push(id);
        id
    }

    pub fn insert_value(&mut self, val: MIRNode) -> MIRNodeId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let id = unit.new_node(val);
        id
    }

    pub fn insert_termination(&mut self, termination: MIRTermination) -> MIRNodeId {
        let insert_point = &self.insert_point;
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

    pub fn print_value(&self, ctx: &MIRContext, node_id: MIRNodeId) {
        let unit = self.package.unit(self.insert_point.unit).unwrap();
        let node = unit.nodes.get(node_id).unwrap();
        #[allow(unused)]
        match node {
            MIRNode::Instruction { ty, inst } => {
                ctx.get_type(*ty).unwrap().print(ctx);
                eprint!(" %{:?}", node_id.data());
            }
            MIRNode::Termination { ty, inst } => {
                ctx.get_type(*ty).unwrap().print(ctx);
                eprint!(" %{:?}", node_id.data());
            }
            MIRNode::Value { value } => {
                // ctx.get_type(*ty).unwrap().print(ctx);
                // eprint!(" ");
                value.print(ctx, &self.package, self.insert_point.unit);
                // eprint!(" {}", value);
            }
        }
    }

    pub fn print_node(&self, ctx: &MIRContext, node: MIRNodeId) {
        let unit = self.package.unit(self.insert_point.unit).unwrap();
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
                MIRInstruction::Not { value } => {
                    eprint!("not ");
                    self.print_node(ctx, *value);
                    eprintln!()
                }
                MIRInstruction::BitAnd { lhs, rhs } => {
                    eprint!("band ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::BitOr { lhs, rhs } => {
                    eprint!("bor ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::BitXor { lhs, rhs } => {
                    eprint!("xor ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::BitNot { value } => {
                    eprint!("bnot ");
                    self.print_node(ctx, *value);
                    eprintln!()
                }
                MIRInstruction::Shl { lhs, rhs } => {
                    eprint!("shl ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Shr { lhs, rhs } => {
                    eprint!("shr ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Eq { lhs, rhs } => {
                    eprint!("eq ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Neq { lhs, rhs } => {
                    eprint!("neq ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Lt { lhs, rhs } => {
                    eprint!("lt ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Leq { lhs, rhs } => {
                    eprint!("leq ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Gt { lhs, rhs } => {
                    eprint!("gt ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Geq { lhs, rhs } => {
                    eprint!("geq ");
                    self.print_value(ctx, *lhs);
                    eprint!(" ");
                    self.print_value(ctx, *rhs);
                    eprintln!()
                }
                MIRInstruction::Alloc { ty } => {
                    eprint!("alloc ");
                    ctx.get_type(*ty).unwrap().print(ctx);
                    eprintln!()
                }
                MIRInstruction::Load { ptr } => {
                    eprint!("load ");
                    self.print_value(ctx, *ptr);
                    eprintln!()
                }
                MIRInstruction::Store { ptr, value } => {
                    eprint!("store ");
                    self.print_value(ctx, *ptr);
                    eprint!(" ");
                    self.print_value(ctx, *value);
                    eprintln!()
                }
                MIRInstruction::Call { callee, args } => {
                    eprint!("call ");
                    self.print_value(ctx, *callee);
                    eprint!(" ");
                    for arg in args {
                        self.print_value(ctx, *arg);
                        eprint!(" ");
                    }
                    eprintln!()
                }
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
            MIRNode::Value { value } => panic!(),
        }
    }

    pub fn print_block(&self, ctx: &MIRContext, block: MIRBlockId) {
        let insert_point = &self.insert_point;
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
        let insert_point = &self.insert_point;
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
    use crane_lex::{Literal, Primitive};

    use super::*;
    use crate::{instruction::MIRValue, ty::FunctionType, MIRContext};

    #[test]
    fn basic() {
        let mut ctx = MIRContext::new();
        let mut builder = MIRBuilder::new();
        let unit = builder.create_unit(&mut ctx, "test".to_string(), None);
        // make sure to set the insert unit!
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
        builder.insert_point.block = entry;

        let lhs = builder.insert_value(MIRNode::Value {
            value: MIRValue::Literal {
                ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
                value: Literal::Int(5),
            },
        });
        let rhs = builder.insert_value(MIRNode::Value {
            value: MIRValue::Literal {
                ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
                value: Literal::Int(50),
            },
        });
        let res = builder.insert_instruction(MIRNode::Instruction {
            ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
            inst: MIRInstruction::Add { lhs, rhs },
        });
        let retval = builder.insert_instruction(MIRNode::Instruction {
            ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
            inst: MIRInstruction::Add { lhs: res, rhs },
        });
        let _ret = builder.insert_termination(MIRTermination::Return {
            value: Some(retval),
        });
        builder.print_function(&ctx, func);
        if std::env::var("FORCE_FAIL").is_ok() {
            assert!(false)
        }
    }
}
