use crane_parse::unit::Unit;
use linked_hash_map::LinkedHashMap;
use slotmap::Key;

use crate::{
    instruction::{MIRInstruction, MIRNode, MIRTermination, MIRValue},
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
            name: name.clone(),
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

    pub fn set_insert_unit(&mut self, unit: MIRUnitId) {
        self.insert_point.unit = unit;
    }

    pub fn set_insert_block(&mut self, block: MIRBlockId) {
        self.insert_point.block = block;
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

    pub fn create_block(&mut self, func: MIRItemId, name: String) -> MIRBlockId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let func = unit.items.get_mut(func).unwrap();
        let block = unit.blocks.insert(MIRBlock {
            name: name.clone(),
            body: vec![],
            termination: None,
        });

        if let MIRItem::Function { body, .. } = func {
            body.insert(name, block);
        }
        block
    }

    pub fn create_instruction(&mut self, instruction: MIRInstruction) -> MIRNodeId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let id = unit.new_node(MIRNode::Instruction { inst: instruction });
        let block = unit.blocks.get_mut(insert_point.block).unwrap();

        block.body.push(id);
        id
    }

    pub fn create_value(&mut self, value: MIRValue) -> MIRNodeId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        unit.new_node(MIRNode::Value { value })
    }

    pub fn create_termination(&mut self, termination: MIRTermination) -> MIRNodeId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let node = unit.new_node(MIRNode::Termination { inst: termination });
        let block = unit.blocks.get_mut(insert_point.block).unwrap();

        if block.termination.is_some() {
            panic!("Block {} already has a termination", block.name);
        } else {
            block.termination = Some(node);
        }
        node
    }

    pub fn build_add(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Add { lhs, rhs })
    }

    pub fn build_sub(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Sub { lhs, rhs })
    }

    pub fn build_mul(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Mul { lhs, rhs })
    }

    pub fn build_div(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Div { lhs, rhs })
    }

    pub fn build_rem(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Rem { lhs, rhs })
    }

    pub fn build_bitwise_and(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::BitAnd { lhs, rhs })
    }

    pub fn build_bitwise_or(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::BitOr { lhs, rhs })
    }

    pub fn build_bitwise_xor(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::BitXor { lhs, rhs })
    }

    pub fn build_bitwise_not(&mut self, value: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::BitNot { value })
    }

    pub fn build_shl(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Shl { lhs, rhs })
    }

    pub fn build_shr(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Shr { lhs, rhs })
    }

    pub fn build_eq(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Eq { lhs, rhs })
    }

    pub fn build_neq(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Neq { lhs, rhs })
    }

    pub fn build_lt(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Lt { lhs, rhs })
    }

    pub fn build_gt(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Gt { lhs, rhs })
    }

    pub fn build_leq(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Leq { lhs, rhs })
    }

    pub fn build_geq(&mut self, lhs: MIRNodeId, rhs: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Geq { lhs, rhs })
    }

    pub fn build_not(&mut self, value: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Not { value })
    }

    pub fn build_neg(&mut self, value: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Neg { value })
    }

    pub fn build_cast(&mut self, value: MIRNodeId, ty: TypeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Cast { value, ty })
    }

    pub fn build_load(&mut self, ptr: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Load { ptr })
    }

    pub fn build_store(&mut self, ptr: MIRNodeId, value: MIRNodeId) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Store { ptr, value })
    }

    pub fn build_call(&mut self, callee: MIRNodeId, args: Vec<MIRNodeId>) -> MIRNodeId {
        self.create_instruction(MIRInstruction::Call { callee, args })
    }

    pub fn build_return(&mut self, value: Option<MIRNodeId>) -> MIRNodeId {
        self.create_termination(MIRTermination::Return { value })
    }

    pub fn build_branch(
        &mut self,
        cond: MIRNodeId,
        then: MIRBlockId,
        r#else: MIRBlockId,
    ) -> MIRNodeId {
        self.create_termination(MIRTermination::Branch { cond, then, r#else })
    }

    pub fn build_jump(&mut self, target: MIRBlockId) -> MIRNodeId {
        self.create_termination(MIRTermination::Jump { target })
    }

    pub fn build_jump_if(&mut self, cond: MIRNodeId, then: MIRBlockId) -> MIRNodeId {
        self.create_termination(MIRTermination::JumpIf { cond, then })
    }

    pub fn print_value(&self, ctx: &MIRContext, node_id: MIRNodeId) {
        let unit = self.package.unit(self.insert_point.unit).unwrap();
        let node = unit.nodes.get(node_id).unwrap();
        match node {
            MIRNode::Instruction { .. } => {
                eprint!("%{:?}", node_id.data());
            }
            MIRNode::Termination { .. } => {
                eprint!("%{:?}", node_id.data());
            }
            MIRNode::Value { value } => {
                value.print(ctx, &self.package, self.insert_point.unit);
                // eprint!(" {}", value);
            }
        }
    }

    pub fn print_node(&self, ctx: &MIRContext, node: MIRNodeId) {
        let unit = self.package.unit(self.insert_point.unit).unwrap();
        let node = unit.nodes.get(node).unwrap();
        match node {
            MIRNode::Instruction { inst } => match inst {
                MIRInstruction::Cast { value, ty } => {
                    eprint!("cast ");
                    self.print_value(ctx, *value);
                    eprint!(" to ");
                    ctx.get_type(*ty).unwrap().print(ctx);
                    eprintln!()
                }
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
            MIRNode::Termination { inst } => match inst {
                MIRTermination::Return { value } => {
                    eprint!("  return ");
                    if let Some(value) = value {
                        self.print_value(ctx, *value);
                    }
                    eprintln!()
                }
                MIRTermination::Branch { cond, then, r#else } => {
                    eprint!("  branch ");
                    self.print_value(ctx, *cond);
                    eprint!(
                        " {} ",
                        self.package
                            .unit(self.insert_point.unit)
                            .unwrap()
                            .blocks
                            .get(*then)
                            .unwrap()
                            .name
                    );
                    eprintln!(
                        "{}",
                        self.package
                            .unit(self.insert_point.unit)
                            .unwrap()
                            .blocks
                            .get(*r#else)
                            .unwrap()
                            .name
                    );
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

        if let MIRItem::Function { name, body, ty } = func {
            eprint!("fn {}", name);
            let ty = ctx.types.get(*ty).unwrap();
            match ty {
                Type::Function(f) => {
                    eprint!("(");
                    for (i, arg) in f.params.iter().enumerate() {
                        if i != 0 {
                            eprint!(", ");
                        }
                        ctx.get_type(*arg).unwrap().print(ctx);
                    }
                    eprint!(")");
                    if let Some(ret) = f.ret {
                        eprint!(" -> ");
                        ctx.get_type(ret).unwrap().print(ctx);
                    }
                }
                _ => panic!(),
            }
            eprintln!(" {{");
            for (name, block) in body {
                eprintln!("{}:", name);
                self.print_block(ctx, *block);
            }
            eprintln!("}}");
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
        let entry = builder.create_block(func, "entry".to_string());
        builder.set_insert_block(entry);

        let lhs = builder.create_value(MIRValue::Literal {
            ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
            value: Literal::Int(5),
        });
        let rhs = builder.create_value(MIRValue::Literal {
            ty: ctx.types.get_or_intern(Type::Primitive(Primitive::U32)),
            value: Literal::Int(50),
        });
        let res = builder.build_add(lhs, rhs);
        let retval = builder.build_add(res, rhs);

        let cmp = builder.build_gt(lhs, rhs);

        let b2 = builder.create_block(func, "if_true".to_string());
        let b3 = builder.create_block(func, "else".to_string());
        builder.build_branch(cmp, b2, b3);

        builder.set_insert_block(b2);
        builder.build_return(Some(retval));

        builder.set_insert_block(b3);
        let rhs = builder.create_value(MIRValue::Literal {
            ty: ctx.intern_type(Type::Primitive(Primitive::U32)),
            value: Literal::Int(10),
        });
        let new_ret = builder.build_shl(retval, rhs);
        builder.build_return(Some(new_ret));

        builder.print_function(&ctx, func);
        if std::env::var("FORCE_FAIL").is_ok() {
            assert!(false)
        }
    }
}
