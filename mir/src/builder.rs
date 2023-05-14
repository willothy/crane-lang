use std::{cell::RefCell, rc::Rc};

use crane_lex::{Literal, Primitive};
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
    pub ctx: Rc<RefCell<MIRContext>>,
    pub insert_point: InsertInfo,
}

impl MIRBuilder {
    pub fn new(ctx: Rc<RefCell<MIRContext>>) -> Self {
        Self {
            package: MIRPackage::new(),
            insert_point: InsertInfo {
                ..Default::default()
            },
            ctx,
        }
    }

    pub fn create_unit(&mut self, name: String, parent: Option<MIRUnitId>) -> MIRUnitId {
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

    pub fn get_or_insert_function(&mut self, name: String, ty: Type) -> MIRItemId {
        let unit = self.package.unit_mut(self.insert_point.unit).unwrap();

        let fn_ty = self.ctx.borrow_mut().types.get_or_intern(ty);

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
        if !func.is_function() {
            panic!("Item {func:?} is not a function");
        }
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

    fn create_instruction(&mut self, instruction: MIRInstruction) -> MIRNodeId {
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

    pub fn build_u8(&mut self, value: u8) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U8));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_u16(&mut self, value: u16) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U16));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_u32(&mut self, value: u32) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U32));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_u64(&mut self, value: u64) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U64));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i8(&mut self, value: i8) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I8));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i16(&mut self, value: i16) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I16));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i32(&mut self, value: i32) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I32));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i64(&mut self, value: i64) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I64));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_f32(&mut self, ctx: &mut MIRContext, value: f32) -> MIRNodeId {
        let ty = ctx.intern_type(Type::Primitive(Primitive::F32));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Float(value as f64),
        })
    }

    pub fn build_f64(&mut self, value: f64) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::F64));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Float(value),
        })
    }

    pub fn build_bool(&mut self, value: bool) -> MIRNodeId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::Bool));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Bool(value),
        })
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

    pub fn print_value(&self, node_id: MIRNodeId) {
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
                value.print(self.ctx.clone(), &self.package, self.insert_point.unit);
                // eprint!(" {}", value);
            }
        }
    }

    pub fn print_node(&self, node: MIRNodeId) {
        let unit = self.package.unit(self.insert_point.unit).unwrap();
        let node = unit.nodes.get(node).unwrap();
        match node {
            MIRNode::Instruction { inst } => match inst {
                MIRInstruction::Cast { value, ty } => {
                    eprint!("cast ");
                    self.print_value(*value);
                    eprint!(" to ");
                    self.ctx
                        .borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(self.ctx.clone());
                    eprintln!()
                }
                MIRInstruction::Add { lhs, rhs } => {
                    eprint!("add ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Sub { lhs, rhs } => {
                    eprint!("sub ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Mul { lhs, rhs } => {
                    eprint!("mul ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Div { lhs, rhs } => {
                    eprint!("div ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Rem { lhs, rhs } => {
                    eprint!("mod ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Neg { value } => {
                    eprint!("neg ");
                    self.print_node(*value);
                    eprintln!()
                }
                MIRInstruction::Not { value } => {
                    eprint!("not ");
                    self.print_node(*value);
                    eprintln!()
                }
                MIRInstruction::BitAnd { lhs, rhs } => {
                    eprint!("band ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::BitOr { lhs, rhs } => {
                    eprint!("bor ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::BitXor { lhs, rhs } => {
                    eprint!("xor ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::BitNot { value } => {
                    eprint!("bnot ");
                    self.print_node(*value);
                    eprintln!()
                }
                MIRInstruction::Shl { lhs, rhs } => {
                    eprint!("shl ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Shr { lhs, rhs } => {
                    eprint!("shr ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Eq { lhs, rhs } => {
                    eprint!("eq ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Neq { lhs, rhs } => {
                    eprint!("neq ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Lt { lhs, rhs } => {
                    eprint!("lt ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Leq { lhs, rhs } => {
                    eprint!("leq ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Gt { lhs, rhs } => {
                    eprint!("gt ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Geq { lhs, rhs } => {
                    eprint!("geq ");
                    self.print_value(*lhs);
                    eprint!(" ");
                    self.print_value(*rhs);
                    eprintln!()
                }
                MIRInstruction::Alloc { ty } => {
                    eprint!("alloc ");
                    self.ctx
                        .borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(self.ctx.clone());
                    eprintln!()
                }
                MIRInstruction::Load { ptr } => {
                    eprint!("load ");
                    self.print_value(*ptr);
                    eprintln!()
                }
                MIRInstruction::Store { ptr, value } => {
                    eprint!("store ");
                    self.print_value(*ptr);
                    eprint!(" ");
                    self.print_value(*value);
                    eprintln!()
                }
                MIRInstruction::Call { callee, args } => {
                    eprint!("call ");
                    self.print_value(*callee);
                    eprint!(" ");
                    for arg in args {
                        self.print_value(*arg);
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
                        self.print_value(*value);
                    }
                    eprintln!()
                }
                MIRTermination::Branch { cond, then, r#else } => {
                    eprint!("  branch ");
                    self.print_value(*cond);
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

    pub fn print_block(&self, block: MIRBlockId) {
        let insert_point = &self.insert_point;
        let unit = self.package.unit(insert_point.unit).unwrap();
        let block = unit.blocks.get(block).unwrap();

        for node in &block.body {
            eprint!("  %{:?} = ", node.data());
            self.print_node(*node);
        }
        if let Some(termination) = &block.termination {
            self.print_node(*termination)
        }
    }

    pub fn print_function(&self, func: MIRItemId) {
        let insert_point = &self.insert_point;
        let unit = self.package.unit(insert_point.unit).unwrap();
        let func = unit.items.get(func).unwrap();

        if let MIRItem::Function { name, body, ty } = func {
            eprint!("fn {}", name);
            let _ctx = self.ctx.borrow();
            let ty = _ctx.types.get(*ty).unwrap();
            match ty {
                Type::Function(f) => {
                    eprint!("(");
                    for (i, arg) in f.params.iter().enumerate() {
                        if i != 0 {
                            eprint!(", ");
                        }
                        self.ctx
                            .borrow()
                            .get_type(*arg)
                            .unwrap()
                            .print(self.ctx.clone());
                    }
                    eprint!(")");
                    if let Some(ret) = f.ret {
                        eprint!(" -> ");
                        self.ctx
                            .borrow()
                            .get_type(ret)
                            .unwrap()
                            .print(self.ctx.clone());
                    }
                }
                _ => panic!(),
            }
            eprintln!(" {{");
            for (name, block) in body {
                eprintln!("{}:", name);
                self.print_block(*block);
            }
            eprintln!("}}");
        }
    }
}

#[cfg(test)]
pub mod tests {
    use std::rc::Rc;

    use crane_lex::Primitive;

    use super::*;
    use crate::{ty::FunctionType, MIRContext};

    #[test]
    fn basic() {
        let ctx = Rc::new(RefCell::new(MIRContext::new()));
        let mut builder = MIRBuilder::new(ctx.clone());
        let unit = builder.create_unit("test".to_string(), None);
        // make sure to set the insert unit!
        builder.set_insert_point(unit, MIRBlockId::default(), InsertPoint::End);
        let u32_ty = ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U32));
        let func = builder.get_or_insert_function(
            "test_func".to_string(),
            Type::Function(FunctionType {
                params: vec![],
                ret: Some(u32_ty),
            }),
        );
        let entry = builder.create_block(func, "entry".to_string());
        builder.set_insert_block(entry);

        let lhs = builder.build_u32(5);
        let rhs = builder.build_u32(50);
        // let rhs = builder.build_u
        let res = builder.build_add(lhs, rhs);
        let retval = builder.build_add(res, rhs);

        let cmp = builder.build_gt(lhs, rhs);

        let b2 = builder.create_block(func, "if_true".to_string());
        let b3 = builder.create_block(func, "else".to_string());
        builder.build_branch(cmp, b2, b3);

        builder.set_insert_block(b2);
        builder.build_return(Some(retval));

        builder.set_insert_block(b3);
        let rhs = builder.build_u32(10);
        let new_ret = builder.build_shl(retval, rhs);
        builder.build_return(Some(new_ret));

        builder.print_function(func);
        if std::env::var("FORCE_FAIL").is_ok() {
            assert!(false)
        }
    }
}
