use std::{cell::RefCell, rc::Rc};

use crane_lex::{Literal, Primitive};
use crane_parse::{package::pass::Inspect, unit::Unit as UnitTrait};
use linked_hash_map::LinkedHashMap;
use slotmap::Key;

use crate::{
    instruction::{MIRInstruction, MIRNode, MIRTermination, MIRValue},
    ty::Type,
    Block, BlockId, Context, Item, ItemId, MIRPackage, TypeId, Unit, UnitId, ValueId,
};

#[derive(Debug, Default)]
pub enum InsertPoint {
    Before(ValueId),
    After(ValueId),
    Start,
    #[default]
    End,
}

#[derive(Debug, Default)]
pub struct InsertInfo {
    pub unit: UnitId,
    pub block: BlockId,
    pub point: InsertPoint,
}
pub struct MIRBuilder {
    pub package: MIRPackage,
    pub ctx: Rc<RefCell<Context>>,
    pub insert_point: InsertInfo,
}

pub struct FnGraphVizPass {
    result: String,
}

impl Inspect for FnGraphVizPass {
    type Scope = MIRPackage;

    type Input = (UnitId, ItemId);

    fn inspect(&mut self, scope: &Self::Scope, input: Self::Input) {
        let unit = scope.unit(input.0).unwrap();
        let func = unit.items.get(input.1).unwrap();
        let Item::Function { name: fn_name, ty: _, body } = func else {
            panic!()
        };
        let mut nodes = vec![];
        let mut edges = vec![];
        for (i, (block_name, block_id)) in body.iter().enumerate() {
            let node = format!("N{i}[label=\"{}\", shape=\"box\"];", block_name);
            nodes.push(node);
            match unit.blocks.get(*block_id).unwrap().termination {
                Some(term) => match unit.node(term).unwrap() {
                    MIRNode::Termination { inst } => match inst {
                        MIRTermination::Return { value: _ } => {}
                        MIRTermination::Branch { cond, then, r#else } => {
                            let then_name = &unit.blocks.get(*then).unwrap().name;
                            let then = body.iter().position(|v| v.0 == then_name).unwrap();

                            let else_name = &unit.blocks.get(*r#else).unwrap().name;
                            let r#else = body.iter().position(|v| v.0 == else_name).unwrap();
                            edges.push(format!("N{} -> N{}[label=\"{:?}\"]", i, then, cond));
                            edges.push(format!("N{} -> N{}[label=\"!{:?}\"]", i, r#else, cond));
                        }
                        MIRTermination::Jump { target } => {
                            let target_name = &unit.blocks.get(*target).unwrap().name;
                            let target = body.iter().position(|v| v.0 == target_name).unwrap();
                            edges.push(format!("N{} -> N{}", i, target));
                        }
                        MIRTermination::JumpIf { cond, then } => {
                            let then_name = &unit.blocks.get(*then).unwrap().name;
                            let then = body.iter().position(|v| v.0 == then_name).unwrap();
                            edges.push(format!("N{} -> N{}[label=\"{:?}\"]", i, then, cond));
                        }
                    },
                    _ => panic!("Expected termination"),
                },
                None => {}
            }
            let mut lines = vec![];
            lines.push(format!("digraph {} {{", fn_name));
            for node in nodes.iter() {
                lines.push(format!("    {}", node));
            }
            for edge in edges.iter() {
                lines.push(format!("    {}", edge));
            }
            lines.push("}".to_string());
            self.result = lines.join("\n");
        }
    }
}

impl MIRBuilder {
    pub fn new(ctx: Rc<RefCell<Context>>) -> Self {
        Self {
            package: MIRPackage::new(),
            insert_point: InsertInfo {
                ..Default::default()
            },
            ctx,
        }
    }

    pub fn create_unit(&mut self, name: String, parent: Option<UnitId>) -> UnitId {
        let unit = self.package.add_unit(Unit::new(name, parent));
        if parent.is_none() {
            self.package.set_root(unit);
        }
        unit
    }

    pub fn get_unit(&self, id: UnitId) -> Option<&Unit> {
        self.package.unit(id)
    }

    pub fn get_unit_mut(&mut self, id: UnitId) -> Option<&mut Unit> {
        self.package.unit_mut(id)
    }

    pub fn get_or_insert_function(&mut self, name: String, ty: Type) -> ItemId {
        let unit = self.package.unit_mut(self.insert_point.unit).unwrap();

        let fn_ty = self.ctx.borrow_mut().types.get_or_intern(ty);

        let item = Item::Function {
            name: name.clone(),
            ty: fn_ty,
            body: LinkedHashMap::new(),
        };

        let item_id = unit.items.insert(item);
        unit.members_mut().insert(name, item_id);

        item_id
    }

    pub fn set_insert_point(&mut self, unit: UnitId, block: BlockId, point: InsertPoint) {
        self.insert_point = InsertInfo { unit, block, point };
    }

    pub fn set_insert_unit(&mut self, unit: UnitId) {
        self.insert_point.unit = unit;
    }

    pub fn set_insert_block(&mut self, block: BlockId) {
        self.insert_point.block = block;
    }

    pub fn get_block_by_name(&self, func: ItemId, name: String) -> Option<&BlockId> {
        let insert_unit = self.insert_point.unit;
        let unit = self.package.unit(insert_unit)?;
        let func = unit.items.get(func);
        if let Some(Item::Function { body, .. }) = func {
            body.get(&name)
        } else {
            None
        }
    }

    pub fn create_block(&mut self, func: ItemId, name: String) -> BlockId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let func = unit.items.get_mut(func).unwrap();
        if !func.is_function() {
            panic!("Item {func:?} is not a function");
        }
        let block = unit.blocks.insert(Block {
            name: name.clone(),
            body: vec![],
            termination: None,
        });

        if let Item::Function { body, .. } = func {
            body.insert(name, block);
        }
        block
    }

    fn create_instruction(&mut self, instruction: MIRInstruction) -> ValueId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        let id = unit.new_node(MIRNode::Instruction { inst: instruction });
        let block = unit.blocks.get_mut(insert_point.block).unwrap();

        block.body.push(id);
        id
    }

    pub fn create_value(&mut self, value: MIRValue) -> ValueId {
        let insert_point = &self.insert_point;
        let unit = self.package.unit_mut(insert_point.unit).unwrap();
        unit.new_node(MIRNode::Value { value })
    }

    pub fn create_termination(&mut self, termination: MIRTermination) -> ValueId {
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

    pub fn build_struct(&mut self, ty: TypeId, fields: Vec<(String, ValueId)>) -> ValueId {
        self.create_value(MIRValue::StructInit { ty, fields })
    }

    pub fn build_tuple(&mut self, ty: TypeId, elements: Vec<ValueId>) -> ValueId {
        self.create_value(MIRValue::TupleInit { ty, elements })
    }

    pub fn build_array(&mut self, ty: TypeId, elements: Vec<ValueId>) -> ValueId {
        self.create_value(MIRValue::ArrayInit { ty, elements })
    }

    pub fn build_string(&mut self, value: String) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::Str));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::String(value),
        })
    }

    pub fn build_u8(&mut self, value: u8) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U8));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_u16(&mut self, value: u16) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U16));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_u32(&mut self, value: u32) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U32));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_u64(&mut self, value: u64) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::U64));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i8(&mut self, value: i8) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I8));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i16(&mut self, value: i16) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I16));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i32(&mut self, value: i32) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I32));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_i64(&mut self, value: i64) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::I64));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Int(value as isize),
        })
    }

    pub fn build_f32(&mut self, ctx: &mut Context, value: f32) -> ValueId {
        let ty = ctx.intern_type(Type::Primitive(Primitive::F32));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Float(value as f64),
        })
    }

    pub fn build_f64(&mut self, value: f64) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::F64));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Float(value),
        })
    }

    pub fn build_bool(&mut self, value: bool) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::Bool));
        self.create_value(MIRValue::Literal {
            ty,
            value: Literal::Bool(value),
        })
    }

    pub fn build_add(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Add { lhs, rhs })
    }

    pub fn build_sub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Sub { lhs, rhs })
    }

    pub fn build_mul(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Mul { lhs, rhs })
    }

    pub fn build_div(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Div { lhs, rhs })
    }

    pub fn build_rem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Rem { lhs, rhs })
    }

    pub fn build_bitwise_and(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::BitAnd { lhs, rhs })
    }

    pub fn build_bitwise_or(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::BitOr { lhs, rhs })
    }

    pub fn build_bitwise_xor(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::BitXor { lhs, rhs })
    }

    pub fn build_bitwise_not(&mut self, value: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::BitNot { value })
    }

    pub fn build_shl(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Shl { lhs, rhs })
    }

    pub fn build_shr(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Shr { lhs, rhs })
    }

    pub fn build_eq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Eq { lhs, rhs })
    }

    pub fn build_neq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Neq { lhs, rhs })
    }

    pub fn build_lt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Lt { lhs, rhs })
    }

    pub fn build_gt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Gt { lhs, rhs })
    }

    pub fn build_leq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Leq { lhs, rhs })
    }

    pub fn build_geq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Geq { lhs, rhs })
    }

    pub fn build_not(&mut self, value: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Not { value })
    }

    pub fn build_neg(&mut self, value: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Neg { value })
    }

    pub fn build_cast(&mut self, value: ValueId, ty: TypeId) -> ValueId {
        self.create_instruction(MIRInstruction::Cast { value, ty })
    }

    pub fn build_load(&mut self, ptr: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Load { ptr })
    }

    pub fn build_store(&mut self, ptr: ValueId, value: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Store { ptr, value })
    }

    pub fn build_call(&mut self, callee: ValueId, args: Vec<ValueId>) -> ValueId {
        self.create_instruction(MIRInstruction::Call { callee, args })
    }

    pub fn build_return(&mut self, value: Option<ValueId>) -> ValueId {
        self.create_termination(MIRTermination::Return { value })
    }

    pub fn build_branch(&mut self, cond: ValueId, then: BlockId, r#else: BlockId) -> ValueId {
        self.create_termination(MIRTermination::Branch { cond, then, r#else })
    }

    pub fn build_jump(&mut self, target: BlockId) -> ValueId {
        self.create_termination(MIRTermination::Jump { target })
    }

    pub fn build_jump_if(&mut self, cond: ValueId, then: BlockId) -> ValueId {
        self.create_termination(MIRTermination::JumpIf { cond, then })
    }

    pub fn print_value(&self, node_id: ValueId) {
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
            }
        }
    }

    pub fn print_node(&self, node: ValueId) {
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
                #[allow(unused_variables)]
                MIRInstruction::FieldAccess { value, field } => todo!(),
                #[allow(unused_variables)]
                MIRInstruction::IndexAccess { value, index } => todo!(),
                #[allow(unused_variables)]
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
            #[allow(unused_variables)]
            MIRNode::Value { value } => panic!(),
        }
    }

    pub fn print_block(&self, block: BlockId) {
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

    pub fn print_function(&self, func: ItemId) {
        let insert_point = &self.insert_point;
        let unit = self.package.unit(insert_point.unit).unwrap();
        let func = unit.items.get(func).unwrap();

        if let Item::Function { name, body, ty } = func {
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
    use crate::{ty::FunctionType, Context};

    #[test]
    fn basic() {
        let ctx = Rc::new(RefCell::new(Context::new()));
        let mut builder = MIRBuilder::new(ctx.clone());
        let unit = builder.create_unit("test".to_string(), None);
        // make sure to set the insert unit!
        builder.set_insert_point(unit, BlockId::default(), InsertPoint::End);
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

        let mut dot = FnGraphVizPass {
            result: String::new(),
        };
        builder.package.inspect(&mut dot, (unit, func));
        println!("{}", dot.result);
        if std::env::var("DOT_OUTPUT").is_ok() {
            std::fs::write(
                env!("CARGO_MANIFEST_DIR").to_owned() + "/test.dot",
                dot.result,
            )
            .unwrap();
        }

        if std::env::var("FORCE_FAIL").is_ok() {
            assert!(false)
        }
    }
}
