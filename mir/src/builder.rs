use std::{cell::RefCell, fmt::Write, rc::Rc};

use crane_lex::{Literal, Primitive};
use crane_parse::unit::Unit as UnitTrait;
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
    pub loc: InsertPoint,
}

pub struct MIRBuilder {
    pub package: MIRPackage,
    pub ctx: Rc<RefCell<Context>>,
    pub insert_point: InsertInfo,
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
        self.insert_point = InsertInfo {
            unit,
            block,
            loc: point,
        };
    }

    pub fn set_insert_unit(&mut self, unit: UnitId) {
        self.insert_point.unit = unit;
    }

    pub fn set_insert_block(&mut self, block: BlockId) {
        self.insert_point.block = block;
        // If the insert point references a specific instruction, reset it to append to the new
        // insert block
        if matches!(
            self.insert_point.loc,
            InsertPoint::Before(_) | InsertPoint::After(_)
        ) {
            self.insert_point.loc = InsertPoint::End;
        }
    }

    pub fn set_insert_location(&mut self, loc: InsertPoint) {
        self.insert_point.loc = loc;
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

        match insert_point.loc {
            InsertPoint::Before(before_id) => {
                let index = block.body.iter().position(|v| *v == before_id).unwrap();
                block.body.insert(index, id)
            }
            InsertPoint::After(after_id) => {
                let index = block.body.iter().position(|v| *v == after_id).unwrap();
                block.body.insert(index + 1, id)
            }
            InsertPoint::Start => block.body.insert(0, id),
            InsertPoint::End => block.body.push(id),
        }
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

    pub fn build_f32(&mut self, value: f32) -> ValueId {
        let ty = self
            .ctx
            .borrow_mut()
            .intern_type(Type::Primitive(Primitive::F32));
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

    pub fn build_alloc(&mut self, ty: TypeId) -> ValueId {
        self.create_instruction(MIRInstruction::Alloc { ty })
    }

    pub fn build_load(&mut self, ptr: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Load { ptr })
    }

    pub fn build_store(&mut self, ptr: ValueId, value: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::Store { ptr, value })
    }

    /// The resulting ValueId will reference a pointer to the access location, so that
    /// `build_field_access` can be used with `build_load` and `build_store` instead of requiring
    /// struct-specific load and store instructions
    pub fn build_field_access(&mut self, value: ValueId, field: String) -> ValueId {
        self.create_instruction(MIRInstruction::FieldAccess { value, field })
    }

    /// The resulting ValueId will reference a pointer to the access location, so that
    /// `build_index_access` can be used with `build_load` and `build_store` instead of requiring
    /// array-specific load and store instructions
    pub fn build_index_access(&mut self, value: ValueId, index: ValueId) -> ValueId {
        self.create_instruction(MIRInstruction::IndexAccess { value, index })
    }

    /// The resulting ValueId will reference a pointer to the access location, so that
    /// `build_index_access` can be used with `build_load` and `build_store` instead of requiring
    /// array-specific load and store instructions
    pub fn build_tuple_access(&mut self, value: ValueId, index: usize) -> ValueId {
        self.create_instruction(MIRInstruction::TupleAccess { value, index })
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

    pub fn print_value(&self, node_id: ValueId, writer: &mut dyn Write) {
        let unit = self.package.unit(self.insert_point.unit).unwrap();
        let node = unit.nodes.get(node_id).unwrap();
        match node {
            MIRNode::Instruction { .. } => {
                write!(writer, "%{:?}", node_id.data()).unwrap();
            }
            MIRNode::Termination { .. } => {
                write!(writer, "%{:?}", node_id.data()).unwrap();
            }
            MIRNode::Value { value } => {
                value.print(
                    self.ctx.clone(),
                    &self.package,
                    self.insert_point.unit,
                    writer,
                );
            }
        }
    }

    pub fn print_node(&self, node: ValueId, writer: &mut dyn Write) {
        let unit = self.package.unit(self.insert_point.unit).unwrap();
        let node = unit.nodes.get(node).unwrap();
        match node {
            MIRNode::Instruction { inst } => match inst {
                MIRInstruction::Cast { value, ty } => {
                    write!(writer, "cast ");
                    self.print_value(*value, writer);
                    write!(writer, " to ");
                    self.ctx
                        .borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(self.ctx.clone(), writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Add { lhs, rhs } => {
                    write!(writer, "add ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Sub { lhs, rhs } => {
                    write!(writer, "sub ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Mul { lhs, rhs } => {
                    write!(writer, "mul ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Div { lhs, rhs } => {
                    write!(writer, "div ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Rem { lhs, rhs } => {
                    write!(writer, "rem ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Neg { value } => {
                    write!(writer, "neg ").unwrap();
                    self.print_node(*value, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Not { value } => {
                    write!(writer, "not ").unwrap();
                    self.print_node(*value, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::BitAnd { lhs, rhs } => {
                    write!(writer, "and ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::BitOr { lhs, rhs } => {
                    write!(writer, "bor ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::BitXor { lhs, rhs } => {
                    write!(writer, "xor ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::BitNot { value } => {
                    write!(writer, "bnot ").unwrap();
                    self.print_node(*value, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Shl { lhs, rhs } => {
                    write!(writer, "shl ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Shr { lhs, rhs } => {
                    write!(writer, "shr ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Eq { lhs, rhs } => {
                    write!(writer, "eq ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Neq { lhs, rhs } => {
                    write!(writer, "neq ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Lt { lhs, rhs } => {
                    write!(writer, "lt ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Leq { lhs, rhs } => {
                    write!(writer, "leq ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Gt { lhs, rhs } => {
                    write!(writer, "gt ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Geq { lhs, rhs } => {
                    write!(writer, "geq ").unwrap();
                    self.print_value(*lhs, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*rhs, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Alloc { ty } => {
                    write!(writer, "alloc ").unwrap();
                    self.ctx
                        .borrow()
                        .get_type(*ty)
                        .unwrap()
                        .print(self.ctx.clone(), writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Load { ptr } => {
                    write!(writer, "load ").unwrap();
                    self.print_value(*ptr, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Store { ptr, value } => {
                    write!(writer, "store ").unwrap();
                    self.print_value(*ptr, writer);
                    write!(writer, " ").unwrap();
                    self.print_value(*value, writer);
                    writeln!(writer).unwrap();
                }
                MIRInstruction::Call { callee, args } => {
                    write!(writer, "call ").unwrap();
                    self.print_value(*callee, writer);
                    write!(writer, " ").unwrap();
                    for arg in args {
                        self.print_value(*arg, writer);
                        write!(writer, " ").unwrap();
                    }
                    writeln!(writer).unwrap();
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
                    write!(writer, "  return ").unwrap();
                    if let Some(value) = value {
                        self.print_value(*value, writer);
                    }
                    writeln!(writer).unwrap();
                }
                MIRTermination::Branch { cond, then, r#else } => {
                    write!(writer, "  branch ").unwrap();
                    self.print_value(*cond, writer);
                    write!(
                        writer,
                        " {} ",
                        self.package
                            .unit(self.insert_point.unit)
                            .unwrap()
                            .blocks
                            .get(*then)
                            .unwrap()
                            .name
                    )
                    .unwrap();
                    writeln!(
                        writer,
                        "{}",
                        self.package
                            .unit(self.insert_point.unit)
                            .unwrap()
                            .blocks
                            .get(*r#else)
                            .unwrap()
                            .name
                    )
                    .unwrap();
                }
                MIRTermination::Jump { target } => writeln!(writer, "  jump {:?}", target).unwrap(),
                MIRTermination::JumpIf { cond, then } => {
                    writeln!(writer, "  jump_if: {:?} {:?}", cond, then).unwrap();
                }
            },
            #[allow(unused_variables)]
            MIRNode::Value { value } => panic!(),
        }
    }

    pub fn print_block(&self, block: BlockId, writer: &mut dyn Write) {
        let insert_point = &self.insert_point;
        let unit = self.package.unit(insert_point.unit).unwrap();
        let block = unit.blocks.get(block).unwrap();

        for node in &block.body {
            write!(writer, "  %{:?} = ", node.data()).unwrap();
            self.print_node(*node, writer);
        }
        if let Some(termination) = &block.termination {
            self.print_node(*termination, writer)
        }
    }

    pub fn print_function(&self, func: ItemId, writer: &mut dyn Write) {
        let insert_point = &self.insert_point;
        let unit = self.package.unit(insert_point.unit).unwrap();
        let func = unit.items.get(func).unwrap();

        if let Item::Function { name, body, ty } = func {
            write!(writer, "fn {}", name).unwrap();
            let _ctx = self.ctx.borrow();
            let ty = _ctx.types.get(*ty).unwrap();
            match ty {
                Type::Function(f) => {
                    write!(writer, "(").unwrap();
                    for (i, arg) in f.params.iter().enumerate() {
                        if i != 0 {
                            write!(writer, ", ").unwrap();
                        }
                        self.ctx
                            .borrow()
                            .get_type(*arg)
                            .unwrap()
                            .print(self.ctx.clone(), writer);
                    }
                    write!(writer, ")").unwrap();
                    if let Some(ret) = f.ret {
                        write!(writer, " -> ").unwrap();
                        self.ctx
                            .borrow()
                            .get_type(ret)
                            .unwrap()
                            .print(self.ctx.clone(), writer);
                    }
                }
                _ => panic!(),
            }
            writeln!(writer, " {{");
            for (name, block) in body {
                writeln!(writer, "{}:", name);
                self.print_block(*block, writer);
            }
            writeln!(writer, "}}");
        }
    }

    pub(crate) fn build_char(&self, c: char) -> ValueId {
        todo!()
    }
}

#[cfg(test)]
pub mod tests {
    use std::rc::Rc;

    use crane_lex::Primitive;

    use super::*;
    use crate::{graphviz::FnGraphVizPass, ty::FunctionType, Context};

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

        let mut writer = String::new();
        builder.print_function(func, &mut writer);
        println!("{}", writer);

        // generate graphviz for the test function
        let mut dot = FnGraphVizPass::new();
        builder.package.inspect(&mut dot, (unit, func, &builder));
        println!("{}", dot.result());
        if std::env::var("DOT_OUTPUT").is_ok() {
            std::fs::write(
                env!("CARGO_MANIFEST_DIR").to_owned() + "/test.dot",
                dot.result(),
            )
            .unwrap();
        }

        if std::env::var("FORCE_FAIL").is_ok() {
            assert!(false)
        }
    }
}
