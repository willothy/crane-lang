use std::{cell::RefCell, rc::Rc};

use crane_parse::{
    expr::Expr,
    package::{pass::Inspect, ASTPackage},
    ty::Signature,
    unit::{NodeId, Unit as UnitTrait},
};

use crate::{
    builder::MIRBuilder, scope::ScopeStack, ty::Type, Context, TypeId, Unit, UnitId, ValueId,
};

pub struct ASTTransformationPass {
    builder: MIRBuilder,
    context: Context,
}

impl ASTTransformationPass {
    pub fn new() -> Self {
        Self {
            builder: MIRBuilder::new(Rc::new(RefCell::new(Context::new()))),
            context: Context::new(),
        }
    }

    pub fn resolve_signature(&mut self, sig: &Signature) -> TypeId {
        match sig {
            Signature::Function { params, ret_ty } => {
                let params = params
                    .into_iter()
                    .map(|sig| self.resolve_signature(sig))
                    .collect();
                let ret = ret_ty
                    .as_ref()
                    .map(|sig| self.resolve_signature(sig.as_ref()));
                self.context
                    .intern_type(Type::Function(crate::ty::FunctionType { params, ret }))
            }
            Signature::Primitive(p) => self.context.intern_type(Type::Primitive(p.clone())),
            Signature::Pointer(inner) => {
                let ty = self.resolve_signature(inner.as_ref());
                self.context.intern_type(Type::Pointer(ty))
            }
            Signature::Array(ty, len) => {
                let ty = self.resolve_signature(ty.as_ref());
                self.context.intern_type(Type::Array(ty, *len))
            }
            Signature::Tuple(types) => {
                let types = types
                    .into_iter()
                    .map(|sig| self.resolve_signature(sig))
                    .collect();
                self.context.intern_type(Type::Tuple(types))
            }
            Signature::Name(_) => todo!(),
        }
    }

    pub fn build_function(
        &mut self,
        package: &ASTPackage,
        ast_unit: crane_parse::unit::UnitId,
        unit: crate::UnitId,
        name: String,
        params: Vec<(String, Signature)>,
        ret_ty: Option<Signature>,
        body: NodeId,
    ) {
        let ast_unit = package.unit(ast_unit).unwrap();
        let body = ast_unit.node(body).unwrap();
        let params = params
            .into_iter()
            .map(|(_name, sig)| {
                let ty = self.resolve_signature(&sig);
                ty
            })
            .collect();

        let ret = ret_ty.map(|r| self.resolve_signature(&r));
        self.builder.set_insert_unit(unit);
        let func = self.builder.get_or_insert_function(
            name,
            Type::Function(crate::ty::FunctionType { params, ret }),
        );
        let entry = self.builder.create_block(func, "entry".to_owned());
        self.builder.set_insert_block(entry);

        let scope = ScopeStack::new();
        match body {
            crane_parse::ASTNode::Expr(e) => match e {
                crane_parse::expr::Expr::Block { exprs } => {
                    for expr in exprs {
                        self.build_stmt(package, ast_unit, unit, scope.clone(), *expr);
                    }
                }
                _ => todo!(),
            },
            crane_parse::ASTNode::Item(_) => panic!(),
            crane_parse::ASTNode::Error => panic!(),
        }
    }

    fn build_stmt(
        &mut self,
        package: &crane_parse::package::Package<
            crane_parse::unit::UnitId,
            crane_parse::unit::ASTUnit,
            NodeId,
            crane_parse::ASTNode,
            NodeId,
        >,
        ast_unit: &crane_parse::unit::ASTUnit,
        unit: UnitId,
        mut scope: ScopeStack,
        expr: NodeId,
    ) -> crate::ValueId {
        let expr = ast_unit.node(expr).unwrap();

        match expr {
            crane_parse::ASTNode::Expr(e) => match e {
                crane_parse::expr::Expr::Literal(l) => match l {
                    crane_lex::Literal::Int(i) => self.builder.build_i64(*i as i64),
                    crane_lex::Literal::Float(f) => self.builder.build_f64(*f),
                    crane_lex::Literal::String(s) => self.builder.build_string(s.clone()),
                    crane_lex::Literal::Char(c) => self.builder.build_char(*c),
                    crane_lex::Literal::Bool(b) => self.builder.build_bool(*b),
                },
                crane_parse::expr::Expr::Let { lhs, ty, value } => {
                    let inner = self.resolve_signature(ty.as_ref().unwrap());
                    let ty = self.context.intern_type(Type::Pointer(inner));
                    let lhs = ast_unit.node(*lhs).unwrap();
                    match lhs {
                        crane_parse::ASTNode::Expr(Expr::Ident(name)) => {
                            let store = self.builder.build_alloc(ty);
                            let initialized = if let Some(value) = value {
                                let value = self.build_expr(
                                    package,
                                    ast_unit,
                                    unit,
                                    scope.make_child(),
                                    *value,
                                );
                                self.builder.build_store(store, value);
                                true
                            } else {
                                false
                            };
                            scope.insert(
                                name.clone(),
                                crate::scope::ScopeItem::Variable {
                                    name: name.clone(),
                                    ty,
                                    value: store,
                                    initialized,
                                },
                            );
                            store
                        }
                        _ => panic!(),
                    }
                }
                crane_parse::expr::Expr::Ident(i) => {
                    let item = scope.get(i).unwrap();
                    match item {
                        crate::scope::ScopeItem::Variable { value, .. } => value,
                        _ => panic!(),
                    }
                }
                crane_parse::expr::Expr::StructInit { ty, fields } => todo!(),
                crane_parse::expr::Expr::Call { callee, args } => todo!(),
                crane_parse::expr::Expr::MemberAccess {
                    object,
                    member,
                    computed,
                } => todo!(),
                crane_parse::expr::Expr::UnaryOp { op, operand } => todo!(),
                crane_parse::expr::Expr::BinaryOp { op, lhs, rhs } => {
                    // todo
                    let lhs = self.build_expr(package, ast_unit, unit, scope.clone(), *lhs);
                    let rhs = self.build_expr(package, ast_unit, unit, scope.clone(), *rhs);
                    match op {
                        crane_parse::ops::BinaryOp::Add => self.builder.build_add(lhs, rhs),
                        crane_parse::ops::BinaryOp::Sub => self.builder.build_sub(lhs, rhs),
                        crane_parse::ops::BinaryOp::Mul => self.builder.build_mul(lhs, rhs),
                        crane_parse::ops::BinaryOp::Div => self.builder.build_div(lhs, rhs),
                        crane_parse::ops::BinaryOp::Mod => self.builder.build_rem(lhs, rhs),
                        crane_parse::ops::BinaryOp::Eq => self.builder.build_eq(lhs, rhs),
                        crane_parse::ops::BinaryOp::Neq => self.builder.build_neq(lhs, rhs),
                        crane_parse::ops::BinaryOp::Lt => self.builder.build_lt(lhs, rhs),
                        crane_parse::ops::BinaryOp::Gt => self.builder.build_gt(lhs, rhs),
                        crane_parse::ops::BinaryOp::Leq => self.builder.build_leq(lhs, rhs),
                        crane_parse::ops::BinaryOp::Geq => self.builder.build_geq(lhs, rhs),
                        crane_parse::ops::BinaryOp::Xor => self.builder.build_bitwise_xor(lhs, rhs),
                        crane_parse::ops::BinaryOp::BitwiseAnd => {
                            self.builder.build_bitwise_and(lhs, rhs)
                        }
                        crane_parse::ops::BinaryOp::BitwiseOr => {
                            self.builder.build_bitwise_or(lhs, rhs)
                        }
                        crane_parse::ops::BinaryOp::ShiftLeft => self.builder.build_shl(lhs, rhs),
                        crane_parse::ops::BinaryOp::ShiftRight => self.builder.build_shr(lhs, rhs),
                        crane_parse::ops::BinaryOp::And => todo!(),
                        crane_parse::ops::BinaryOp::Or => todo!(),
                        crane_parse::ops::BinaryOp::Cast => todo!(),
                    }
                }
                crane_parse::expr::Expr::Assignment { lhs, op, rhs } => {
                    todo!()
                }
                crane_parse::expr::Expr::Break { value } => todo!(),
                crane_parse::expr::Expr::Return { value } => todo!(),
                crane_parse::expr::Expr::Result { value } => todo!(),
                crane_parse::expr::Expr::Continue => todo!(),
                crane_parse::expr::Expr::Cast { ty, expr } => todo!(),
                crane_parse::expr::Expr::Block { exprs } => {
                    todo!()
                    // let block = self.builder.create_block();
                }
                crane_parse::expr::Expr::If { cond, then, r#else } => todo!(),
                crane_parse::expr::Expr::While { cond, body } => todo!(),
                crane_parse::expr::Expr::Loop { body } => todo!(),
                crane_parse::expr::Expr::ScopeResolution { path } => todo!(),
                crane_parse::expr::Expr::List { exprs } => todo!(),
                crane_parse::expr::Expr::Closure {
                    params,
                    ret_ty,
                    body,
                } => todo!(),
                crane_parse::expr::Expr::Tuple { exprs } => todo!(),
            },
            _ => panic!(),
        }
    }

    fn build_expr(
        &self,
        package: &crane_parse::package::Package<
            crane_parse::unit::UnitId,
            crane_parse::unit::ASTUnit,
            NodeId,
            crane_parse::ASTNode,
            NodeId,
        >,
        ast_unit: &crane_parse::unit::ASTUnit,
        unit: UnitId,
        scope: ScopeStack,
        value: NodeId,
    ) -> ValueId {
        todo!()
    }
}

impl Inspect for ASTTransformationPass {
    type Scope = ASTPackage;

    type Input = (crane_parse::unit::UnitId, Option<UnitId>);

    fn inspect(&mut self, scope: &Self::Scope, input: Self::Input) {
        let unit_id = input.0;
        let unit = scope.unit(unit_id).unwrap();
        let mir_unit_id = self.builder.create_unit(unit.name().to_owned(), input.1);
        unit.members().iter().for_each(|(name, node)| {
            let node = unit.node(*node).unwrap();
            match node {
                crane_parse::ASTNode::Item(i) => match i {
                    crane_parse::item::Item::Submodule { vis, name, id } => {
                        self.inspect(scope, (*id, Some(mir_unit_id)))
                    }
                    crane_parse::item::Item::FunctionDef {
                        vis,
                        name,
                        params,
                        ret_ty,
                        body,
                    } => self.build_function(
                        &scope,
                        unit_id,
                        mir_unit_id,
                        name.clone(),
                        params.clone(),
                        ret_ty.clone(),
                        *body,
                    ),
                    crane_parse::item::Item::FunctionDecl {
                        vis,
                        name,
                        args,
                        ret_ty,
                    } => todo!(),
                    crane_parse::item::Item::StructDef { vis, name, fields } => {
                        // TODO
                        // self
                        //     .context
                        //     .intern_type(crate::ty::Type::Struct(crate::ty::StructType {
                        //         fields: fields,
                        //     }))
                    }
                    _ => {
                        // TODO
                    }
                },
                _ => {}
            }
        });
    }
}
