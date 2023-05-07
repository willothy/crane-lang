use slotmap::SlotMap;

use crate::unit::{Unit, UnitId};

use self::pass::{Inspect, Transform};

#[derive(Debug)]
pub struct Package {
    units: SlotMap<UnitId, Unit>,
    root: UnitId,
}

impl Default for Package {
    fn default() -> Self {
        Self {
            units: SlotMap::with_key(),
            root: UnitId::default(),
        }
    }
}

pub mod pass {
    use std::{cell::RefCell, fmt::Write, rc::Rc};

    use crane_lex::Literal;

    use crate::{
        expr::Expr,
        item::Item,
        path::{ItemPath, PathPart},
        unit::{NodeId, UnitId},
        ASTNode,
    };

    use super::Package;

    pub trait Transform {
        type Scope;
        type Input;
        fn transform(&mut self, scope: &mut Self::Scope, input: Self::Input);
    }

    pub trait Inspect {
        type Scope;
        type Input;
        fn inspect(&mut self, scope: &Self::Scope, input: Self::Input);
    }

    pub struct PrintPackage;
    pub struct PrintUnit;
    pub struct PrintNode<'a> {
        _marker: std::marker::PhantomData<&'a ()>,
    }

    impl PrintNode<'_> {
        pub fn new() -> Self {
            Self {
                _marker: std::marker::PhantomData,
            }
        }
    }

    impl<'a> Inspect for PrintNode<'a> {
        type Scope = Package;
        type Input = (UnitId, NodeId, usize, Rc<RefCell<dyn Write>>, usize, bool);

        fn inspect(&mut self, scope: &Self::Scope, input: Self::Input) {
            let unit = input.0;
            let node = input.1;
            let indent = input.2;
            let out = input.3;
            let nested = input.4;
            let newline = input.5;
            let node = scope.units.get(unit).unwrap().get_node(node).unwrap();
            let indent_str = "  ".repeat(indent.checked_sub(1).unwrap_or(0));

            match node {
                ASTNode::Expr(e) => {
                    match e {
                        Expr::Literal(l) => {
                            let indent = if newline {
                                "  ".repeat(indent - 1)
                            } else {
                                "".to_owned()
                            };
                            match l {
                                Literal::Int(i) => write!(out.borrow_mut(), "{indent}{i}").unwrap(),
                                Literal::Float(f) => {
                                    write!(out.borrow_mut(), "{indent}{f}").unwrap()
                                }
                                Literal::String(s) => {
                                    write!(out.borrow_mut(), "\"{indent}{s}\"").unwrap()
                                }
                                Literal::Char(c) => {
                                    write!(out.borrow_mut(), "'{indent}{c}'").unwrap()
                                }
                                Literal::Bool(b) => {
                                    write!(out.borrow_mut(), "{indent}{b}").unwrap()
                                }
                            }
                        }
                        Expr::Ident(n) => write!(
                            out.borrow_mut(),
                            "{indent}{n}",
                            indent = if newline {
                                "  ".repeat(indent - 1)
                            } else {
                                "".to_owned()
                            }
                        )
                        .unwrap(),
                        Expr::StructInit { ty, fields } => {
                            write!(out.borrow_mut(), "{ty} {{", ty = ty).unwrap();
                            for (name, expr) in fields {
                                write!(out.borrow_mut(), "{name}: ", name = name).unwrap();
                                self.inspect(
                                    scope,
                                    (unit, *expr, indent, out.clone(), nested + 1, false),
                                );
                                write!(out.borrow_mut(), ", ").unwrap();
                            }
                            write!(out.borrow_mut(), "}}").unwrap();
                        }
                        Expr::Call { callee, args } => {
                            self.inspect(
                                scope,
                                (unit, *callee, indent, out.clone(), nested, false),
                            );
                            write!(out.borrow_mut(), "(").unwrap();
                            for (i, arg) in args.iter().enumerate() {
                                if i > 0 {
                                    write!(out.borrow_mut(), ", ").unwrap();
                                }
                                self.inspect(
                                    scope,
                                    (unit, *arg, indent, out.clone(), nested, false),
                                );
                            }
                            write!(out.borrow_mut(), ")").unwrap();
                        }
                        Expr::MemberAccess {
                            object,
                            member,
                            computed,
                        } => {
                            self.inspect(
                                scope,
                                (unit, *object, indent, out.clone(), nested + 1, false),
                            );
                            if *computed {
                                write!(out.borrow_mut(), "[").unwrap();
                                self.inspect(
                                    scope,
                                    (unit, *member, indent, out.clone(), nested + 1, false),
                                );
                                write!(out.borrow_mut(), "]").unwrap();
                            } else {
                                write!(out.borrow_mut(), ".").unwrap();
                                self.inspect(
                                    scope,
                                    (unit, *member, indent, out.clone(), nested + 1, false),
                                );
                            }
                        }
                        Expr::UnaryOp { op, operand } => {
                            write!(
                                out.borrow_mut(),
                                "{indent}{op}",
                                op = op,
                                indent = if newline {
                                    "  ".repeat(indent - 1)
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                            self.inspect(
                                scope,
                                (unit, *operand, indent, out.clone(), nested + 1, false),
                            );
                        }
                        Expr::BinaryOp { op, lhs, rhs } => {
                            if newline {
                                write!(out.borrow_mut(), "{}", "  ".repeat(indent - 1)).unwrap();
                            }
                            self.inspect(
                                scope,
                                (unit, *lhs, indent, out.clone(), nested + 1, false),
                            );
                            write!(out.borrow_mut(), " {op} ", op = op).unwrap();
                            self.inspect(
                                scope,
                                (unit, *rhs, indent, out.clone(), nested + 1, false),
                            );
                        }
                        Expr::Assignment { lhs, op, rhs } => {
                            if nested <= 3 {
                                write!(out.borrow_mut(), "{}", "  ".repeat(indent - 1)).unwrap();
                            }
                            self.inspect(
                                scope,
                                (unit, *lhs, indent, out.clone(), nested + 1, false),
                            );
                            write!(out.borrow_mut(), " {op} ", op = op).unwrap();
                            self.inspect(
                                scope,
                                (unit, *rhs, indent, out.clone(), nested + 1, false),
                            );
                        }
                        Expr::Cast { ty, expr } => {
                            write!(out.borrow_mut(), "({ty})", ty = ty).unwrap();
                            self.inspect(
                                scope,
                                (unit, *expr, indent, out.clone(), nested + 1, false),
                            );
                        }
                        Expr::Block { exprs: stmts } => {
                            writeln!(out.borrow_mut(), "{{").unwrap();
                            for stmt in stmts {
                                self.inspect(
                                    scope,
                                    (unit, *stmt, indent + 1, out.clone(), nested + 1, true),
                                );
                            }
                            write!(
                                out.borrow_mut(),
                                "{indent}}}",
                                indent = " ".repeat(indent - 1)
                            )
                            .unwrap();
                        }
                        Expr::If { cond, then, r#else } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}if ",
                                indent_str = if newline { indent_str } else { "".to_owned() }
                            )
                            .unwrap();
                            self.inspect(
                                scope,
                                (unit, *cond, indent, out.clone(), nested + 1, false),
                            );
                            write!(out.borrow_mut(), " ").unwrap();
                            self.inspect(
                                scope,
                                (
                                    unit,
                                    *then,
                                    indent + (1 - if indent == 0 { 1 } else { indent % 2 }),
                                    out.clone(),
                                    nested + 1,
                                    false,
                                ),
                            );
                            if let Some(r#else) = r#else {
                                write!(
                                    out.borrow_mut(),
                                    " else ",
                                    // indent = if newline { indent_str } else { "".to_owned() }
                                )
                                .unwrap();
                                self.inspect(
                                    scope,
                                    (unit, *r#else, indent, out.clone(), nested + 1, false),
                                );
                            }
                        }
                        Expr::While { cond, body } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}while ",
                                indent_str = indent_str
                            )
                            .unwrap();
                            self.inspect(
                                scope,
                                (unit, *cond, indent, out.clone(), nested + 1, false),
                            );
                            write!(out.borrow_mut(), " ").unwrap();
                            self.inspect(
                                scope,
                                (unit, *body, indent + 1, out.clone(), nested + 1, false),
                            );
                        }
                        Expr::Loop { body } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}loop ",
                                indent_str = indent_str
                            )
                            .unwrap();
                            self.inspect(
                                scope,
                                (unit, *body, indent + 1, out.clone(), nested + 1, false),
                            );
                        }
                        Expr::ScopeResolution { path } => {
                            // self.inspect(
                            //     scope,
                            //     (unit, *object, indent, out.clone(), nested + 1, false),
                            // );
                            // write!(out.borrow_mut(), "::").unwrap();
                            // self.inspect(
                            //     scope,
                            //     (unit, *member, indent, out.clone(), nested + 1, false),
                            // );
                            write!(
                                out.borrow_mut(),
                                "{indent}{}",
                                path,
                                indent = "  ".repeat(indent - 1)
                            )
                            .unwrap();
                        }
                        Expr::Let { name, ty, value } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}let {name}: {ty}",
                                indent_str = if newline {
                                    "  ".repeat(indent - 1)
                                } else {
                                    "".to_owned()
                                },
                                name = name,
                                ty = ty,
                            )
                            .unwrap();
                            if let Some(value) = value {
                                write!(out.borrow_mut(), " = ").unwrap();
                                self.inspect(
                                    scope,
                                    (unit, *value, indent + 1, out.clone(), nested + 1, false),
                                );
                            }
                        }
                        Expr::Break { value } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}break",
                                indent_str = "  ".repeat(indent - 1)
                            )
                            .unwrap();
                            if let Some(value) = value {
                                write!(out.borrow_mut(), " ").unwrap();
                                self.inspect(
                                    scope,
                                    (unit, *value, indent + 1, out.clone(), nested + 1, false),
                                );
                            }
                        }
                        Expr::Return { value } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}return",
                                indent_str = "  ".repeat(indent - 1)
                            )
                            .unwrap();
                            if let Some(value) = value {
                                write!(out.borrow_mut(), " ").unwrap();
                                self.inspect(
                                    scope,
                                    (unit, *value, indent + 1, out.clone(), nested + 1, false),
                                );
                            }
                        }
                        Expr::Continue => writeln!(
                            out.borrow_mut(),
                            "{indent_str}continue",
                            indent_str = "  ".repeat(indent - 1)
                        )
                        .unwrap(),
                        Expr::List { .. } => todo!(),
                    };
                    if newline {
                        writeln!(out.borrow_mut()).unwrap();
                    }
                }
                ASTNode::Item(item) => {
                    match item {
                        Item::Submodule { vis, name, id } => {
                            writeln!(
                                out.borrow_mut(),
                                "{indent_str}{vis}mod {name} (Unit {path}) {{",
                                vis = vis,
                                name = name,
                                path = name //scope.canonicalize_path(*id, name.clone()),
                            )
                            .unwrap();
                            for member in scope.units.get(*id).unwrap().members().values() {
                                self.inspect(
                                    scope,
                                    (*id, *member, indent + 1, out.clone(), nested + 1, true),
                                );
                            }
                            writeln!(out.borrow_mut(), "  }}").unwrap();
                        }
                        Item::FunctionDef {
                            vis,
                            name,
                            params,
                            ret_ty,
                            body,
                        } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}{vis}fn {name}({params}) {ret}",
                                vis = vis,
                                name = name,
                                params = params
                                    .iter()
                                    .map(|(name, ty)| format!("{}: {}", name, ty))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                ret = ret_ty
                                    .clone()
                                    .map(|ty| format!("-> {} ", ty))
                                    .unwrap_or(" ".to_owned()),
                            )
                            .unwrap();
                            self.inspect(
                                scope,
                                (unit, *body, indent + 1, out.clone(), nested + 1, false),
                            );
                        }
                        Item::FunctionDecl {
                            vis,
                            name,
                            args,
                            ret_ty,
                        } => writeln!(
                            out.borrow_mut(),
                            "{indent_str}{}extern fn {}({}) -> {}",
                            vis,
                            name,
                            args.iter()
                                .map(|(name, ty)| format!("  {}: {}", name, ty))
                                .collect::<Vec<_>>()
                                .join(", "),
                            ret_ty
                                .clone()
                                .unwrap_or(ItemPath::from(vec![PathPart::Named(
                                    "void".to_owned()
                                )]))
                        )
                        .unwrap(),
                        Item::StructDef { vis, name, fields } => writeln!(
                            out.borrow_mut(),
                            "{indent_str}{}struct {} {{\n{}\n{indent_str}}}",
                            vis,
                            name,
                            fields
                                .iter()
                                .map(|v| format!("{}{}: {}", "  ".repeat(indent + 1), v.0, v.1))
                                .collect::<Vec<_>>()
                                .join(",\n"),
                        )
                        .unwrap(),
                        Item::TypeDef { vis, name, ty } => writeln!(
                            out.borrow_mut(),
                            "{indent_str}{}type {} = {}",
                            vis,
                            name,
                            ty
                        )
                        .unwrap(),
                        v @ Item::ConstDef {
                            vis,
                            name,
                            ty,
                            value,
                        }
                        | v @ Item::StaticDef {
                            vis,
                            ty,
                            name,
                            value,
                        } => {
                            write!(
                                out.borrow_mut(),
                                "{indent_str}{}{kind} {}: {} = ",
                                vis,
                                name,
                                ty,
                                kind = if matches!(v, Item::ConstDef { .. }) {
                                    "const"
                                } else {
                                    "static"
                                }
                            )
                            .unwrap();
                            self.inspect(
                                scope,
                                (unit, *value, indent + 1, out.clone(), nested + 1, false),
                            );
                        }
                    };
                    writeln!(out.borrow_mut()).unwrap();
                }
                #[allow(unreachable_patterns)]
                _ => writeln!(out.borrow_mut(), "{}???", "  ".repeat(indent - 1)).unwrap(),
            };
        }
    }

    impl Inspect for PrintUnit {
        type Scope = Package;
        type Input = UnitId;

        fn inspect(&mut self, package: &Package, id: UnitId) {
            let unit = package.units.get(id).unwrap();
            let buf = Rc::new(RefCell::new(String::new()));
            for member in unit.members().values().copied() {
                package.inspect(
                    &mut PrintNode::new(),
                    (id, member, 0, buf.clone(), 0, false),
                );
                print!("{}", buf.borrow_mut());
            }
            println!("");
        }
    }

    impl Inspect for PrintPackage {
        type Scope = Package;
        type Input = ();

        fn inspect(&mut self, package: &Package, _: ()) {
            let root = package.root;
            let unit = package.units.get(root).unwrap();
            let buf = Rc::new(RefCell::new(String::new()));
            for member in unit.members().values().copied() {
                package.inspect(
                    &mut PrintNode::new(),
                    (root, member, 0, buf.clone(), 0, false),
                );
            }
            println!("{}", buf.borrow_mut());
        }
    }
}

impl Package {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn inspect<P: Inspect<Scope = Package>>(&self, pass: &mut P, input: P::Input) {
        pass.inspect(self, input);
    }

    pub fn transform<P: Transform<Scope = Package>>(&mut self, pass: &mut P, input: P::Input) {
        pass.transform(self, input);
    }

    pub fn name(&self) -> &str {
        self.units.get(self.root).unwrap().name()
    }

    pub fn unit(&self, id: UnitId) -> Option<&Unit> {
        self.units.get(id)
    }

    pub fn unit_mut(&mut self, id: UnitId) -> Option<&mut Unit> {
        self.units.get_mut(id)
    }

    pub fn add_unit(&mut self, unit: Unit) -> UnitId {
        self.units.insert(unit)
    }

    pub fn get_root(&self) -> UnitId {
        self.root
    }

    pub fn set_root(&mut self, root: UnitId) {
        self.root = root;
    }

    pub fn get_parent_name(&self, unit_id: UnitId) -> Option<(UnitId, &str)> {
        let unit = self.units.get(unit_id).unwrap();
        if let Some(parent) = unit.parent() {
            return self.units.get(parent).map(|u| (parent, u.name()));
        }
        None
    }

    // pub fn canonicalize_path(&self, unit_id: UnitId, path: ItemPath) -> ItemPath {
    //     match path {
    //         ItemPath::Name(name) => {
    //             let mut new_path = vec![name];
    //             let mut id = unit_id;
    //             while let Some((new_id, parent)) = self.get_parent_name(id) {
    //                 id = new_id;
    //                 new_path.push(parent.to_owned());
    //             }
    //             new_path.reverse();
    //             ItemPath::Absolute(new_path)
    //         }
    //         path => path,
    //     }
    // }
}
