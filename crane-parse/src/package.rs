use std::fmt::Write;

use crane_lex::Literal;
use slotmap::SlotMap;

use crate::{
    path::{ItemPath, PathPart},
    unit::{Unit, UnitId},
    ASTNode, Expr, Item, NodeId,
};

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
    use crate::unit::UnitId;

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

    impl Inspect for PrintUnit {
        type Scope = Package;
        type Input = UnitId;

        fn inspect(&mut self, package: &Package, id: UnitId) {
            let unit = package.units.get(id).unwrap();
            let mut buf = String::new();
            for member in unit.members().values().copied() {
                package.print_node(id, member, 0, &mut buf, 0, false);
            }
            println!("{}", buf);
        }
    }

    impl Inspect for PrintPackage {
        type Scope = Package;
        type Input = ();

        fn inspect(&mut self, package: &Package, _: ()) {
            let root = package.root;
            let unit = package.units.get(root).unwrap();
            let mut buf = String::new();
            for member in unit.members().values().copied() {
                package.print_node(root, member, 0, &mut buf, 0, false);
            }
            println!("{}", buf);
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

    pub fn print_node(
        &self,
        unit: UnitId,
        node: NodeId,
        indent: usize,
        out: &mut dyn Write,
        nested: usize,
        newline: bool,
    ) {
        let node = self.units.get(unit).unwrap().get_node(node).unwrap();
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
                            Literal::Int(i) => write!(out, "{indent}{i}").unwrap(),
                            Literal::Float(f) => write!(out, "{indent}{f}").unwrap(),
                            Literal::String(s) => write!(out, "\"{indent}{s}\"").unwrap(),
                            Literal::Char(c) => write!(out, "'{indent}{c}'").unwrap(),
                            Literal::Bool(b) => write!(out, "{indent}{b}").unwrap(),
                        }
                    }
                    Expr::Ident(n) => write!(
                        out,
                        "{indent}{n}",
                        indent = if newline {
                            "  ".repeat(indent - 1)
                        } else {
                            "".to_owned()
                        }
                    )
                    .unwrap(),
                    Expr::StructInit { ty, fields } => {
                        write!(out, "{ty} {{", ty = ty).unwrap();
                        for (name, expr) in fields {
                            write!(out, "{name}: ", name = name).unwrap();
                            self.print_node(unit, *expr, indent, out, nested + 1, false);
                            write!(out, ", ").unwrap();
                        }
                        write!(out, "}}").unwrap();
                    }
                    Expr::Call { callee, args } => {
                        self.print_node(unit, *callee, indent, out, nested, false);
                        write!(out, "(").unwrap();
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 {
                                write!(out, ", ").unwrap();
                            }
                            self.print_node(unit, *arg, indent, out, nested, false);
                        }
                        write!(out, ")").unwrap();
                    }
                    Expr::MemberAccess {
                        object,
                        member,
                        computed,
                    } => {
                        self.print_node(unit, *object, indent, out, nested + 1, false);
                        if *computed {
                            write!(out, "[").unwrap();
                            self.print_node(unit, *member, indent, out, nested + 1, false);
                            write!(out, "]").unwrap();
                        } else {
                            write!(out, ".").unwrap();
                            self.print_node(unit, *member, indent, out, nested + 1, false);
                        }
                    }
                    Expr::UnaryOp { op, operand } => {
                        write!(
                            out,
                            "{indent}{op}",
                            op = op,
                            indent = if newline {
                                "  ".repeat(indent - 1)
                            } else {
                                "".to_owned()
                            }
                        )
                        .unwrap();
                        self.print_node(unit, *operand, indent, out, nested + 1, false);
                    }
                    Expr::BinaryOp { op, lhs, rhs } => {
                        if newline {
                            write!(out, "{}", "  ".repeat(indent - 1)).unwrap();
                        }
                        self.print_node(unit, *lhs, indent, out, nested + 1, false);
                        write!(out, " {op} ", op = op).unwrap();
                        self.print_node(unit, *rhs, indent, out, nested + 1, false);
                    }
                    Expr::Assignment { lhs, op, rhs } => {
                        if nested <= 3 {
                            write!(out, "{}", "  ".repeat(indent - 1)).unwrap();
                        }
                        self.print_node(unit, *lhs, indent, out, nested + 1, false);
                        write!(out, " {op} ", op = op).unwrap();
                        self.print_node(unit, *rhs, indent, out, nested + 1, false);
                    }
                    Expr::Cast { ty, expr } => {
                        write!(out, "({ty})", ty = ty).unwrap();
                        self.print_node(unit, *expr, indent, out, nested + 1, false);
                    }
                    Expr::Block { exprs: stmts } => {
                        writeln!(out, "{{").unwrap();
                        for stmt in stmts {
                            self.print_node(unit, *stmt, indent + 1, out, nested + 1, true);
                        }
                        write!(out, "{indent}}}", indent = " ".repeat(indent - 1)).unwrap();
                    }
                    Expr::If { cond, then, r#else } => {
                        write!(
                            out,
                            "{indent_str}if ",
                            indent_str = if newline { indent_str } else { "".to_owned() }
                        )
                        .unwrap();
                        self.print_node(unit, *cond, indent + 1, out, nested + 1, false);
                        write!(out, " ").unwrap();
                        self.print_node(
                            unit,
                            *then,
                            indent + (1 - if indent == 0 { 1 } else { indent % 2 }),
                            out,
                            nested + 1,
                            false,
                        );
                        if let Some(r#else) = r#else {
                            write!(
                                out,
                                " else ",
                                // indent = if newline { indent_str } else { "".to_owned() }
                            )
                            .unwrap();
                            self.print_node(unit, *r#else, indent, out, nested + 1, false);
                        }
                    }
                    Expr::While { cond, body } => {
                        write!(out, "{indent_str}while ", indent_str = indent_str).unwrap();
                        self.print_node(unit, *cond, indent + 1, out, nested + 1, false);
                        write!(out, " ").unwrap();
                        self.print_node(unit, *body, indent + 1, out, nested + 1, false);
                    }
                    Expr::Loop { body } => {
                        write!(out, "{indent_str}loop ", indent_str = indent_str).unwrap();
                        self.print_node(unit, *body, indent + 1, out, nested + 1, false);
                    }
                    Expr::ScopeResolution { object, member } => {
                        self.print_node(unit, *object, indent, out, nested + 1, false);
                        write!(out, "::").unwrap();
                        self.print_node(unit, *member, indent, out, nested + 1, false);
                    }
                    Expr::Let { name, ty, value } => {
                        write!(
                            out,
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
                            write!(out, " = ").unwrap();
                            self.print_node(unit, *value, indent + 1, out, nested + 1, false);
                        }
                    }
                    Expr::Break { value } => {
                        write!(
                            out,
                            "{indent_str}break",
                            indent_str = "  ".repeat(indent - 1)
                        )
                        .unwrap();
                        if let Some(value) = value {
                            write!(out, " ").unwrap();
                            self.print_node(unit, *value, indent + 1, out, nested + 1, false);
                        }
                    }
                    Expr::Return { value } => {
                        write!(
                            out,
                            "{indent_str}return",
                            indent_str = "  ".repeat(indent - 1)
                        )
                        .unwrap();
                        if let Some(value) = value {
                            write!(out, " ").unwrap();
                            self.print_node(unit, *value, indent + 1, out, nested + 1, false);
                        }
                    }
                    Expr::Continue => writeln!(
                        out,
                        "{indent_str}continue",
                        indent_str = "  ".repeat(indent - 1)
                    )
                    .unwrap(),
                    Expr::List { .. } => todo!(),
                    Expr::Error => todo!(),
                };
                if newline {
                    writeln!(out).unwrap();
                }
            }
            ASTNode::Item(item) => {
                match item {
                    Item::Submodule { vis, name, id } => {
                        writeln!(
                            out,
                            "{indent_str}{vis}mod {name} (Unit {path}) {{",
                            vis = vis,
                            name = name,
                            path = name //self.canonicalize_path(*id, name.clone()),
                        )
                        .unwrap();
                        for member in self.units.get(*id).unwrap().members().values() {
                            self.print_node(*id, *member, indent + 1, out, nested + 1, true);
                        }
                        writeln!(out, "  }}").unwrap();
                    }
                    Item::FunctionDef {
                        vis,
                        name,
                        params,
                        ret_ty,
                        body,
                    } => {
                        write!(
                            out,
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
                        self.print_node(unit, *body, indent + 1, out, nested + 1, false);
                    }
                    Item::FunctionDecl {
                        vis,
                        name,
                        args,
                        ret_ty,
                    } => writeln!(
                        out,
                        "{indent_str}{}extern fn {}({}) -> {}",
                        vis,
                        name,
                        args.iter()
                            .map(|(name, ty)| format!("  {}: {}", name, ty))
                            .collect::<Vec<_>>()
                            .join(", "),
                        ret_ty
                            .clone()
                            .unwrap_or(ItemPath::from(vec![PathPart::Named("void".to_owned())]))
                    )
                    .unwrap(),
                    Item::StructDef { vis, name, fields } => writeln!(
                        out,
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
                    Item::TypeDef { vis, name, ty } => {
                        writeln!(out, "{indent_str}{}type {} = {}", vis, name, ty).unwrap()
                    }
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
                            out,
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
                        self.print_node(unit, *value, indent + 1, out, nested + 1, false);
                    }
                };
                writeln!(out).unwrap();
            }
            #[allow(unreachable_patterns)]
            _ => writeln!(out, "{}???", "  ".repeat(indent - 1)).unwrap(),
        };
    }
}
