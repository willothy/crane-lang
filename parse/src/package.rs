use slotmap::SlotMap;

use crate::{
    unit::{ASTUnit, NodeId, Unit, UnitId},
    ASTNode,
};

use self::pass::{Inspect, Transform};

#[derive(Debug)]
pub struct Package<K, U, NK, N, MK>
where
    K: slotmap::Key,
    NK: slotmap::Key,
    MK: slotmap::Key,
{
    units: SlotMap<K, U>,
    root: K,
    _marker: std::marker::PhantomData<(NK, N, MK)>,
}

impl<K, U, NK, N, MK> Default for Package<K, U, NK, N, MK>
where
    K: slotmap::Key,
    NK: slotmap::Key,
    MK: slotmap::Key,
{
    fn default() -> Self {
        Self {
            units: SlotMap::with_key(),
            root: K::default(),
            _marker: std::marker::PhantomData,
        }
    }
}

pub type ASTPackage = Package<UnitId, ASTUnit, NodeId, ASTNode, NodeId>;

impl<K, U, NK, MK, N> Package<K, U, NK, N, MK>
where
    K: slotmap::Key,
    NK: slotmap::Key,
    MK: slotmap::Key,
    U: Unit<K, NK, MK, N>,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn inspect<P: Inspect<Scope = Self>>(&self, pass: &mut P, input: P::Input) {
        pass.inspect(self, input);
    }

    pub fn transform<P: Transform<Scope = Self>>(&mut self, pass: &mut P, input: P::Input) {
        pass.transform(self, input);
    }

    pub fn name(&self) -> &str {
        self.units.get(self.root).unwrap().name()
    }

    pub fn unit(&self, id: K) -> Option<&U> {
        self.units.get(id)
    }

    pub fn unit_mut(&mut self, id: K) -> Option<&mut U> {
        self.units.get_mut(id)
    }

    pub fn units(&self) -> impl Iterator<Item = &U> {
        self.units.values()
    }

    pub fn add_unit(&mut self, unit: U) -> K {
        self.units.insert(unit)
    }

    pub fn get_root(&self) -> K {
        self.root
    }

    pub fn set_root(&mut self, root: K) {
        self.root = root;
    }

    pub fn get_parent_name(&self, unit_id: K) -> Option<(K, &str)> {
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

pub mod pass {
    use std::{cell::RefCell, fmt::Write, rc::Rc};

    use crane_lex::Literal;

    use crate::{
        expr::Expr,
        item::Item,
        unit::{NodeId, Unit, UnitId},
        ASTNode,
    };

    use super::ASTPackage;

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

    #[derive(Clone)]
    pub struct PrintNodeCtx {
        unit: UnitId,
        node: NodeId,
        indent: usize,
        out: Rc<RefCell<dyn Write>>,
        nested: usize,
        newline: bool,
        stmt: bool,
        result: bool,
    }

    impl PrintNodeCtx {
        pub fn new(unit: UnitId, node: NodeId, out: Rc<RefCell<dyn Write>>) -> Self {
            Self {
                unit,
                node,
                out,
                indent: 0,
                nested: 0,
                newline: false,
                stmt: false,
                result: false,
            }
        }

        fn node(mut self, node: NodeId) -> Self {
            self.node = node;
            self
        }

        fn newline(mut self) -> Self {
            self.newline = true;
            self
        }

        fn nested(mut self) -> Self {
            self.nested += 1;
            self
        }

        fn indent(mut self) -> Self {
            self.indent += 1;
            self
        }

        fn result(mut self) -> Self {
            self.result = true;
            self
        }

        fn stmt(mut self) -> Self {
            self.stmt = true;
            self
        }

        fn with(&self) -> Self {
            let mut new = self.clone();
            new.newline = false;
            new.stmt = false;
            new.result = false;
            new
        }
    }

    impl PrintNode<'_> {
        pub fn new() -> Self {
            Self {
                _marker: std::marker::PhantomData,
            }
        }
    }

    impl<'a> Inspect for PrintNode<'a> {
        type Scope = ASTPackage;
        type Input = PrintNodeCtx;

        fn inspect(&mut self, scope: &Self::Scope, input: Self::Input) {
            let node = scope
                .units
                .get(input.unit)
                .unwrap()
                .node(input.node)
                .unwrap_or(&ASTNode::Error);
            let indent_str = "  ".repeat(input.indent.checked_sub(1).unwrap_or(0));

            match node {
                ASTNode::Expr(e) => {
                    match e {
                        Expr::Result { value } => {
                            // write!(input.out.borrow_mut(), "{indent_str}").unwrap();
                            self.inspect(scope, input.with().node(*value).nested().result());
                            // writeln!(out.borrow_mut()).unwrap();
                        }
                        Expr::Literal(l) => {
                            let indent = if input.newline || input.result {
                                "  ".repeat(input.indent - 1)
                            } else {
                                "".to_owned()
                            };
                            match l {
                                Literal::Int(i) => {
                                    write!(input.out.borrow_mut(), "{indent}{i}").unwrap()
                                }
                                Literal::Float(f) => {
                                    write!(input.out.borrow_mut(), "{indent}{f}").unwrap()
                                }
                                Literal::String(s) => {
                                    write!(input.out.borrow_mut(), "\"{indent}{s}\"").unwrap()
                                }
                                Literal::Char(c) => {
                                    write!(input.out.borrow_mut(), "'{indent}{c}'").unwrap()
                                }
                                Literal::Bool(b) => {
                                    write!(input.out.borrow_mut(), "{indent}{b}").unwrap()
                                }
                            }
                        }
                        Expr::Ident(n) => write!(
                            input.out.borrow_mut(),
                            "{indent}{n}",
                            indent = if input.newline || input.result {
                                "  ".repeat(input.indent - 1)
                            } else {
                                "".to_owned()
                            }
                        )
                        .unwrap(),
                        Expr::StructInit { ty, fields } => {
                            write!(input.out.borrow_mut(), "{ty} {{\n", ty = ty).unwrap();
                            for (i, (name, expr)) in fields.iter().enumerate() {
                                if i > 0 {
                                    write!(input.out.borrow_mut(), ",\n").unwrap();
                                }
                                write!(
                                    input.out.borrow_mut(),
                                    "{indent_str}{name}: ",
                                    name = name,
                                    indent_str = "  ".repeat(input.indent)
                                )
                                .unwrap();
                                self.inspect(scope, input.with().node(*expr).nested());
                            }
                            write!(input.out.borrow_mut(), "\n{indent_str}}}").unwrap();
                        }
                        Expr::Call { callee, args } => {
                            if input.newline || input.result {
                                write!(input.out.borrow_mut(), "{indent}", indent = indent_str)
                                    .unwrap();
                            }
                            self.inspect(scope, input.with().node(*callee));
                            write!(input.out.borrow_mut(), "(").unwrap();
                            for (i, arg) in args.iter().enumerate() {
                                if i > 0 {
                                    write!(input.out.borrow_mut(), ", ").unwrap();
                                }
                                self.inspect(scope, input.with().node(*arg));
                            }
                            write!(input.out.borrow_mut(), ")").unwrap();
                        }
                        Expr::MemberAccess {
                            object,
                            member,
                            computed,
                        } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent}",
                                indent = if input.newline || input.result {
                                    indent_str
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                            self.inspect(scope, input.with().node(*object).nested());
                            if *computed {
                                write!(input.out.borrow_mut(), "[").unwrap();
                                self.inspect(scope, input.with().node(*member).nested());
                                write!(input.out.borrow_mut(), "]").unwrap();
                            } else {
                                write!(input.out.borrow_mut(), ".").unwrap();
                                self.inspect(scope, input.with().node(*member).nested());
                            }
                        }
                        Expr::UnaryOp { op, operand } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent}{op}",
                                op = op,
                                indent = if input.newline || input.result {
                                    "  ".repeat(input.indent - 1)
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                            self.inspect(scope, input.with().node(*operand).nested());
                        }
                        Expr::BinaryOp { op, lhs, rhs } => {
                            if input.newline || input.result {
                                write!(input.out.borrow_mut(), "{}", "  ".repeat(input.indent - 1))
                                    .unwrap();
                            }
                            self.inspect(scope, input.with().node(*lhs).nested());
                            write!(input.out.borrow_mut(), " {op} ", op = op).unwrap();
                            self.inspect(scope, input.with().node(*rhs).nested());
                        }
                        Expr::Assignment { lhs, op, rhs } => {
                            if input.nested <= 3 {
                                write!(input.out.borrow_mut(), "{}", "  ".repeat(input.indent - 1))
                                    .unwrap();
                            }
                            self.inspect(scope, input.with().node(*lhs).nested());
                            write!(input.out.borrow_mut(), " {op} ", op = op).unwrap();
                            self.inspect(scope, input.with().node(*rhs).nested());
                        }
                        Expr::Cast { ty, expr } => {
                            self.inspect(scope, input.with().node(*expr).nested());
                            write!(input.out.borrow_mut(), " as {ty}", ty = ty).unwrap();
                        }
                        Expr::Block { exprs: stmts } => {
                            writeln!(input.out.borrow_mut(), "{{").unwrap();
                            for stmt in stmts {
                                self.inspect(
                                    scope,
                                    input.with().node(*stmt).indent().newline().stmt(),
                                );
                            }
                            write!(
                                input.out.borrow_mut(),
                                "{indent}}}",
                                indent = "  ".repeat(input.indent - 1)
                            )
                            .unwrap();
                        }
                        Expr::If { cond, then, r#else } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent_str}if ",
                                indent_str = if input.newline || input.result {
                                    indent_str
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                            self.inspect(scope, input.with().node(*cond).nested());
                            write!(input.out.borrow_mut(), " ").unwrap();
                            self.inspect(
                                scope,
                                input.with().node(*then).nested(),
                                //     indent + (1 - if indent == 0 { 1 } else { indent % 2 }),
                            );
                            if let Some(r#else) = r#else {
                                write!(
                                    input.out.borrow_mut(),
                                    " else ",
                                    // indent = if newline { indent_str } else { "".to_owned() }
                                )
                                .unwrap();
                                self.inspect(scope, input.with().node(*r#else).nested());
                            }
                        }
                        Expr::While { cond, body } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent_str}while ",
                                indent_str = indent_str
                            )
                            .unwrap();
                            self.inspect(scope, input.with().node(*cond).nested());
                            write!(input.out.borrow_mut(), " ").unwrap();
                            self.inspect(scope, input.with().node(*body).nested());
                        }
                        Expr::Loop { body } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent_str}loop ",
                                indent_str = indent_str
                            )
                            .unwrap();
                            self.inspect(scope, input.with().node(*body));
                        }
                        Expr::ScopeResolution { path } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent}{}",
                                path,
                                indent = if input.newline || input.result {
                                    indent_str
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                        }
                        Expr::Let { lhs, ty, value } => {
                            write!(input.out.borrow_mut(), "{indent_str}let ").unwrap();
                            self.inspect(scope, input.with().node(*lhs));
                            if let Some(ty) = ty {
                                write!(input.out.borrow_mut(), ": {ty}", ty = ty).unwrap();
                            }
                            if let Some(value) = value {
                                write!(input.out.borrow_mut(), " = ").unwrap();
                                self.inspect(scope, input.with().node(*value).nested());
                            }
                        }
                        Expr::Break { value } => {
                            write!(input.out.borrow_mut(), "{indent_str}break",).unwrap();
                            if let Some(value) = value {
                                write!(input.out.borrow_mut(), " ").unwrap();
                                self.inspect(scope, input.with().node(*value).nested().indent());
                            }
                        }
                        Expr::Return { value } => {
                            write!(input.out.borrow_mut(), "{indent_str}return",).unwrap();
                            if let Some(value) = value {
                                write!(input.out.borrow_mut(), " ").unwrap();
                                self.inspect(scope, input.with().node(*value).nested().indent());
                            }
                        }
                        Expr::Continue => {
                            write!(input.out.borrow_mut(), "{indent_str}continue",).unwrap()
                        }
                        Expr::List { exprs } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent_str}[",
                                indent_str = if input.newline || input.result {
                                    indent_str
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                            for (i, item) in exprs.iter().enumerate() {
                                if i != 0 {
                                    write!(input.out.borrow_mut(), ", ").unwrap();
                                }
                                self.inspect(scope, input.with().node(*item).nested());
                            }
                            write!(input.out.borrow_mut(), "]",).unwrap();
                        }
                        Expr::Tuple { exprs } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent_str}(",
                                indent_str = if input.newline || input.result {
                                    indent_str
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                            for (i, item) in exprs.iter().enumerate() {
                                if i != 0 {
                                    write!(input.out.borrow_mut(), ", ").unwrap();
                                }
                                self.inspect(scope, input.with().node(*item).nested());
                            }
                            write!(input.out.borrow_mut(), ")",).unwrap();
                        }
                        Expr::Closure {
                            params,
                            ret_ty,
                            body,
                        } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent_str}fn(",
                                indent_str = if input.newline || input.result {
                                    "  ".repeat(input.indent - 1)
                                } else {
                                    "".to_owned()
                                }
                            )
                            .unwrap();
                            for (i, param) in params.iter().enumerate() {
                                if i != 0 {
                                    write!(input.out.borrow_mut(), ", ").unwrap();
                                }
                                write!(input.out.borrow_mut(), "{}: {}", param.0, param.1).unwrap();
                            }
                            write!(input.out.borrow_mut(), ")").unwrap();
                            if let Some(ret_ty) = ret_ty {
                                write!(input.out.borrow_mut(), " -> {}", ret_ty).unwrap();
                            }
                            write!(input.out.borrow_mut(), " ").unwrap();
                            if let ASTNode::Expr(Expr::Block { .. }) =
                                scope.unit(input.unit).unwrap().node(input.node).unwrap()
                            {
                                self.inspect(scope, input.with().node(*body).nested());
                            } else {
                                write!(input.out.borrow_mut(), "=> ").unwrap();
                                self.inspect(scope, input.with().node(*body).nested());
                            }
                        }
                    };
                    if input.result {
                        write!(input.out.borrow_mut(), " # result").unwrap();
                    } else if input.stmt {
                        let unit = scope.unit(input.unit).unwrap();
                        let node = unit.node(input.node).unwrap();
                        match node {
                            ASTNode::Expr(Expr::Result { .. }) => {}
                            _ => write!(input.out.borrow_mut(), ";").unwrap(),
                        }
                    }
                    if input.newline {
                        writeln!(input.out.borrow_mut()).unwrap();
                    }
                }
                ASTNode::Item(item) => {
                    match item {
                        Item::Submodule { vis, name, id } => {
                            writeln!(
                                input.out.borrow_mut(),
                                "{indent_str}{vis}mod {name} (Unit {path}) {{",
                                vis = vis,
                                name = name,
                                path = name //scope.canonicalize_path(*id, name.clone()),
                            )
                            .unwrap();
                            for member in scope.units.get(*id).unwrap().members().values() {
                                self.inspect(scope, input.with().node(*member).indent().nested());
                            }
                            writeln!(input.out.borrow_mut(), "  }}").unwrap();
                        }
                        Item::FunctionDef {
                            vis,
                            name,
                            params,
                            ret_ty,
                            body,
                        } => {
                            write!(
                                input.out.borrow_mut(),
                                "{indent_str}{vis}fn {name}({params}) {ret}",
                                vis = vis,
                                name = name,
                                params = params
                                    .iter()
                                    .map(|(name, ty)| format!("{}: {}", name, ty))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                ret = ret_ty
                                    .as_ref()
                                    .clone()
                                    .map(|ty| format!("-> {} ", ty))
                                    .unwrap_or(" ".to_owned()),
                            )
                            .unwrap();
                            self.inspect(scope, input.with().node(*body).indent().nested());
                        }
                        Item::FunctionDecl {
                            vis,
                            name,
                            args,
                            ret_ty,
                        } => writeln!(
                            input.out.borrow_mut(),
                            "{indent_str}{}extern fn {}({}) -> {}",
                            vis,
                            name,
                            args.iter()
                                .map(|(name, ty)| format!("  {}: {}", name, ty))
                                .collect::<Vec<_>>()
                                .join(", "),
                            ret_ty
                                .as_ref()
                                .map(|v| format!("-> {}", v))
                                .unwrap_or("".to_string())
                        )
                        .unwrap(),
                        Item::StructDef { vis, name, fields } => writeln!(
                            input.out.borrow_mut(),
                            "{indent_str}{}struct {} {{\n{}\n{indent_str}}}",
                            vis,
                            name,
                            fields
                                .iter()
                                .map(|v| format!(
                                    "{}{}: {}",
                                    "  ".repeat(input.indent + 1),
                                    v.0,
                                    v.1
                                ))
                                .collect::<Vec<_>>()
                                .join(",\n"),
                        )
                        .unwrap(),
                        Item::TypeDef { vis, name, ty } => writeln!(
                            input.out.borrow_mut(),
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
                                input.out.borrow_mut(),
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
                            self.inspect(scope, input.with().node(*value).indent().nested());
                        }
                        Item::Import { vis, path } => {
                            writeln!(
                                input.out.borrow_mut(),
                                "{indent_str}{}import {}",
                                match vis {
                                    crane_lex::Visibility::Public => "pub ",
                                    crane_lex::Visibility::Private => "",
                                },
                                path
                            )
                            .unwrap();
                        }
                    };
                    writeln!(input.out.borrow_mut(), "\n").unwrap();
                }
                ASTNode::Error => {
                    let indent = if input.newline || input.result {
                        "  ".repeat(input.indent)
                    } else {
                        "".to_string()
                    };
                    const RED: &str = "\x1b[0;31m";
                    const RESET: &str = "\x1b[0m";
                    write!(input.out.borrow_mut(), "{}{RED}Error{RESET}", indent).unwrap();
                }
                #[allow(unreachable_patterns)]
                _ => writeln!(
                    input.out.borrow_mut(),
                    "{}???",
                    "  ".repeat(input.indent - 1)
                )
                .unwrap(),
            };
        }
    }

    impl Inspect for PrintUnit {
        type Scope = ASTPackage;
        type Input = UnitId;

        fn inspect(&mut self, package: &Self::Scope, id: UnitId) {
            let unit = package.units.get(id).unwrap();
            let buf = Rc::new(RefCell::new(String::new()));
            for member in unit.members().values().copied() {
                package.inspect(
                    &mut PrintNode::new(),
                    PrintNodeCtx::new(id, member, buf.clone()),
                );
                print!("{}", buf.borrow_mut());
            }
            println!("");
        }
    }

    impl Inspect for PrintPackage {
        type Scope = ASTPackage;
        type Input = ();

        fn inspect(&mut self, package: &Self::Scope, _: ()) {
            let root = package.root;
            let unit = package.units.get(root).unwrap();
            let buf = Rc::new(RefCell::new(String::new()));
            for member in unit.members().values().copied() {
                package.inspect(
                    &mut PrintNode::new(),
                    PrintNodeCtx::new(root, member, buf.clone()),
                );
            }
            println!("{}", buf.borrow_mut());
        }
    }
}
