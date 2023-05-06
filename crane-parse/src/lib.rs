use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use anyhow::Result;
use log::debug;

use crane_lex as lex;
use lex::{
    Arithmetic, Assignment, Bitwise, Comparison, Keyword, Literal, Logical, OperatorType,
    Punctuation, Spanned, Symbol, Token, Visibility,
};
use slotmap::{new_key_type, SlotMap};

new_key_type! {
    pub struct NodeId;
    pub struct TypeId;
    pub struct UnitId;
}

#[derive(Debug)]
pub struct Unit {
    parent: Option<UnitId>,
    name: String,
    ast_nodes: SlotMap<NodeId, ASTNode>,
    members: HashMap<String, NodeId>,
}

#[derive(Debug)]
pub struct Package {
    units: SlotMap<UnitId, Unit>,
    root: UnitId,
}

impl Package {
    pub fn new() -> Self {
        Self {
            units: SlotMap::with_key(),
            root: UnitId::default(),
        }
    }

    pub fn name(&self) -> &str {
        &self.units.get(self.root).unwrap().name
    }

    pub fn get_unit(&self, id: UnitId) -> Option<&Unit> {
        self.units.get(id)
    }

    fn get_parent_name(&self, unit_id: UnitId) -> Option<(UnitId, String)> {
        let unit = self.units.get(unit_id).unwrap();
        if let Some(parent) = unit.parent {
            return self.units.get(parent).map(|u| (parent, u.name.clone()));
        }
        None
    }

    fn canonicalize_path(&self, unit_id: UnitId, path: ItemPath) -> ItemPath {
        match path {
            ItemPath::Name(name) => {
                let mut new_path = vec![name];
                let mut id = unit_id;
                while let Some((new_id, parent)) = self.get_parent_name(id) {
                    id = new_id;
                    new_path.push(parent);
                }
                new_path.reverse();
                ItemPath::Absolute(new_path)
            }
            path => path,
        }
    }

    pub fn print_node(
        &self,
        unit: UnitId,
        node: NodeId,
        indent: usize,
        out: &mut dyn Write,
        nested: usize,
    ) {
        let node = self.units.get(unit).unwrap().ast_nodes.get(node).unwrap();
        let indent_str = "  ".repeat(indent);
        match node {
            ASTNode::Expr(e) => match e {
                Expr::Literal(l) => match l {
                    Literal::Int(i) => write!(out, "{i}").unwrap(),
                    Literal::Float(f) => write!(out, "{f}").unwrap(),
                    Literal::String(s) => write!(out, "\"{s}\"").unwrap(),
                    Literal::Char(c) => write!(out, "'{c}'").unwrap(),
                    Literal::Bool(b) => write!(out, "{b}").unwrap(),
                },
                Expr::Ident(n) => write!(out, "{n}").unwrap(),
                Expr::StructInit { ty, fields } => {
                    write!(out, "{ty} {{", ty = ty).unwrap();
                    for (name, expr) in fields {
                        write!(out, "{name}: ", name = name).unwrap();
                        self.print_node(unit, *expr, indent, out, nested + 1);
                        write!(out, ", ").unwrap();
                    }
                    write!(out, "}}").unwrap();
                }
                Expr::Call { callee, args } => {
                    self.print_node(unit, *callee, indent, out, nested + 1);
                    write!(out, "(").unwrap();
                    for arg in args {
                        self.print_node(unit, *arg, indent, out, nested + 1);
                        write!(out, ", ").unwrap();
                    }
                    write!(out, ")").unwrap();
                }
                Expr::MemberAccess {
                    object,
                    member,
                    computed,
                } => {
                    self.print_node(unit, *object, indent, out, nested + 1);
                    if *computed {
                        write!(out, "[").unwrap();
                        self.print_node(unit, *member, indent, out, nested + 1);
                        write!(out, "]").unwrap();
                    } else {
                        write!(out, ".").unwrap();
                        self.print_node(unit, *member, indent, out, nested + 1);
                    }
                }
                Expr::UnaryOp { op, operand } => {
                    write!(out, "{op}", op = op).unwrap();
                    self.print_node(unit, *operand, indent, out, nested + 1);
                }
                Expr::BinaryOp { op, lhs, rhs } => {
                    self.print_node(unit, *lhs, indent, out, nested + 1);
                    write!(out, " {op} ", op = op).unwrap();
                    self.print_node(unit, *rhs, indent, out, nested + 1);
                }
                Expr::AssignmentOp { lhs, op, rhs } => {
                    if nested <= 3 {
                        write!(out, "{}", "  ".repeat(indent - 1)).unwrap();
                    }
                    self.print_node(unit, *lhs, indent, out, nested + 1);
                    write!(out, " {op} ", op = op).unwrap();
                    self.print_node(unit, *rhs, indent, out, nested + 1);
                }
                Expr::Cast { ty, expr } => {
                    write!(out, "({ty})", ty = ty).unwrap();
                    self.print_node(unit, *expr, indent, out, nested + 1);
                }
                Expr::Block { stmts } => {
                    for stmt in stmts {
                        self.print_node(unit, *stmt, indent + 1, out, nested + 1);
                    }
                }
                Expr::If { cond, then, r#else } => {
                    writeln!(out, "{indent_str}if (", indent_str = indent_str).unwrap();
                    self.print_node(unit, *cond, indent + 1, out, nested + 1);
                    writeln!(out, ") {{").unwrap();
                    self.print_node(unit, *then, indent + 1, out, nested + 1);
                    writeln!(out, "{indent_str}}}", indent_str = indent_str).unwrap();
                    if let Some(r#else) = r#else {
                        writeln!(out, "{indent_str}else {{", indent_str = indent_str).unwrap();
                        self.print_node(unit, *r#else, indent + 1, out, nested + 1);
                        writeln!(out, "\n{indent_str}}}", indent_str = indent_str).unwrap();
                    }
                }
                Expr::While { cond, body } => {
                    writeln!(out, "{indent_str}while (", indent_str = indent_str).unwrap();
                    self.print_node(unit, *cond, indent + 1, out, nested + 1);
                    writeln!(out, ") {{").unwrap();
                    self.print_node(unit, *body, indent + 1, out, nested + 1);
                    writeln!(out, "\n{indent_str}}}", indent_str = indent_str).unwrap();
                }
                Expr::Loop { body } => {
                    writeln!(out, "{indent_str}loop {{", indent_str = indent_str).unwrap();
                    self.print_node(unit, *body, indent + 1, out, nested + 1);
                    writeln!(out, "\n{indent_str}}}", indent_str = indent_str).unwrap();
                }
                Expr::ScopeResolution { object, member } => {
                    self.print_node(unit, *object, indent, out, nested + 1);
                    write!(out, "::").unwrap();
                    self.print_node(unit, *member, indent, out, nested + 1);
                }
            },
            ASTNode::Stmt(s) => match s {
                Stmt::Expr(e) => self.print_node(unit, *e, indent, out, nested + 1),
                Stmt::Let { name, ty, value } => {
                    write!(
                        out,
                        "{indent_str}let {name}: {ty}",
                        indent_str = "  ".repeat(indent - 1),
                        name = name,
                        ty = ty,
                    )
                    .unwrap();
                    if let Some(value) = value {
                        write!(out, " = ").unwrap();
                        self.print_node(unit, *value, indent + 1, out, nested + 1);
                    }
                    writeln!(out, "").unwrap();
                }
                Stmt::Break { value } => {
                    write!(
                        out,
                        "{indent_str}break",
                        indent_str = "  ".repeat(indent - 1)
                    )
                    .unwrap();
                    if let Some(value) = value {
                        write!(out, " ").unwrap();
                        self.print_node(unit, *value, indent + 1, out, nested + 1);
                    }
                    writeln!(out, "").unwrap();
                }
                Stmt::Return { value } => {
                    write!(
                        out,
                        "{indent_str}return",
                        indent_str = "  ".repeat(indent - 1)
                    )
                    .unwrap();
                    if let Some(value) = value {
                        write!(out, " ").unwrap();
                        self.print_node(unit, *value, indent + 1, out, nested + 1);
                    }
                    writeln!(out, "").unwrap();
                }
                Stmt::Continue => writeln!(
                    out,
                    "{indent_str}continue",
                    indent_str = "  ".repeat(indent - 1)
                )
                .unwrap(),
            },
            ASTNode::Item(item) => {
                match item {
                    Item::Submodule { vis, name, id } => {
                        writeln!(
                            out,
                            "{indent_str}{vis}mod {name} (Unit {path}) {{",
                            vis = vis,
                            name = name,
                            path = self.canonicalize_path(*id, name.clone()),
                        )
                        .unwrap();
                        for member in self.units.get(*id).unwrap().members.values() {
                            self.print_node(*id, *member, indent + 1, out, nested + 1);
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
                        writeln!(
                            out,
                            "{indent_str}{vis}fn {name}({params}) {ret} {{",
                            vis = vis,
                            name = name,
                            params = params
                                .iter()
                                .map(|(name, ty)| format!("{}: {}", name, ty))
                                .collect::<Vec<_>>()
                                .join(", "),
                            ret = ret_ty.clone().unwrap_or(ItemPath::Name("void".to_owned())),
                        )
                        .unwrap();
                        for stmt in body {
                            self.print_node(unit, *stmt, indent + 1, out, nested + 1);
                        }
                        writeln!(out, "{indent_str}}}").unwrap();
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
                        ret_ty.clone().unwrap_or(ItemPath::Name("void".to_owned()))
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
                    Item::ConstDef {
                        vis,
                        name,
                        ty,
                        value: _,
                    } => writeln!(out, "{indent_str}{}const {}: {}", vis, name, ty).unwrap(),
                };
                writeln!(out, "").unwrap();
            }
            #[allow(unreachable_patterns)]
            _ => writeln!(out, "{}???", "  ".repeat(indent - 1)).unwrap(),
        }
    }

    pub fn dbg_print(&self) {
        let root = self.root;
        let unit = self.units.get(root).unwrap();
        let mut writer = String::new();
        for member in unit.members.values().copied() {
            self.print_node(root, member, 0, &mut writer, 0);
        }
        println!("{}", writer);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ItemPath {
    /// Starting with root::
    Absolute(Vec<String>),
    /// Starting with self:: or nothing
    Relative(Vec<String>),
    /// Starting with :: followed by an external package name
    External(Vec<String>),
    /// Single identifier (name expected to be in scope)
    Name(String),
}

impl Display for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemPath::Absolute(path) => {
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", part)?;
                }
            }
            ItemPath::Relative(path) => {
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", part)?;
                }
            }
            ItemPath::External(path) => {
                write!(f, "::")?;
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", part)?;
                }
            }
            ItemPath::Name(name) => write!(f, "{}", name)?,
        }
        Ok(())
    }
}

/// Item path starting with root:: or :: (external)
/// Guaranteed to be absolute
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum AbsoluteItemPath {
    Absolute(Vec<String>),
    External(Vec<String>),
}

impl TryInto<AbsoluteItemPath> for ItemPath {
    type Error = ();

    fn try_into(self) -> Result<AbsoluteItemPath, Self::Error> {
        match self {
            ItemPath::Absolute(path) => Ok(AbsoluteItemPath::Absolute(path)),
            ItemPath::External(path) => Ok(AbsoluteItemPath::External(path)),
            _ => Err(()),
        }
    }
}

impl ItemPath {
    pub fn name(&self) -> Option<&String> {
        match self {
            ItemPath::Absolute(path) => path.last(),
            ItemPath::Relative(path) => path.last(),
            ItemPath::External(path) => path.last(),
            ItemPath::Name(name) => Some(name),
        }
    }

    pub fn into_absolute(self) -> Option<AbsoluteItemPath> {
        match self {
            ItemPath::Absolute(path) => Some(AbsoluteItemPath::Absolute(path)),
            ItemPath::External(path) => Some(AbsoluteItemPath::External(path)),
            _ => None,
        }
    }

    pub fn root(&self) -> Option<&String> {
        match self {
            ItemPath::Absolute(path) => path.first(),
            ItemPath::Relative(path) => path.first(),
            ItemPath::External(path) => path.first(),
            ItemPath::Name(_) => None,
        }
    }

    pub fn is_absolute(&self) -> bool {
        match self {
            ItemPath::Absolute(_) => true,
            _ => false,
        }
    }

    pub fn is_relative(&self) -> bool {
        match self {
            ItemPath::Relative(_) => true,
            _ => false,
        }
    }

    pub fn is_external(&self) -> bool {
        match self {
            ItemPath::External(_) => true,
            _ => false,
        }
    }

    pub fn is_name(&self) -> bool {
        match self {
            ItemPath::Name(_) => true,
            _ => false,
        }
    }
}

/// Module-level items
#[derive(Debug, PartialEq)]
pub enum Item {
    Submodule {
        vis: Visibility,
        name: ItemPath,
        id: UnitId,
    },
    FunctionDef {
        vis: Visibility,
        name: ItemPath,
        params: Vec<(String, ItemPath)>,
        ret_ty: Option<ItemPath>,
        body: Vec<NodeId>,
    },
    FunctionDecl {
        vis: Visibility,
        name: ItemPath,
        args: Vec<(String, ItemPath)>,
        ret_ty: Option<ItemPath>,
    },
    StructDef {
        vis: Visibility,
        name: ItemPath,
        fields: Vec<(String, ItemPath)>,
    },
    TypeDef {
        vis: Visibility,
        name: ItemPath,
        ty: ItemPath,
    },
    ConstDef {
        vis: Visibility,
        name: String,
        ty: ItemPath,
        value: NodeId,
    },
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Deref,
    Ref,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Neg => "-",
                UnaryOp::Not => "!",
                UnaryOp::Deref => "*",
                UnaryOp::Ref => "&",
            }
        )
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Mod => "%",
                BinaryOp::And => "&&",
                BinaryOp::Or => "||",
                BinaryOp::Eq => "==",
                BinaryOp::Neq => "!=",
                BinaryOp::Lt => "<",
                BinaryOp::Gt => ">",
                BinaryOp::Leq => "<=",
                BinaryOp::Geq => ">=",
                BinaryOp::BitwiseAnd => "&",
                BinaryOp::BitwiseOr => "|",
                BinaryOp::Xor => "^",
                BinaryOp::ShiftLeft => "<<",
                BinaryOp::ShiftRight => ">>",
            }
        )
    }
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignOp::Assign => write!(f, "="),
            AssignOp::AddAssign => write!(f, "+="),
            AssignOp::SubAssign => write!(f, "-="),
            AssignOp::MulAssign => write!(f, "*="),
            AssignOp::DivAssign => write!(f, "/="),
            AssignOp::ModAssign => write!(f, "%="),
            AssignOp::AndAssign => write!(f, "&="),
            AssignOp::OrAssign => write!(f, "|="),
            AssignOp::XorAssign => write!(f, "^="),
            AssignOp::ShlAssign => write!(f, "<<="),
            AssignOp::ShrAssign => write!(f, ">>="),
        }
    }
}

impl TryFrom<&Symbol> for UnaryOp {
    type Error = anyhow::Error;

    fn try_from(value: &Symbol) -> std::result::Result<Self, Self::Error> {
        match value {
            Symbol::Arithmetic(Arithmetic::Minus) => Ok(UnaryOp::Neg),
            Symbol::Logical(Logical::Not) => Ok(UnaryOp::Not),
            Symbol::Bitwise(Bitwise::And) => Ok(UnaryOp::Ref),
            Symbol::Arithmetic(Arithmetic::Times) => Ok(UnaryOp::Deref),
            igl => Err(anyhow::anyhow!("Invalid unary operator {:?}", igl)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    Xor,
    BitwiseAnd,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
}

impl BinaryOp {
    pub fn op_type(&self) -> OperatorType {
        use OperatorType::*;
        match self {
            BinaryOp::Add | BinaryOp::Sub => Additive,
            BinaryOp::Mul | BinaryOp::Mod | BinaryOp::Div => Multiplicative,
            BinaryOp::And => LogicalAnd,
            BinaryOp::Or => LogicalOr,
            BinaryOp::Eq => Equality,
            BinaryOp::Neq => Equality,
            BinaryOp::Lt => Relational,
            BinaryOp::Gt => Relational,
            BinaryOp::Leq => Relational,
            BinaryOp::Geq => Relational,
            BinaryOp::Xor => Relational,
            BinaryOp::BitwiseAnd => Bitwise,
            BinaryOp::BitwiseOr => Bitwise,
            BinaryOp::ShiftLeft => Bitwise,
            BinaryOp::ShiftRight => Bitwise,
        }
    }
}

impl TryFrom<&Token> for BinaryOp {
    type Error = anyhow::Error;

    fn try_from(value: &Token) -> std::result::Result<Self, Self::Error> {
        match value {
            Token::Symbol(s) => Self::try_from(s),
            _ => Err(anyhow::anyhow!("Invalid binary operator {:?}", value)),
        }
    }
}

impl TryFrom<&Symbol> for BinaryOp {
    type Error = anyhow::Error;

    fn try_from(value: &Symbol) -> std::result::Result<Self, Self::Error> {
        match value {
            Symbol::Arithmetic(Arithmetic::Plus) => Ok(BinaryOp::Add),
            Symbol::Arithmetic(Arithmetic::Minus) => Ok(BinaryOp::Sub),
            Symbol::Arithmetic(Arithmetic::Times) => Ok(BinaryOp::Mul),
            Symbol::Arithmetic(Arithmetic::Divide) => Ok(BinaryOp::Div),
            Symbol::Arithmetic(Arithmetic::Mod) => Ok(BinaryOp::Mod),
            Symbol::Logical(Logical::And) => Ok(BinaryOp::And),
            Symbol::Logical(Logical::Or) => Ok(BinaryOp::Or),
            Symbol::Bitwise(Bitwise::Or) => Ok(BinaryOp::BitwiseOr),
            Symbol::Bitwise(Bitwise::Xor) => Ok(BinaryOp::Xor),
            Symbol::Bitwise(Bitwise::And) => Ok(BinaryOp::BitwiseAnd),
            Symbol::Bitwise(Bitwise::ShiftLeft) => Ok(BinaryOp::ShiftLeft),
            Symbol::Bitwise(Bitwise::ShiftRight) => Ok(BinaryOp::ShiftRight),
            Symbol::Comparison(Comparison::Equal) => Ok(BinaryOp::Eq),
            Symbol::Comparison(Comparison::NotEqual) => Ok(BinaryOp::Neq),
            Symbol::Comparison(Comparison::LessThan) => Ok(BinaryOp::Lt),
            Symbol::Comparison(Comparison::GreaterThan) => Ok(BinaryOp::Gt),
            Symbol::Comparison(Comparison::LessThanOrEqual) => Ok(BinaryOp::Leq),
            Symbol::Comparison(Comparison::GreaterThanOrEqual) => Ok(BinaryOp::Geq),
            igl => Err(anyhow::anyhow!("Invalid binary operator {:?}", igl)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    StructInit {
        ty: ItemPath,
        fields: Vec<(String, NodeId)>,
    },
    Call {
        callee: NodeId,
        args: Vec<NodeId>,
    },
    MemberAccess {
        object: NodeId,
        member: NodeId,
        computed: bool,
    },
    UnaryOp {
        op: UnaryOp,
        operand: NodeId,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: NodeId,
        rhs: NodeId,
    },
    AssignmentOp {
        lhs: NodeId,
        op: AssignOp,
        rhs: NodeId,
    },
    Cast {
        ty: ItemPath,
        expr: NodeId,
    },
    Block {
        stmts: Vec<NodeId>,
    },
    If {
        cond: NodeId,
        then: NodeId,
        r#else: Option<NodeId>,
    },
    While {
        cond: NodeId,
        body: NodeId,
    },
    Loop {
        body: NodeId,
    },
    ScopeResolution {
        object: NodeId,
        member: NodeId,
    },
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(NodeId),
    Let {
        name: String,
        ty: ItemPath,
        value: Option<NodeId>,
    },
    Break {
        value: Option<NodeId>,
    },
    Return {
        value: Option<NodeId>,
    },
    Continue,
}

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Item(Item),
    Expr(Expr),
    Stmt(Stmt),
}

pub struct Parser<'a> {
    tokens: Vec<Spanned<Token>>,
    pos: usize,
    package: &'a mut Package,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Spanned<Token>>, package: &'a mut Package) -> Self {
        Self {
            tokens,
            pos: 0,
            package,
        }
    }

    pub fn current(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.pos)
    }

    pub fn advance(&mut self) -> Option<&Spanned<Token>> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    pub fn peek(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.pos + 1)
    }

    pub fn expect(&mut self, token: Token) -> Result<Token> {
        self.skip_whitespace();
        if let Some((true, tok)) = self
            .current()
            .map(|t| (t.value.same_kind(&token), t.value.clone()))
        {
            self.pos += 1;
            Ok(tok)
        } else {
            Err(anyhow::anyhow!(
                "expected {:?}, found {:?}",
                token,
                self.current().map(|t| &t.value)
            ))
        }
    }

    pub fn parse_unit(&mut self, name: String, parent: Option<UnitId>) -> Result<UnitId> {
        debug!("parse_unit");
        let root = parent.is_none();
        let unit = Unit {
            name,
            parent,
            members: HashMap::new(),
            ast_nodes: SlotMap::with_key(),
        };
        let unit_id = self.package.units.insert(unit);
        if root {
            self.package.root = unit_id;
        }
        self.skip_whitespace();
        self.parse_unit_body(unit_id)?;
        Ok(unit_id)
    }

    fn parse_unit_body(&mut self, unit_id: UnitId) -> Result<()> {
        debug!("parse_unit_body");
        let mut public = false;
        self.skip_whitespace();
        while let Some(current) = self.advance() {
            match &current.value {
                // Public item
                Token::Visibility(Visibility::Public) if !public => public = true,
                // Anything else is
                Token::Keyword(kw) => {
                    let pb = public;
                    if public {
                        public = false
                    };
                    match kw {
                        Keyword::Fn => self.parse_fn(unit_id, pb)?,
                        Keyword::Mod => self.parse_submodule(unit_id, pb)?,
                        Keyword::Struct => self.parse_struct_def(unit_id, pb)?,
                        Keyword::Type => self.parse_type_alias(unit_id, pb)?,
                        illegal => return Err(anyhow::anyhow!("illegal kw {:?}", illegal)),
                    }
                    self.skip_whitespace();
                }
                Token::Visibility(Visibility::Public) if public => {
                    unreachable!(
                        "The previous token set public to true, so this should never happen"
                    )
                }
                Token::Visibility(Visibility::Private) => {
                    unreachable!("A token should never actually have type Visibility::Private")
                }
                Token::Newline => {}
                illegal => return Err(anyhow::anyhow!("Invalid token {:?} in unit body", illegal)),
            }
            self.skip_whitespace();
        }
        Ok(())
    }

    fn parse_type_alias(&mut self, unit_id: UnitId, public: bool) -> Result<()> {
        debug!("parse_type_alias");
        self.skip_whitespace();
        let Some(Token::Ident(name)) = self.advance().map(|t| t.value.clone()) else {
            return Err(anyhow::anyhow!("expected identifier"));
        };
        self.expect(Token::Symbol(lex::Symbol::Assignment(
            lex::Assignment::Assign,
        )))?;

        let ty = self.parse_path()?;

        let unit: &mut Unit = self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?;

        let id = unit.ast_nodes.insert(ASTNode::Item(Item::TypeDef {
            vis: if public {
                Visibility::Public
            } else {
                Visibility::Private
            },
            name: ItemPath::Name(name.clone()),
            ty,
        }));
        unit.members.insert(name, id);

        Ok(())
    }

    fn parse_path(&mut self) -> Result<ItemPath> {
        debug!("parse_path");
        self.skip_whitespace();
        let mut path = Vec::new();

        let mut external = false;
        if let Some(Token::Symbol(lex::Symbol::Punctuation(lex::Punctuation::DoubleColon))) =
            self.current().map(|t| &t.value)
        {
            external = true;
            self.advance();
        }

        self.skip_whitespace();
        let root = self
            .advance()
            .ok_or(anyhow::anyhow!("expected ident or self:: or root::"))?
            .value
            .clone();

        while self
            .expect(Token::Symbol(lex::Symbol::Punctuation(
                lex::Punctuation::DoubleColon,
            )))
            .is_ok()
        {
            if let Token::Ident(name) = self.expect(Token::Ident("".into()))? {
                path.push(name);
            }
            self.skip_whitespace();
        }
        Ok(match root {
            Token::Keyword(Keyword::Self_) => ItemPath::Relative(path),
            Token::Keyword(Keyword::Root) => ItemPath::Absolute(path),
            Token::Ident(name) => {
                if external {
                    path.insert(0, name);
                    ItemPath::External(path)
                } else if path.is_empty() {
                    ItemPath::Name(name)
                } else {
                    path.insert(0, name);
                    ItemPath::Relative(path)
                }
            }
            Token::Keyword(Keyword::Super) => unimplemented!(),
            _ => return Err(anyhow::anyhow!("expected ident or self:: or root::")),
        })
    }

    fn parse_struct_def(&mut self, unit_id: UnitId, public: bool) -> Result<()> {
        debug!("parse_struct_def");
        self.skip_whitespace();
        let Some(Token::Ident(name)) = self.advance().map(|t| t.value.clone()) else {
            return Err(anyhow::anyhow!("expected identifier"));
        };

        self.skip_whitespace();
        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::OpenBrace,
        )))?;

        let mut fields = Vec::new();

        loop {
            self.skip_whitespace();
            let Ok(name) = self.expect_ident() else {
                break;
            };
            self.expect(Token::Symbol(lex::Symbol::Punctuation(
                lex::Punctuation::Colon,
            )))?;
            let ty = self.parse_path()?;
            fields.push((name, ty));
            if let Ok(_) = self.expect(Token::Symbol(lex::Symbol::Punctuation(
                lex::Punctuation::Comma,
            ))) {
                continue;
            } else {
                break;
            }
        }
        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::CloseBrace,
        )))?;

        let unit: &mut Unit = self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?;

        let id = unit.ast_nodes.insert(ASTNode::Item(Item::StructDef {
            vis: Self::vis(public),
            name: ItemPath::Name(name.clone()),
            fields,
        }));
        unit.members.insert(name, id);
        self.skip_whitespace();

        Ok(())
    }

    fn vis(public: bool) -> Visibility {
        if public {
            Visibility::Public
        } else {
            Visibility::Private
        }
    }

    fn parse_submodule(&mut self, unit_id: UnitId, public: bool) -> Result<()> {
        debug!("parse_submodule");
        self.skip_whitespace();
        let Some(Token::Ident(name)) = self.advance().map(|t| t.value.clone()) else {
            return Err(anyhow::anyhow!("expected identifier"));
        };

        let unit = Unit {
            name: name.clone(),
            parent: Some(unit_id),
            ast_nodes: SlotMap::with_key(),
            members: HashMap::new(),
        };
        let new_unit = self.package.units.insert(unit);
        let parent_unit = self.package.units.get_mut(unit_id).unwrap();
        let node = parent_unit.ast_nodes.insert(ASTNode::Item(Item::Submodule {
            vis: Self::vis(public),
            name: ItemPath::Name(name.clone()),
            id: new_unit,
        }));
        parent_unit.members.insert(name, node);

        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::OpenBrace,
        )))?;

        let mut public = false;
        self.skip_whitespace();
        while let Some(current) = match self.advance() {
            Some(e)
                if e.value.same_kind(&Token::Symbol(lex::Symbol::Punctuation(
                    lex::Punctuation::CloseBrace,
                ))) =>
            {
                None
            }
            Some(v) => Some(v),
            None => None,
        } {
            match &current.value {
                // Public item
                Token::Visibility(Visibility::Public) if !public => public = true,
                // Anything else is private
                Token::Keyword(kw) => {
                    let pb = public;
                    if public {
                        public = false
                    };
                    match kw {
                        Keyword::Fn => self.parse_fn(new_unit, pb)?,
                        Keyword::Mod => self.parse_submodule(new_unit, pb)?,
                        Keyword::Struct => self.parse_struct_def(new_unit, pb)?,
                        Keyword::Type => self.parse_type_alias(new_unit, pb)?,
                        _ => return Err(anyhow::anyhow!("Expected item, found {:?}", kw)),
                    }
                }
                Token::Visibility(Visibility::Public) if public => {
                    unreachable!(
                        "The previous token set public to true, so this should never happen"
                    )
                }
                Token::Visibility(Visibility::Private) => {
                    unreachable!("A token should never actually have type Visibility::Private")
                }
                igl => {
                    unreachable!("{:?} - {:?}", igl, current.span)
                }
            }
        }

        self.skip_whitespace();
        Ok(())
    }

    fn expect_ident(&mut self) -> Result<String> {
        self.skip_whitespace();
        let Some(Token::Ident(name)) = self.advance().map(|t| t.value.clone()) else {
            return Err(anyhow::anyhow!("expected identifier"));
        };
        self.skip_whitespace();
        Ok(name)
    }

    fn parse_fn(&mut self, unit_id: UnitId, public: bool) -> Result<()> {
        debug!("parse_fn");
        let vis = Self::vis(public);
        self.skip_whitespace();
        let Some(Token::Ident(name)) = self.advance().map(|t| t.value.clone()) else {
            return Err(anyhow::anyhow!("expected identifier"));
        };

        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::OpenParen,
        )))?;

        let mut params = Vec::new();
        loop {
            if self
                .expect(Token::Symbol(lex::Symbol::Punctuation(
                    lex::Punctuation::CloseParen,
                )))
                .is_ok()
            {
                break;
            }

            self.skip_whitespace();
            let Some(Token::Ident(name)) = self.advance().map(|t| t.value.clone()) else {
                return Err(anyhow::anyhow!("expected identifier"));
            };

            self.expect(Token::Symbol(lex::Symbol::Punctuation(
                lex::Punctuation::Colon,
            )))?;

            let ty = self.parse_path()?;

            params.push((name, ty));

            self.skip_whitespace();
            if let Ok(_) = self.expect(Token::Symbol(lex::Symbol::Punctuation(
                lex::Punctuation::Comma,
            ))) {
                continue;
            } else {
                self.expect(Token::Symbol(lex::Symbol::Punctuation(
                    lex::Punctuation::CloseParen,
                )))?;
                break;
            }
        }

        self.skip_whitespace();
        let ret_ty = if let Some(Token::Symbol(lex::Symbol::Punctuation(Punctuation::RightArrow))) =
            self.peek().map(|t| &t.value)
        {
            self.skip_whitespace();
            self.advance();
            Some(self.parse_path()?)
        } else {
            None
        };
        self.skip_whitespace();
        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::OpenBrace,
        )))?;
        self.skip_whitespace();
        let body = self.parse_block(unit_id)?;
        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::CloseBrace,
        )))?;
        let unit: &mut Unit = self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?;
        let id = unit.ast_nodes.insert(ASTNode::Item(Item::FunctionDef {
            vis,
            name: ItemPath::Name(name.clone()),
            params,
            ret_ty,
            body,
        }));
        unit.members.insert(name, id);
        self.skip_whitespace();

        Ok(())
    }

    fn parse_block(&mut self, unit_id: UnitId) -> Result<Vec<NodeId>> {
        debug!("parse_block");
        let mut nodes = Vec::new();
        loop {
            if let Some(Token::Symbol(lex::Symbol::Punctuation(lex::Punctuation::CloseBrace))) =
                self.current().map(|t| &t.value)
            {
                break;
            }
            nodes.push(self.parse_stmt(unit_id)?);
        }
        Ok(nodes)
    }

    fn parse_stmt(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_stmt");
        match self
            .current()
            .map(|t| &t.value)
            .ok_or(anyhow::anyhow!("EOF (in Parser::parse_stmt)"))?
        {
            Token::Keyword(Keyword::Let) => self.parse_let_stmt(unit_id),
            Token::Keyword(Keyword::Return) => self.parse_return_stmt(unit_id),
            Token::Keyword(Keyword::Break) => self.parse_break_stmt(unit_id),
            Token::Keyword(Keyword::Continue) => self.parse_continue_stmt(unit_id),
            _ => self.parse_expr_stmt(unit_id),
        }
    }

    fn parse_expr_stmt(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_expr_stmt: {:?}", self.current());
        self.parse_expr(unit_id)
    }

    fn parse_let_stmt(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_let_stmt");
        self.expect(Token::Keyword(Keyword::Let))?;
        let name = self.expect_ident()?;
        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::Colon,
        )))?;

        let ty = self.parse_path()?;
        let init = if let Some(Token::Symbol(lex::Symbol::Assignment(lex::Assignment::Assign))) =
            self.current().map(|t| &t.value)
        {
            self.advance();
            Some(self.parse_expr(unit_id)?)
        } else {
            None
        };
        let unit: &mut Unit = self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?;
        let id = unit.ast_nodes.insert(ASTNode::Stmt(Stmt::Let {
            name,
            ty,
            value: init,
        }));
        Ok(id)
    }

    fn parse_return_stmt(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_return_stmt");
        self.expect(Token::Keyword(Keyword::Return))?;
        if let Some(Token::Newline) = self.peek().map(|t| &t.value) {
            self.advance();
            Ok(self
                .package
                .units
                .get_mut(unit_id)
                .ok_or(anyhow::anyhow!("unit not found"))?
                .ast_nodes
                .insert(ASTNode::Stmt(Stmt::Return { value: None })))
        } else {
            let value = self.parse_expr(unit_id)?;
            self.expect(Token::Newline)?;
            Ok(self
                .package
                .units
                .get_mut(unit_id)
                .ok_or(anyhow::anyhow!("unit not found"))?
                .ast_nodes
                .insert(ASTNode::Stmt(Stmt::Return { value: Some(value) })))
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(Token::Newline) = self.current().map(|t| &t.value) {
            self.advance();
        }
    }

    fn parse_break_stmt(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_break_stmt");
        self.expect(Token::Keyword(Keyword::Break))?;
        if let Some(Token::Newline) = self.peek().map(|t| &t.value) {
            self.advance();
            return Ok(self
                .package
                .units
                .get_mut(unit_id)
                .ok_or(anyhow::anyhow!("unit not found"))?
                .ast_nodes
                .insert(ASTNode::Stmt(Stmt::Break { value: None })));
        }
        let expr = self.parse_expr(unit_id)?;
        Ok(self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?
            .ast_nodes
            .insert(ASTNode::Stmt(Stmt::Break { value: Some(expr) })))
    }

    fn parse_continue_stmt(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_continue_stmt");
        self.expect(Token::Keyword(Keyword::Continue))?;
        Ok(self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?
            .ast_nodes
            .insert(ASTNode::Stmt(Stmt::Continue)))
    }

    fn parse_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_expr: {:?}", self.current());
        match self
            .current()
            .ok_or(anyhow::anyhow!("unexpected eof"))?
            .value
        {
            Token::Keyword(Keyword::If) => self.parse_if_expr(unit_id),
            Token::Keyword(Keyword::While) => self.parse_while_expr(unit_id),
            Token::Keyword(Keyword::For) => self.parse_for_expr(unit_id),
            Token::Keyword(Keyword::Loop) => self.parse_loop_expr(unit_id),
            Token::Symbol(Symbol::Punctuation(Punctuation::OpenBrace)) => {
                self.parse_block_expr(unit_id)
            }
            _ => self.parse_assignment(unit_id),
        }
    }

    fn parse_if_expr(&mut self, unit_id: UnitId) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_if_expr");
        self.expect(Token::Keyword(Keyword::If))?;
        let cond = self.parse_expr(unit_id)?;
        let then = self.parse_expr(unit_id)?;
        let r#else = if let Some(Token::Keyword(Keyword::Else)) = self.peek().map(|t| &t.value) {
            self.advance();
            Some(self.parse_expr(unit_id)?)
        } else {
            None
        };
        Ok(self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?
            .ast_nodes
            .insert(ASTNode::Expr(Expr::If { cond, then, r#else })))
    }

    fn parse_while_expr(&mut self, unit_id: UnitId) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_while_expr");
        self.expect(Token::Keyword(Keyword::While))?;
        let cond = self.parse_expr(unit_id)?;
        let body = self.parse_expr(unit_id)?;
        Ok(self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?
            .ast_nodes
            .insert(ASTNode::Expr(Expr::While { cond, body })))
    }

    fn parse_for_expr(&self, _unit_id: UnitId) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_for_expr");
        unimplemented!("For expressions are more complex so won't be implemented yet")
    }

    fn parse_loop_expr(&mut self, unit_id: UnitId) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_loop_expr");
        self.expect(Token::Keyword(Keyword::Loop))?;
        let block = self.parse_block_expr(unit_id)?;
        Ok(self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?
            .ast_nodes
            .insert(ASTNode::Expr(Expr::Loop { body: block })))
    }

    fn parse_block_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_block_expr");
        self.expect(Token::Symbol(Symbol::Punctuation(Punctuation::OpenBrace)))?;
        let block = self.parse_block(unit_id)?;
        self.expect(Token::Symbol(Symbol::Punctuation(Punctuation::CloseBrace)))?;
        Ok(self
            .package
            .units
            .get_mut(unit_id)
            .ok_or(anyhow::anyhow!("unit not found"))?
            .ast_nodes
            .insert(ASTNode::Expr(Expr::Block { stmts: block })))
    }

    fn binary_expr_helper<F>(
        &mut self,
        unit_id: UnitId,
        builder: F,
        ty: OperatorType,
    ) -> Result<NodeId>
    where
        F: Fn(&mut Self, UnitId) -> Result<NodeId>,
    {
        let mut lhs = builder(self, unit_id)?;

        while self
            .current()
            .map(|c| c.value.op_type().is_some())
            .unwrap_or(false)
        {
            let Ok(op) = BinaryOp::try_from(&self.current().unwrap().value) else {
                break;
            };
            if op.op_type() != ty {
                break;
            }
            // self.advance();
            let rhs = builder(self, unit_id)?;
            lhs = self
                .package
                .units
                .get_mut(unit_id)
                .unwrap()
                .ast_nodes
                .insert(ASTNode::Expr(Expr::BinaryOp { op, lhs, rhs }));
        }
        Ok(lhs)
    }

    pub fn parse_logical_or(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_logical_or");
        self.binary_expr_helper(unit_id, Self::parse_logical_and, OperatorType::LogicalOr)
    }

    pub fn parse_logical_and(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_logical_and");
        self.binary_expr_helper(unit_id, Self::parse_equality, OperatorType::LogicalAnd)
    }

    pub fn parse_equality(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_equality");
        self.binary_expr_helper(unit_id, Self::parse_relational, OperatorType::Equality)
    }

    pub fn parse_relational(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_relational");
        self.binary_expr_helper(unit_id, Self::parse_additive, OperatorType::Relational)
    }

    pub fn parse_additive(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_additive");
        self.binary_expr_helper(unit_id, Self::parse_multiplicative, OperatorType::Additive)
    }

    pub fn parse_multiplicative(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_multiplicative");
        // self.binary_expr_helper(unit_id, Self::parse_as_expr, OperatorType::Multiplicative)
        self.binary_expr_helper(
            unit_id,
            Self::parse_unary_expr,
            OperatorType::Multiplicative,
        )
    }

    // fn parse_as_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
    // debug!("parse_as_expr");
    //     let mut expr = self.parse_unary_expr(unit_id)?;
    //     if let Some(Token::Keyword(Keyword::As)) = self.current().map(|t| t.value.clone()) {
    //         self.advance();
    //         let ty = self.parse_path()?;
    //         expr = self
    //             .package
    //             .units
    //             .get_mut(unit_id)
    //             .unwrap()
    //             .ast_nodes
    //             .insert(ASTNode::Expr(Expr::Cast { expr, ty }));
    //     }
    //     Ok(expr)
    // }

    fn parse_unary_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_unary_expr");
        let current = self
            .current()
            .ok_or(anyhow::anyhow!(
                "unexpected EOF (in Parser::parse_unary_expr)"
            ))?
            .clone();
        match &current.value {
            tok @ Token::Symbol(sym) if tok.is_unary_op() => {
                self.advance();
                let operand = self.parse_expr(unit_id)?;
                let op = UnaryOp::try_from(sym)?;
                Ok(self
                    .package
                    .units
                    .get_mut(unit_id)
                    .ok_or(anyhow::anyhow!("unit {unit_id:?} not found"))?
                    .ast_nodes
                    .insert(ASTNode::Expr(Expr::UnaryOp { op, operand })))
            }
            _ => self.parse_call_member_expr(unit_id),
        }
    }

    fn parse_call_member_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_method_or_member_expr 2");
        debug!("current: {:?}", self.current());
        let member = self.parse_member_expr(unit_id)?;
        debug!("member: {:?}", member);
        if let Some(Token::Symbol(Symbol::Punctuation(Punctuation::OpenParen))) =
            self.current().map(|t| &t.value)
        {
            self.advance();
            self.parse_call_expr(unit_id, member)
        } else {
            Ok(member)
        }
    }

    fn parse_call_expr(&mut self, unit_id: UnitId, callee: NodeId) -> Result<NodeId> {
        debug!("parse_call_expr");
        let args = self.parse_fn_args(unit_id)?;
        Ok(self
            .package
            .units
            .get_mut(unit_id)
            .unwrap()
            .ast_nodes
            .insert(ASTNode::Expr(Expr::Call { callee, args })))
    }

    fn parse_fn_args(&mut self, unit_id: UnitId) -> Result<Vec<NodeId>> {
        debug!("parse_fn_args");
        let mut args = Vec::new();

        loop {
            match self.current().map(|t| &t.value) {
                Some(Token::Symbol(Symbol::Punctuation(Punctuation::CloseParen))) => {
                    self.advance();
                    break;
                }
                _ => {
                    args.push(self.parse_expr(unit_id)?);
                    if let Some(Token::Symbol(Symbol::Punctuation(Punctuation::Comma))) =
                        self.current().map(|t| &t.value)
                    {
                        self.advance();
                    } else {
                        self.expect(Token::Symbol(Symbol::Punctuation(Punctuation::CloseParen)))?;
                        break;
                    }
                }
            }
        }

        Ok(args)
    }

    fn parse_member_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_member_expr 3 {:?}", self.current());
        let mut object = self.parse_primary_expr(unit_id)?;
        let mut scope_resolution = false;

        while let Some(Token::Symbol(Symbol::Punctuation(
            sym @ (Punctuation::Dot | Punctuation::DoubleColon | Punctuation::OpenBracket),
        ))) = self.current().map(|c| c.value.clone())
        {
            self.advance();
            match sym {
                Punctuation::Dot => {
                    if scope_resolution {
                        return Err(anyhow::anyhow!(
                            "unexpected '.' after '::' (cannot use instance access on static path)"
                        ));
                    }
                    let member = self.parse_identifier(unit_id)?;
                    object = self
                        .package
                        .units
                        .get_mut(unit_id)
                        .ok_or(anyhow::anyhow!("unit {unit_id:?} not found"))?
                        .ast_nodes
                        .insert(ASTNode::Expr(Expr::MemberAccess {
                            object,
                            member,
                            computed: false,
                        }));
                }
                Punctuation::DoubleColon => {
                    scope_resolution = true;
                    let member = self.parse_identifier(unit_id)?;
                    object = self
                        .package
                        .units
                        .get_mut(unit_id)
                        .ok_or(anyhow::anyhow!("unit {unit_id:?} not found"))?
                        .ast_nodes
                        .insert(ASTNode::Expr(Expr::ScopeResolution { object, member }));
                }
                Punctuation::OpenBracket => {
                    if scope_resolution {
                        return Err(anyhow::anyhow!(
                            "unexpected '[' after '::' (cannot use index access on static path)"
                        ));
                    }
                    let member = self.parse_expr(unit_id)?;
                    self.expect(Token::Symbol(Symbol::Punctuation(
                        Punctuation::CloseBracket,
                    )))?;
                    object = self
                        .package
                        .units
                        .get_mut(unit_id)
                        .ok_or(anyhow::anyhow!("unit {unit_id:?} not found"))?
                        .ast_nodes
                        .insert(ASTNode::Expr(Expr::MemberAccess {
                            object,
                            member,
                            computed: true,
                        }));
                }
                _ => unreachable!(),
            }
        }
        Ok(object)
    }

    fn parse_primary_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_primary_expr");
        match self.current().map(|c| &c.value).ok_or(anyhow::anyhow!(
            "Unexpected EOF (in Parser::parse_primary_expr)"
        ))? {
            Token::Literal(_) => self.parse_literal(unit_id),
            Token::Symbol(Symbol::Punctuation(Punctuation::OpenParen)) => {
                self.parse_paren_expr(unit_id)
            }
            Token::Ident(_) => {
                match self.peek().map(|c| &c.value).ok_or(anyhow::anyhow!(
                    "Unexpected EOF (in Parser::parse_primary_expr)"
                ))? {
                    Token::Symbol(Symbol::Punctuation(Punctuation::OpenBrace)) => {
                        self.parse_struct_init(unit_id)
                    }
                    _ => self.parse_identifier(unit_id),
                }
            }
            _ => self.parse_lhs_expr(unit_id),
        }
    }

    fn parse_lhs_expr(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_lhs_expr 1");
        self.parse_call_member_expr(unit_id)
    }

    fn parse_identifier(&mut self, unit_id: UnitId) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_identifier");
        match self.advance().map(|c| c.value.clone()) {
            Some(Token::Ident(ident)) => Ok(self
                .package
                .units
                .get_mut(unit_id)
                .ok_or(anyhow::anyhow!("unit {unit_id:?} not found"))?
                .ast_nodes
                .insert(ASTNode::Expr(Expr::Ident(ident)))),
            _ => Err(anyhow::anyhow!("expected identifier")),
        }
    }

    fn parse_struct_init(
        &mut self,
        _unit_id: UnitId,
    ) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_struct_init");
        unimplemented!("struct init not yet implemented")
    }

    fn parse_paren_expr(&mut self, unit_id: UnitId) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_paren_expr");
        self.expect(Token::Symbol(Symbol::Punctuation(Punctuation::OpenParen)))?;
        let expr = self.parse_expr(unit_id)?;
        self.expect(Token::Symbol(Symbol::Punctuation(Punctuation::CloseParen)))?;
        Ok(expr)
    }

    fn parse_literal(&mut self, unit_id: UnitId) -> std::result::Result<NodeId, anyhow::Error> {
        debug!("parse_literal");
        match self.advance().map(|c| c.value.clone()) {
            Some(Token::Literal(lit)) => Ok(self
                .package
                .units
                .get_mut(unit_id)
                .ok_or(anyhow::anyhow!("unit {unit_id:?} not found"))?
                .ast_nodes
                .insert(ASTNode::Expr(Expr::Literal(lit)))),
            _ => Err(anyhow::anyhow!("expected literal")),
        }
    }

    fn parse_assignment(&mut self, unit_id: UnitId) -> Result<NodeId> {
        debug!("parse_assignment");
        debug!("curr: {:?}", self.current());
        let lhs = self.parse_logical_or(unit_id)?;
        debug!("curr2: {:?}", self.current());
        if let Some(Token::Symbol(Symbol::Assignment(op))) = self.current().map(|t| t.value.clone())
        {
            self.advance();
            let rhs = self.parse_assignment(unit_id)?;
            Ok(self
                .package
                .units
                .get_mut(unit_id)
                .unwrap()
                .ast_nodes
                .insert(ASTNode::Expr(Expr::AssignmentOp {
                    op: match op {
                        Assignment::Assign => AssignOp::Assign,
                        Assignment::AddAssign => AssignOp::AddAssign,
                        Assignment::SubAssign => AssignOp::SubAssign,
                        Assignment::MulAssign => AssignOp::MulAssign,
                        Assignment::DivAssign => AssignOp::DivAssign,
                        Assignment::ModAssign => AssignOp::ModAssign,
                        Assignment::AndAssign => AssignOp::AndAssign,
                        Assignment::OrAssign => AssignOp::OrAssign,
                        Assignment::XorAssign => AssignOp::XorAssign,
                        Assignment::ShlAssign => AssignOp::ShlAssign,
                        Assignment::ShrAssign => AssignOp::ShrAssign,
                    },
                    lhs,
                    rhs,
                })))
        } else {
            Ok(lhs)
        }
    }

    // fn parse_primary_expr(&self) -> std::result::Result<NodeId, anyhow::Error> {
    // debug!("parse_primary_expr");
    //     todo!()
    // }
    //
    // fn parse_binary_expr(&mut self, unit_id: UnitId, min_prec: u8) -> Result<NodeId> {
    // debug!("parse_binary_expr");
    //     let mut lhs = self.parse_unary_expr(unit_id)?;
    //     loop {
    //         let prec = self.peek().map(|t| t.value.precedence()).unwrap_or(0);
    //         if prec < min_prec {
    //             break;
    //         }
    //         let op = match self.advance() {
    //             Some(op) if op.value.is_binary_op() => match op.value {
    //                 Token::Symbol(Symbol::Arithmetic(Arithmetic::Plus)) => BinaryOp::Add,
    //                 Token::Symbol(Symbol::Arithmetic(Arithmetic::Minus)) => BinaryOp::Sub,
    //                 Token::Symbol(Symbol::Arithmetic(Arithmetic::Times)) => BinaryOp::Mul,
    //                 Token::Symbol(Symbol::Arithmetic(Arithmetic::Divide)) => BinaryOp::Div,
    //                 Token::Symbol(Symbol::Arithmetic(Arithmetic::Mod)) => BinaryOp::Mod,
    //                 Token::Symbol(Symbol::Logical(Logical::And)) => BinaryOp::And,
    //                 Token::Symbol(Symbol::Logical(Logical::Or)) => BinaryOp::Or,
    //                 Token::Symbol(Symbol::Bitwise(Bitwise::Xor)) => BinaryOp::Xor,
    //                 Token::Symbol(Symbol::Bitwise(Bitwise::And)) => BinaryOp::BitwiseAnd,
    //                 Token::Symbol(Symbol::Bitwise(Bitwise::Or)) => BinaryOp::BitwiseOr,
    //                 Token::Symbol(Symbol::Bitwise(Bitwise::ShiftLeft)) => BinaryOp::ShiftLeft,
    //                 Token::Symbol(Symbol::Bitwise(Bitwise::ShiftRight)) => BinaryOp::ShiftRight,
    //                 Token::Symbol(Symbol::Comparison(Comparison::Equal)) => BinaryOp::Eq,
    //                 Token::Symbol(Symbol::Comparison(Comparison::NotEqual)) => BinaryOp::Neq,
    //                 Token::Symbol(Symbol::Comparison(Comparison::LessThan)) => BinaryOp::Lt,
    //                 Token::Symbol(Symbol::Comparison(Comparison::LessThanOrEqual)) => BinaryOp::Leq,
    //                 Token::Symbol(Symbol::Comparison(Comparison::GreaterThan)) => BinaryOp::Gt,
    //                 Token::Symbol(Symbol::Comparison(Comparison::GreaterThanOrEqual)) => {
    //                     BinaryOp::Geq
    //                 }
    //                 _ => unreachable!(),
    //             },
    //             Some(op) => {
    //                 return Err(anyhow::anyhow!("expected binary operator, found {:?}", op))
    //             }
    //             None => return Err(anyhow::anyhow!("unexpected EOF")),
    //         };
    //         let rhs = self.parse_binary_expr(unit_id, prec + 1)?;
    //         lhs = self
    //             .package
    //             .units
    //             .get_mut(unit_id)
    //             .ok_or(anyhow::anyhow!("unit not found"))?
    //             .ast_nodes
    //             .insert(ASTNode::Expr(Expr::BinaryOp { lhs, op, rhs }));
    //     }
    //     Ok(lhs)
    // }
}
