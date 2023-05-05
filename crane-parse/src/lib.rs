use std::{collections::HashMap, path::Iter};

use anyhow::Result;
use crane_lex as lex;
use lex::{Keyword, Literal, SpannedToken, Token, Visibility};
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

pub struct TypeDef {
    name: String,
    // TODO: add fields
}

#[derive(Debug)]
pub struct Package {
    units: SlotMap<UnitId, Unit>,
    root: UnitId,
}

impl Package {
    pub fn new() -> Self {
        let mut units = SlotMap::with_key();
        let root = units.insert(Unit {
            parent: None,
            name: String::from("root"),
            ast_nodes: SlotMap::with_key(),
            members: HashMap::new(),
        });
        Self { units, root }
    }

    pub fn get_unit(&self, id: UnitId) -> Option<&Unit> {
        self.units.get(id)
    }
}

#[derive(Debug, PartialEq)]
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
        args: Vec<(String, ItemPath)>,
        ret_ty: ItemPath,
        body: Vec<NodeId>,
    },
    FunctionDecl {
        vis: Visibility,
        name: ItemPath,
        args: Vec<(String, ItemPath)>,
        ret: ItemPath,
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
    FieldAccess {
        base: NodeId,
        field: String,
    },
    Index {
        base: NodeId,
        index: NodeId,
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
    Cast {
        ty: ItemPath,
        expr: NodeId,
    },
    Block {
        stmts: Vec<NodeId>,
        expr: Option<NodeId>,
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
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(NodeId),
    Let {
        name: String,
        ty: ItemPath,
        value: NodeId,
    },
    Assign {
        lhs: NodeId,
        op: AssignOp,
        rhs: NodeId,
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
    tokens: Vec<SpannedToken<'a>>,
    pos: usize,
    package: &'a mut Package,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<SpannedToken<'a>>, package: &'a mut Package) -> Self {
        Self {
            tokens,
            pos: 0,
            package,
        }
    }

    pub fn current(&self) -> Option<&SpannedToken<'a>> {
        self.tokens.get(self.pos)
    }

    pub fn advance(&mut self) -> Option<&SpannedToken<'a>> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    pub fn peek(&self) -> Option<&SpannedToken<'a>> {
        self.tokens.get(self.pos + 1)
    }

    pub fn expect(&mut self, token: Token) -> Result<Token> {
        if let Some((true, tok)) = self
            .current()
            .map(|t| (t.kind.same_kind(&token), t.kind.clone()))
        {
            self.pos += 1;
            Ok(tok)
        } else {
            Err(anyhow::anyhow!("expected {:?}", token))
        }
    }

    pub fn parse_unit(&mut self, name: String, parent: Option<UnitId>) -> Result<UnitId> {
        let unit = Unit {
            name,
            parent,
            members: HashMap::new(),
            ast_nodes: SlotMap::with_key(),
        };
        let unit_id = self.package.units.insert(unit);
        self.parse_unit_body(unit_id)?;
        Ok(unit_id)
    }

    pub fn parse_unit_body(&mut self, unit_id: UnitId) -> Result<()> {
        let mut public = false;
        while let Some(current) = self.advance() {
            match &current.kind {
                // Public item
                Token::Visibility(Visibility::Public) if public == false => public = true,
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
                        _ => panic!(),
                    }
                }
                Token::Visibility(Visibility::Public) if public == true => {
                    unreachable!(
                        "The previous token set public to true, so this should never happen"
                    )
                }
                Token::Visibility(Visibility::Private) => {
                    unreachable!("A token should never actually have type Visibility::Private")
                }
                _ => {
                    // self.parse_priv_unit_body(unit_id)?;
                }
            }
        }
        Ok(())
    }

    fn parse_type_alias(&mut self, unit_id: UnitId, public: bool) -> Result<()> {
        let Some(Token::Ident(name)) = self.advance().map(|t| t.kind.clone()) else {
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

    fn get_parent_name(&self, unit_id: UnitId) -> Option<(UnitId, String)> {
        let unit = self.package.units.get(unit_id).unwrap();
        if let Some(parent) = unit.parent {
            return self
                .package
                .units
                .get(parent)
                .map(|u| (parent, u.name.clone()));
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

    fn parse_path(&mut self) -> Result<ItemPath> {
        let mut path = Vec::new();

        let mut external = false;
        if let Some(Token::Symbol(lex::Symbol::Punctuation(lex::Punctuation::DoubleColon))) =
            self.current().map(|t| &t.kind)
        {
            external = true;
            self.advance();
        }

        let root = self
            .advance()
            .ok_or(anyhow::anyhow!("expected ident or self:: or root::"))?
            .kind
            .clone();

        while let Ok(_) = self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::DoubleColon,
        ))) {
            if let Token::Ident(name) = self.expect(Token::Ident("".into()))? {
                path.push(name);
            }
        }
        Ok(match root {
            Token::Keyword(Keyword::Self_) => ItemPath::Relative(path),
            Token::Keyword(Keyword::Root) => ItemPath::Absolute(path),
            Token::Ident(name) => {
                if external {
                    path.insert(0, name.clone());
                    ItemPath::External(path)
                } else if path.len() == 0 {
                    ItemPath::Name(name.clone())
                } else {
                    path.insert(0, name.clone());
                    ItemPath::Relative(path)
                }
            }
            Token::Keyword(Keyword::Super) => unimplemented!(),
            _ => return Err(anyhow::anyhow!("expected ident or self:: or root::")),
        })
    }

    fn parse_struct_def(&mut self, unit_id: UnitId, public: bool) -> Result<()> {
        let Some(Token::Ident(name)) = self.advance().map(|t| t.kind.clone()) else {
            return Err(anyhow::anyhow!("expected identifier"));
        };

        self.expect(Token::Symbol(lex::Symbol::Punctuation(
            lex::Punctuation::OpenBrace,
        )))?;

        let mut fields = Vec::new();

        while let Some(Token::Ident(name)) = self.advance().map(|t| t.kind.clone()) {
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
                self.expect(Token::Symbol(lex::Symbol::Punctuation(
                    lex::Punctuation::CloseBrace,
                )))?;
                break;
            }
        }

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
        let Some(Token::Ident(name)) = self.advance().map(|t| t.kind.clone()) else {
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

        let mut public = false;
        while let Some(current) = match self.advance() {
            Some(e)
                if e.kind.same_kind(&Token::Symbol(lex::Symbol::Punctuation(
                    lex::Punctuation::CloseBrace,
                ))) =>
            {
                None
            }
            Some(v) => Some(v),
            None => None,
        } {
            match &current.kind {
                // Public item
                Token::Visibility(Visibility::Public) if public == false => public = true,
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
                        _ => panic!(),
                    }
                }
                Token::Visibility(Visibility::Public) if public == true => {
                    unreachable!(
                        "The previous token set public to true, so this should never happen"
                    )
                }
                Token::Visibility(Visibility::Private) => {
                    unreachable!("A token should never actually have type Visibility::Private")
                }
                _ => {
                    // self.parse_priv_unit_body(unit_id)?;
                }
            }
        }

        Ok(())
    }

    fn parse_fn(&self, unit_id: UnitId, public: bool) -> Result<()> {
        todo!()
    }
}
