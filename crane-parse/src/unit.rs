use std::collections::HashMap;

use slotmap::{new_key_type, SlotMap};

use crate::{ASTNode, Expr, Item, Stmt};

new_key_type! {
    pub struct UnitId;
    pub struct NodeId;
}

#[derive(Debug)]
pub struct Unit {
    parent: Option<UnitId>,
    name: String,
    ast_nodes: SlotMap<NodeId, ASTNode>,
    members: HashMap<String, NodeId>,
}

impl Unit {
    pub fn new(name: String, parent: Option<UnitId>) -> Self {
        Self {
            parent,
            name,
            ast_nodes: SlotMap::with_key(),
            members: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn parent(&self) -> Option<UnitId> {
        self.parent
    }

    pub fn get_node(&self, id: NodeId) -> Option<&ASTNode> {
        self.ast_nodes.get(id)
    }

    pub fn members(&self) -> &HashMap<String, NodeId> {
        &self.members
    }

    pub fn new_item(&mut self, name: String, item: Item) -> NodeId {
        let id = self.ast_nodes.insert(ASTNode::Item(item));
        self.members.insert(name, id);
        id
    }

    pub fn new_expr(&mut self, expr: Expr) -> NodeId {
        self.ast_nodes.insert(ASTNode::Expr(expr))
    }

    pub fn new_stmt(&mut self, stmt: Stmt) -> NodeId {
        self.ast_nodes.insert(ASTNode::Stmt(stmt))
    }
}
