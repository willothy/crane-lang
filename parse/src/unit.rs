use std::collections::HashMap;

use slotmap::{new_key_type, SlotMap};

use crate::{ASTNode, Expr, Item};

new_key_type! {
    pub struct UnitId;
    pub struct NodeId;
}

pub trait Unit<U, K, M, N> {
    fn name(&self) -> &str;
    fn parent(&self) -> Option<U>;
    fn node(&self, id: K) -> Option<&N>;
    fn node_mut(&mut self, id: K) -> Option<&mut N>;
    fn new_node(&mut self, node: N) -> K;
    fn members(&self) -> &HashMap<String, M>;
    fn members_mut(&mut self) -> &mut HashMap<String, M>;
}

impl Unit<UnitId, NodeId, NodeId, ASTNode> for ASTUnit {
    fn name(&self) -> &str {
        &self.name
    }

    fn parent(&self) -> Option<UnitId> {
        self.parent
    }

    fn node(&self, id: NodeId) -> Option<&ASTNode> {
        self.nodes.get(id)
    }

    fn node_mut(&mut self, id: NodeId) -> Option<&mut ASTNode> {
        self.nodes.get_mut(id)
    }

    fn new_node(&mut self, node: ASTNode) -> NodeId {
        self.nodes.insert(node)
    }

    fn members(&self) -> &HashMap<String, NodeId> {
        &self.members
    }

    fn members_mut(&mut self) -> &mut HashMap<String, NodeId> {
        &mut self.members
    }
}

#[derive(Debug)]
pub struct ASTUnit {
    parent: Option<UnitId>,
    name: String,
    nodes: SlotMap<NodeId, ASTNode>,
    members: HashMap<String, NodeId>,
}

impl ASTUnit {
    pub fn new_with_data(name: String, parent: Option<UnitId>) -> Self {
        Self {
            parent,
            name,
            nodes: SlotMap::with_key(),
            members: HashMap::new(),
        }
    }
}

impl ASTUnit {
    pub fn new(name: String, parent: Option<UnitId>) -> Self {
        Self {
            parent,
            name,
            nodes: SlotMap::with_key(),
            members: HashMap::new(),
        }
    }

    pub fn new_item(&mut self, name: String, item: Item) -> NodeId {
        let id = self.nodes.insert(ASTNode::Item(item));
        self.members.insert(name, id);
        id
    }

    pub fn new_expr(&mut self, expr: Expr) -> NodeId {
        self.nodes.insert(ASTNode::Expr(expr))
    }

    pub fn new_error(&mut self) -> NodeId {
        self.nodes.insert(ASTNode::Error)
    }

    /// Wraps an expr node in a Result node
    pub(crate) fn make_result(&mut self, expr: NodeId) -> NodeId {
        self.new_expr(Expr::Result { value: expr })
    }
}
