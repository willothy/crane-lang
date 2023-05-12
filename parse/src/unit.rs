use std::collections::HashMap;

use slotmap::{new_key_type, SlotMap};

use crate::{ASTNode, Expr, Item};

new_key_type! {
    pub struct UnitId;
    pub struct NodeId;
}

#[derive(Debug)]
pub struct Unit<U, K, N, D = ()>
where
    U: slotmap::Key,
    K: slotmap::Key,
{
    parent: Option<U>,
    name: String,
    nodes: SlotMap<K, N>,
    members: HashMap<String, K>,
    data: D,
}

pub type ASTUnit = Unit<UnitId, NodeId, ASTNode>;

impl<U, K, N, D> Unit<U, K, N, D>
where
    U: slotmap::Key,
    K: slotmap::Key,
{
    pub fn new_with_data(name: String, parent: Option<U>, data: D) -> Self {
        Self {
            parent,
            name,
            nodes: SlotMap::with_key(),
            members: HashMap::new(),
            data,
        }
    }

    pub fn data(&self) -> &D {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut D {
        &mut self.data
    }
}

impl<U, K, N, D> Unit<U, K, N, D>
where
    U: slotmap::Key,
    K: slotmap::Key,
    D: Default,
{
    pub fn new(name: String, parent: Option<U>) -> Self {
        Self {
            parent,
            name,
            nodes: SlotMap::with_key(),
            members: HashMap::new(),
            data: Default::default(),
        }
    }
}

impl<U, K, N, D> Unit<U, K, N, D>
where
    U: slotmap::Key,
    K: slotmap::Key,
{
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn parent(&self) -> Option<U> {
        self.parent
    }

    pub fn node(&self, id: K) -> Option<&N> {
        self.nodes.get(id)
    }

    pub fn node_mut(&mut self, id: K) -> Option<&mut N> {
        self.nodes.get_mut(id)
    }

    pub fn new_node(&mut self, node: N) -> K {
        self.nodes.insert(node)
    }

    pub fn members(&self) -> &HashMap<String, K> {
        &self.members
    }

    pub fn members_mut(&mut self) -> &mut HashMap<String, K> {
        &mut self.members
    }
}

impl Unit<UnitId, NodeId, ASTNode> {
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
