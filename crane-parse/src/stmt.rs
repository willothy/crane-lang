use crate::{path::ItemPath, unit::NodeId};

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
