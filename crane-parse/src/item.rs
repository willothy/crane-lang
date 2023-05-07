use crane_lex::Visibility;

use crate::{
    path::ItemPath,
    unit::{NodeId, UnitId},
};

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
        body: NodeId,
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
    StaticDef {
        vis: Visibility,
        ty: ItemPath,
        name: String,
        value: NodeId,
    },
}
