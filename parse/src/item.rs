use crane_lex::Visibility;

use crate::{
    path::ItemPath,
    ty::Signature,
    unit::{NodeId, UnitId},
};

/// Module-level items
#[derive(Debug, PartialEq)]
pub enum Item {
    Submodule {
        vis: Visibility,
        name: String,
        id: UnitId,
    },
    Import {
        vis: Visibility,
        path: ItemPath,
    },
    FunctionDef {
        vis: Visibility,
        name: String,
        params: Vec<(String, Signature)>,
        ret_ty: Option<Signature>,
        body: NodeId,
    },
    FunctionDecl {
        vis: Visibility,
        name: String,
        args: Vec<(String, Signature)>,
        ret_ty: Option<Signature>,
    },
    StructDef {
        vis: Visibility,
        name: String,
        fields: Vec<(String, Signature)>,
    },
    TypeDef {
        vis: Visibility,
        name: String,
        ty: Signature,
    },
    ConstDef {
        vis: Visibility,
        name: String,
        ty: Signature,
        value: NodeId,
    },
    StaticDef {
        vis: Visibility,
        ty: Signature,
        name: String,
        value: NodeId,
    },
}
