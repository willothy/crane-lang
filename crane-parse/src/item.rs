use crane_lex::Visibility;

use crate::{
    path::{ItemPath, TypeName},
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
        params: Vec<(String, TypeName)>,
        ret_ty: Option<TypeName>,
        body: NodeId,
    },
    FunctionDecl {
        vis: Visibility,
        name: String,
        args: Vec<(String, TypeName)>,
        ret_ty: Option<TypeName>,
    },
    StructDef {
        vis: Visibility,
        name: String,
        fields: Vec<(String, TypeName)>,
    },
    TypeDef {
        vis: Visibility,
        name: String,
        ty: TypeName,
    },
    ConstDef {
        vis: Visibility,
        name: String,
        ty: TypeName,
        value: NodeId,
    },
    StaticDef {
        vis: Visibility,
        ty: TypeName,
        name: String,
        value: NodeId,
    },
}
