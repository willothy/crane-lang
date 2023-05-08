use crane_lex::Literal;

use crate::{
    ops::{AssignOp, BinaryOp, UnaryOp},
    path::{ItemPath, TypeName},
    unit::NodeId,
};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    StructInit {
        ty: TypeName,
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
    Assignment {
        lhs: NodeId,
        op: AssignOp,
        rhs: NodeId,
    },
    Let {
        name: String,
        ty: TypeName,
        value: Option<NodeId>,
    },
    Break {
        value: Option<NodeId>,
    },
    Return {
        value: Option<NodeId>,
    },
    Continue,
    Cast {
        ty: TypeName,
        expr: NodeId,
    },
    Block {
        exprs: Vec<NodeId>,
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
        // object: NodeId,
        // member: NodeId,
        path: ItemPath,
    },
    List {
        exprs: Vec<NodeId>,
    },
    Closure {
        params: Vec<(String, TypeName)>,
        ret_ty: Option<TypeName>,
        body: NodeId,
    },
}
