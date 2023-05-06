use std::fmt::Display;

use crane_lex::{Arithmetic, Bitwise, Comparison, Logical, OperatorType, Symbol, Token};

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
