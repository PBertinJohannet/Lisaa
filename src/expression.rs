//! The module containing the code for the different expressions.
//!

use token::{Token, TokenType};

#[derive(Debug)]
/// The base for an expression.
pub enum Expr{
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Literal(LiteralExpr),
}

pub type Operator = Token;
#[derive(Debug)]
/// An unary expression contains only an operator and the expression.
pub struct UnaryExpr {
    operator : Operator,
    expr : Box<Expr>,
}

impl UnaryExpr {
    /// Creates a new unary expression.
    pub fn new(operator : Operator, expr : Expr ) -> Self {
        UnaryExpr {
            operator : operator,
            expr : Box::new(expr),
        }
    }
}
#[derive(Debug)]
/// An unary expression contains an operator and two expressions.
pub struct BinaryExpr {
    lhs : Box<Expr>,
    operator : Operator,
    rhs : Box<Expr>,
}
impl BinaryExpr {
    /// Creates a new binary expression.
    pub fn new(lhs : Expr, operator : Operator, rhs : Expr ) -> Self {
        BinaryExpr {
            lhs : Box::new(lhs),
            operator : operator,
            rhs : Box::new(rhs),
        }
    }
}

#[derive(Debug)]
/// An literal expression is a string/number literal.
pub enum LiteralExpr {
    NUMBER(f64),
    STRING(String),
}
