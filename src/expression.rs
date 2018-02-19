//! The module containing the code for the different expressions.
//!

use token::Token;
use std::fmt;

#[derive(Debug)]
/// The base for an expression.
pub enum Expr {
    /// The unary expressions, see below.
    Unary(UnaryExpr),
    /// The binary expressions, see below.
    Binary(BinaryExpr),
    /// The literals, see below.
    Literal(LiteralExpr),
}

/// Operators are represented by tokens for now.
pub type Operator = Token;

#[derive(Debug)]
/// An unary expression contains only an operator and the expression.
pub struct UnaryExpr {
    operator: Operator,
    expr: Box<Expr>,
}

impl UnaryExpr {
    /// Creates a new unary expression.
    pub fn new(operator: Operator, expr: Expr) -> Self {
        UnaryExpr {
            operator: operator,
            expr: Box::new(expr),
        }
    }
    /// Returns the expression.
    pub fn expression(&self) -> &Expr {
        &*self.expr
    }
    /// Returns the operator.
    pub fn operator(&self) -> Operator {
        self.operator.clone()
    }
}
#[derive(Debug)]
/// An unary expression contains an operator and two expressions.
pub struct BinaryExpr {
    lhs: Box<Expr>,
    operator: Operator,
    rhs: Box<Expr>,
}
impl BinaryExpr {
    /// Creates a new binary expression.
    pub fn new(lhs: Expr, operator: Operator, rhs: Expr) -> Self {
        BinaryExpr {
            lhs: Box::new(lhs),
            operator: operator,
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug, Clone)]
/// An literal expression is a string/number literal.
pub enum LiteralExpr {
    /// Anything from bool to null will be a number
    NUMBER(f64),
    /// Anything else will be a string.
    STRING(String),
}


impl fmt::Display for LiteralExpr {
    /// Formats the error to a user readable format.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LiteralExpr::NUMBER(n) => write!(f, "{}", n),
            &LiteralExpr::STRING(ref s) => write!(f, "{}", s),
        }
    }
}