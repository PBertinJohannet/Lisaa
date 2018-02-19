//! The module for all the operations between literals.
//!
use expression::LiteralExpr;

/// The trait for unary operations.
pub trait UnaryOperations {
    /// Applys the negate operator to the literal.
    fn minus(&self) -> Result<LiteralExpr, String>;
    /// Apply the not operator to the literal.
    fn bang(&self) -> Result<LiteralExpr, String>;
}

impl UnaryOperations for LiteralExpr {
    fn bang(&self) -> Result<LiteralExpr, String>{
        match self{
            &LiteralExpr::NUMBER(n) => Ok(LiteralExpr::NUMBER((n == 0.0) as i32 as f64)),
            &LiteralExpr::STRING(ref s) => Ok(LiteralExpr::NUMBER((s.len() == 0) as i32 as f64)),
        }
    }

    fn minus(&self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) => Ok(LiteralExpr::NUMBER(-n)),
            &LiteralExpr::STRING(_) => Err("Cant apply negative operation - to string values".to_string()),
        }
    }
}
