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

/// The trait for binary operations, lets go for a lot of matches.
/// No implicit conversions atm.
pub trait BinaryOperations {
    /// Substract other from self.
    fn minus(&self, other : &Self) -> Result<LiteralExpr, String>;
    /// Add other to self
    fn plus(&self, other : &Self) -> Result<LiteralExpr, String>;
    /// Multiply other and self
    fn times(&self, other : &Self) -> Result<LiteralExpr, String>;
    /// Divide self by other.
    fn divide(&self, other : &Self) -> Result<LiteralExpr, String>;
    /// Returns 1 or 0 if other is lower
    fn ge(&self, other : &Self) -> Result<LiteralExpr, String>;
    /// Returns 1 or 0 if other is lower or equal
    fn gt(&self, other : &Self) -> Result<LiteralExpr, String>;
    /// Returns 1 or 0 if other is greater
    fn le(&self, other : &Self) -> Result<LiteralExpr, String>;
    /// Returns 1 or 0 if other is greater or equal.
    fn lt(&self, other : &Self) -> Result<LiteralExpr, String>;
}


impl BinaryOperations for LiteralExpr {
    fn minus(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER(n - k)),
                _ => Err("Cannot substract string fom int".to_string()),
            }
            _ => Err("Cannot substract fom string".to_string()),
        }
    }

    fn plus(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER(n + k)),
                _ => Err("Cannot add string to int".to_string()),
            }
            _ => Err("Cannot add to string".to_string()),
        }
    }

    fn times(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER(n + k)),
                _ => Err("Cannot multiply string with int".to_string()),
            }
            _ => Err("Cannot multiply with string".to_string()),
        }
    }

    fn divide(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER(n / k)),
                _ => Err("Cannot divide string with int".to_string()),
            }
            _ => Err("Cannot divide with string".to_string()),
        }
    }

    fn ge(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER((n >= k) as i32 as f64)),
                _ => Err("Cannot compare string with int".to_string()),
            }
            _ => Err("Cannot compare with string".to_string()),
        }
    }

    fn gt(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER((n > k) as i32 as f64)),
                _ => Err("Cannot compare string with int".to_string()),
            }
            _ => Err("Cannot compare with string".to_string()),
        }
    }

    fn le(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER((n <= k) as i32 as f64)),
                _ => Err("Cannot compare string with int".to_string()),
            }
            _ => Err("Cannot compare with string".to_string()),
        }
    }

    fn lt(&self, other: &Self) -> Result<LiteralExpr, String> {
        match self{
            &LiteralExpr::NUMBER(n) =>  match other {
                &LiteralExpr::NUMBER(k) => Ok(LiteralExpr::NUMBER((n < k) as i32 as f64)),
                _ => Err("Cannot compare string with int".to_string()),
            }
            _ => Err("Cannot compare with string".to_string()),
        }
    }
}