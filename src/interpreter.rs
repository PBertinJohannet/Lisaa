//! The interpreter for the language
use expression::{Expr, UnaryExpr, LiteralExpr, BinaryExpr};
use token::TokenType;
use operations::UnaryOperations;

/// The interpreter's struct, will interpret the expressions.
pub struct Interpreter {

}

impl Interpreter {
    /// Creates a new interpreter.
    pub fn new() -> Self {
        Interpreter {

        }
    }
    /// Evaluates the given expression.
    /// Returns a litteral if possible,
    /// if any fail occurs, returns an error.
    pub fn evaluate(&self, expr : &Expr) -> Result<LiteralExpr, String>{
        match expr {
            &Expr::Literal(ref l) => Ok(l.clone()),
            &Expr::Unary(ref u) => self.unary(u),
            &Expr::Binary(ref b) => self.binary(b),
        }
    }

    /// Evaluates a unary expression.
    pub fn unary(&self, exp : &UnaryExpr) -> Result<LiteralExpr, String>{
        let exp_res = self.evaluate(exp.expression())?;
        match exp.operator().get_type() {
            &TokenType::MINUS => exp_res.minus(),
            &TokenType::BANG => exp_res.bang(),
            e => Err(format!("operator {:?} can not be aplied to one value", e))
        }
    }
    /// Evaluates a binary expression.
    pub fn binary(&self, _exp : &BinaryExpr) -> Result<LiteralExpr, String>{
        unimplemented!();
        //let mut exp_res = self.evaluate(exp.expression());
    }
}