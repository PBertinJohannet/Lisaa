//! The interpreter for the language
use expression::{Expr, UnaryExpr, LiteralExpr, BinaryExpr};
use token::TokenType;
use operations::{BinaryOperations, UnaryOperations};

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
            &TokenType::MINUS => UnaryOperations::minus(&exp_res),
            &TokenType::BANG => exp_res.bang(),
            e => Err(format!("operator {:?} can not be aplied to one value", e))
        }
    }
    /// Evaluates a binary expression.
    pub fn binary(&self, exp : &BinaryExpr) -> Result<LiteralExpr, String>{
        let exp_left = self.evaluate(exp.lhs())?;
        let exp_right = self.evaluate(exp.rhs())?;
        match exp.operator().get_type() {
            &TokenType::MINUS => BinaryOperations::minus(&exp_left,&exp_right),
            &TokenType::PLUS => exp_left.plus(&exp_right),
            &TokenType::STAR => exp_left.times(&exp_right),
            &TokenType::SLASH => exp_left.divide(&exp_right),
            &TokenType::GreaterEqual => exp_left.ge(&exp_right),
            &TokenType::GREATER => exp_left.gt(&exp_right),
            &TokenType::LessEqual => exp_left.le(&exp_right),
            &TokenType::LESS => exp_left.lt(&exp_right),
            e => Err(format!("operator {:?} can not be applied to binbary expression", e))
        }
    }
}