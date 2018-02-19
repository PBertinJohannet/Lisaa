//! The interpreter for the language
use expression::{Expr, UnaryExpr, LiteralExpr, BinaryExpr};
use token::TokenType;
use operations::{BinaryOperations, UnaryOperations};
use statement::{Statement, Assignment};
use std::collections::HashMap;

/// The interpreter's struct, will interpret the expressions.
pub struct Interpreter {
    variables : HashMap<String, LiteralExpr>
}

impl Interpreter {
    /// Creates a new interpreter.
    pub fn new() -> Self {
        Interpreter {
            variables : HashMap::new()
        }
    }
    /// Outputs the state as a string.
    pub fn state(&self) -> String {
        format!("vars : {:?}", self.variables)
    }
    /// Runs the given statement, returns an error if it failed.
    pub fn run(&mut self, statement : &Statement) -> Result<(), String>{
        match statement {
            &Statement::Assignment(ref a) => self.assignment(&a),
            &Statement::ExprStatement(ref e) => {self.evaluate(&e); Ok(())},
            _ => Err("declarations are not supported for now".to_string()),
        }
    }
    /// Runs an assignment.
    pub fn assignment(&mut self, assignment : &Assignment) -> Result<(), String>{
        let mut res = self.evaluate(assignment.expr())?;
        let var_name = assignment.identifier().get_lexeme().to_string();
        if self.variables.get(&var_name).is_none(){
            self.variables.insert(var_name, res);
        } else {
            let mut a = self.variables.get_mut(&var_name);
            a = Some(&mut res);
        }
        Ok(())
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