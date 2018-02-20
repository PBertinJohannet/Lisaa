//! The interpreter for the language
use expression::{Expr, UnaryExpr, LiteralExpr, BinaryExpr};
use token::TokenType;
use operations::{BinaryOperations, UnaryOperations};
use statement::{Statement, Assignment};
use std::collections::HashMap;
use std::rc::Rc;

/// Represents a scope with its variables.
#[derive(Debug)]
pub struct Scope {
    variables : HashMap<String, LiteralExpr>,
    /// This represents the number of scopes that shares variables with this one.
    depth : usize,
}
impl Scope {
    /// Creates a new scope.
    pub fn new(depth : usize) -> Self {
        Scope {
            depth : depth,
            variables : HashMap::new(),
        }
    }
    /// Checks if the given variable exists in scope.
    pub fn has_var(&self, var_name : &str) -> bool {
        self.variables.contains_key(var_name)
    }
    /// Returns the given variable.
    pub fn get_var(&self, var_name : &str) -> Option<&LiteralExpr> {
        self.variables.get(var_name)
    }
    /// Sets the value of the given variable to the required value.
    /// If the variable is not existing it crashes.
    pub fn set_var(&mut self, var_name : &str, val : LiteralExpr) {
        *self.variables.get_mut(var_name).unwrap() = val;
    }
    /// Creates a new variable with the given name.
    pub fn create_var(&mut self, var_name : &str, val : LiteralExpr) {
        self.variables.insert(var_name.to_string(), val);
    }
}


/// The interpreter's struct, will interpret the expressions.
/// contains the current scope.
pub struct Interpreter {
    scopes : Vec<Scope>,
}

impl Interpreter {
    /// Creates a new interpreter with a parent or not.
    pub fn new(parent : Option<Rc<Interpreter>>) -> Self{
        Interpreter{
            scopes : vec![Scope::new(1)],
        }
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn set_var(&mut self, var_name : &str, result : LiteralExpr){
        let current_scope = self.scopes.last().unwrap().depth;
        let len = self.scopes.len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes[len-sc-1].has_var(var_name){
                found = true;
                self.scopes[len-sc-1].set_var(var_name, result);
                return;
            }
        }
        if !found {
            self.scopes.last_mut().unwrap().create_var(var_name, result);
        }
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn has_var(&self, var_name : &str) -> bool{
        let current_scope = self.scopes.last().unwrap().depth;
        let len = self.scopes.len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes[len-sc-1].has_var(var_name){
                found = true;
            }
        }
        found
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn get_var(&self, var_name : &str) -> Option<&LiteralExpr>{
        let current_scope = self.scopes.last().unwrap().depth;
        let len = self.scopes.len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes[len-sc-1].has_var(var_name){
                found = true;
                return self.scopes[len-sc-1].get_var(var_name);
            }
        }
        None
    }
    /// Outputs the state as a string.
    pub fn state(&self) -> String {
        format!("vars : {:?}", self.scopes)
    }
    /// Runs the given statement, returns an error if it failed.
    pub fn run(&mut self, statement : &Statement) -> Result<(), String>{
        match statement {
            &Statement::Assignment(ref a) => self.assignment(&a),
            &Statement::ExprStatement(ref e) => {self.evaluate(&e); Ok(())},
            &Statement::Scope(ref s) => self.scope(s),
            _ => Err("declarations are not supported for now".to_string()),
        }
    }
    /// Interprets the new scope.
    pub fn scope(&mut self, scope : &Vec<Statement>) -> Result<(), String>{
        let depth = self.scopes.last().unwrap().depth;
        self.scopes.push(Scope::new(depth+1));
        for s in scope {
            self.run(s)?;
        }
        self.scopes.pop();
        Ok(())
    }

    /// Runs an assignment.
    pub fn assignment(&mut self, assignment : &Assignment) -> Result<(), String>{
        let mut res = self.evaluate(assignment.expr())?;
        let var_name = assignment.identifier().get_lexeme().to_string();
        self.set_var(&var_name, res);
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
            &Expr::Identifier(ref i) => self.identifier(i),
        }
    }

    /// Evaluates an identifier.
    pub fn identifier(&self, i : &str) -> Result<LiteralExpr, String>{
        match self.get_var(i){
            Some(v) => Ok(v.clone()),
            _ => Err(format!("use of uninitialised variable : {}", i)),
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