//! The interpreter for the language
use expression::{Expr, UnaryExpr, LiteralExpr, BinaryExpr, FunctionCall};
use token::TokenType;
use operations::{BinaryOperations, UnaryOperations};
use statement::{Statement, Assignment, IfStatement, StatementResult, FunctionDecl};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

/// Represents a scope with its variables.
#[derive(Debug)]
pub struct Scope {
    variables : Rc<RefCell<HashMap<String, LiteralExpr>>>,
    /// This represents the number of scopes that shares variables with this one.
    depth : usize,
}
impl Scope {
    /// Creates a new scope.
    pub fn new(depth : usize) -> Self {
        Scope {
            depth : depth,
            variables : Rc::new(RefCell::new(HashMap::new())),
        }
    }
    /// Checks if the given variable exists in scope.
    pub fn has_var(&self, var_name : &str) -> bool {
        let vars = self.variables.borrow();
        vars.contains_key(var_name).clone()
    }
    /// Returns the given variable.
    pub fn get_var(&self, var_name : &str) -> Option<LiteralExpr> {
        let vars = self.variables.borrow();
        let res = vars.get(var_name);
        match res {
            Some(t) => Some(t.clone()),
            None => None,
        }
    }
    /// Sets the value of the given variable to the required value.
    /// If the variable is not existing it crashes.
    pub fn set_var(&self, var_name : &str, val : LiteralExpr) {
        let mut vars = self.variables.borrow_mut();
        *vars.get_mut(var_name).unwrap() = val;
    }
    /// Creates a new variable with the given name.
    pub fn create_var(&self, var_name : &str, val : LiteralExpr) {
        let mut vars = self.variables.borrow_mut();
        vars.insert(var_name.to_string(), val);
    }
}


/// The interpreter's struct, will interpret the expressions.
/// contains the current scope.
pub struct Interpreter {
    scopes : RefCell<Vec<Scope>>,
    functions : HashMap<String, FunctionDecl>,
}

impl Interpreter {
    /// Creates a new interpreter with a parent or not.
    pub fn new(parent : Option<Rc<Interpreter>>) -> Self{
        Interpreter{
            scopes : RefCell::new(vec![Scope::new(1)]),
            functions : HashMap::new(),
        }
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn set_var(&self, var_name : &str, result : LiteralExpr){
        let current_scope = self.scopes.borrow().last().unwrap().depth;
        let len = self.scopes.borrow().len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes.borrow()[len-sc-1].has_var(var_name){
                found = true;
                self.scopes.borrow()[len-sc-1].set_var(var_name, result);
                return;
            }
        }
        if !found {
            self.scopes.borrow().last().unwrap().create_var(var_name, result);
        }
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn has_var(&self, var_name : &str) -> bool{
        let current_scope = self.scopes.borrow().last().unwrap().depth;
        let len = self.scopes.borrow().len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes.borrow()[len-sc-1].has_var(var_name){
                found = true;
            }
        }
        found
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn get_var(&self, var_name : &str) -> Option<LiteralExpr>{
        let current_scope = self.scopes.borrow().last().unwrap().depth;
        let len = self.scopes.borrow().len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes.borrow()[len-sc-1].has_var(var_name){
                found = true;
                return self.scopes.borrow()[len-sc-1].get_var(var_name);
            }
        }
        None
    }
    /// Outputs the state as a string.
    pub fn state(&self) -> String {
        format!("vars : {:?}", self.scopes.borrow())
    }
    /// Runs the programm, starting from the main function
    pub fn run(&mut self, programm : HashMap<String, FunctionDecl>) -> Result<StatementResult, String>{
        self.functions = programm;
        match self.functions.get("main"){
            Some(f) => self.function(f),
            None => Err("Error, no main function found".to_string()),
        }
    }
    pub fn function(&self, func : &FunctionDecl)-> Result<StatementResult, String>{
        for st in func.scope(){
            self.run_statement(st)?;
        }
        Ok(StatementResult::Empty)
    }

    /// Runs the given statement, returns an error if it failed.
    pub fn run_statement(&self, statement : &Statement) -> Result<StatementResult, String>{
        match statement {
            &Statement::Assignment(ref a) => self.assignment(&a),
            &Statement::ExprStatement(ref e) => {self.evaluate(&e); Ok(StatementResult::Empty)},
            &Statement::Scope(ref s) => self.scope(s),
            &Statement::IfStatement(ref i) => self.if_statement(i),
            &Statement::BreakStatement => Ok(StatementResult::Break),
            &Statement::ReturnStatement(ref i) => Ok(StatementResult::Return(self.evaluate(i)?)),
            ref a => Err(format!("other statements are not supported for now : {:?}", a).to_string()),
        }
    }
    /// Interprets an if statement.
    pub fn if_statement(&self, statement : &IfStatement) -> Result<StatementResult, String> {
        let res = self.evaluate(statement.condition())?;
        if self.is_true(&res) {
            self.run_statement(statement.statement())?;
        }
        Ok(StatementResult::Empty)
    }
    /// Interprets the new scope.
    pub fn scope(&self, scope : &Vec<Statement>) -> Result<StatementResult, String>{
        let depth = self.scopes.borrow().last().unwrap().depth;
        self.scopes.borrow_mut().push(Scope::new(depth+1));
        for s in scope {
            let res = self.run_statement(s)?;
            if res.is_quit(){
                println!("quit scope ");
                self.scopes.borrow_mut().pop();
                return Ok(res);
            }
        }
        self.scopes.borrow_mut().pop();
        Ok(StatementResult::Empty)
    }

    /// Runs an assignment.
    pub fn assignment(&self, assignment : &Assignment) -> Result<StatementResult, String>{
        let mut res = self.evaluate(assignment.expr())?;
        let var_name = assignment.identifier().get_lexeme().to_string();
        self.set_var(&var_name, res);
        Ok(StatementResult::Empty)
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
            &Expr::FunctionCall(ref f) => self.function_call(f),
        }
    }

    pub fn function_call(&self, func : &FunctionCall) -> Result<LiteralExpr, String>{
        let depth = self.scopes.borrow().last().unwrap().depth;
        self.scopes.borrow_mut().push(Scope::new(1));
        let args_given = func.args();
        let actual_function = self.functions.get(func.name())
            .ok_or("could not find function in scope".to_string())?;

        let actual_args = actual_function.args();
        for i in 0..args_given.len() {
            self.scopes.borrow_mut().last().unwrap().create_var(&actual_args[i], self.evaluate(&args_given[i])?);
        }
        for s in actual_function.scope() {
            let res = self.run_statement(s)?;
            match res {
                StatementResult::Return(l) => {println!("returning value"); self.scopes.borrow_mut().pop(); return Ok(l)},
                StatementResult::Break => Err("can't break a function like that you silly".to_string()),
                _ => Ok(())
            }?;
        }
        println!("returning nothing");
        self.scopes.borrow_mut().pop();
        Ok(LiteralExpr::NUMBER(0.0))
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
    /// Checks if the value is true
    pub fn is_true(&self, expr : &LiteralExpr) -> bool {
        match expr {
            &LiteralExpr::NUMBER(0.0) => false,
            _ => true,
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