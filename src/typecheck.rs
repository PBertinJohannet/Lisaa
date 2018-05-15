use expression::{Expr, UnaryExpr, LiteralExpr, BinaryExpr, FunctionCall, ExprEnum};
use token::TokenType;
use operations::{BinaryOperations, UnaryOperations};
use statement::{Statement, Assignment, IfStatement, StatementResult, FunctionDecl, WhileStatement, Declaration, TypedVar};
use std::collections::HashMap;
use native::get_native_types;

/// Represents a scope with its variables.
#[derive(Debug)]
struct Scope {
    variables : HashMap<String, TypedVar>,
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
        self.variables.contains_key(var_name).clone()
    }
    /// Returns the given variable.
    pub fn get_var(&self, var_name : &str) -> Option<TypedVar> {
        let res = self.variables.get(var_name);
        match res {
            Some(t) => Some(t.clone()),
            None => None,
        }
    }
    /// Creates a new variable with the given name.
    pub fn create_var(&mut self, var : TypedVar) {
        self.variables.insert(var.name().to_string(), var);
    }
}


/// The type checker
/// Contains a programm and functions to resolve types/verify consistency.
pub struct TypeChecker {
    native_functions : HashMap<String, Vec<TypedVar>> ,
    functions : HashMap<String, FunctionDecl>,
    scopes : Vec<Scope>,
}

impl TypeChecker {
    /// Creates a new typechekcer object.
    pub fn new() -> Self{
        TypeChecker{
            scopes : vec![Scope::new(1)],
            functions : HashMap::new(),
            native_functions : HashMap::new(),
        }
    }
    /// Add a lib to the program.
    pub fn add_lib(&mut self, lib : &str) {
        for f in get_native_types(lib){
            self.native_functions.insert(f.name(), f.args());
        }
    }
    /// Creates a variable in the current scope.
    pub fn create_var(&mut self, var : TypedVar){
        self.scopes.last_mut().unwrap().create_var(var);
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
    pub fn get_var(&self, var_name : &str) -> Option<TypedVar>{
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
    /// Checks if the type of the given expression matches with the given type.
    pub fn check_type(&self, expr : &Expr, expected : &String) -> Result<(), String>{
        if expr.return_type() != expected{
            return Err(format!("Expected : {}, got : {}", expected, expr.return_type()).to_string());
        }
        Ok(())
    }

    /// Resolve types if possible
    /// The aim is to traverse the tree and resolve the return type of all expressions.
    pub fn resolve(&mut self, program : &mut HashMap<String, FunctionDecl>) -> Result<(), String>{
        self.add_lib("base");
        self.functions = program.clone();
        for (name, mut func) in program{
            self.function(&mut func)?;
        }
        Ok(())
    }

    /// Resolve/check types for a function declaration
    /// This does two things :
    ///     Checks statements inside.
    ///     Checks the return type
    pub fn function(&mut self, func : &mut FunctionDecl) -> Result<(), String>{
        let ret_type = func.ret_type().clone();
        for st in func.scope_mut(){
            self.statement(st)?;
            if let &mut Statement::ReturnStatement(ref mut expr) = st {
                self.check_type(&expr, &ret_type)?;
            }
        }
        Ok(())
    }
    /// Resolve what needs to be resolved in a statement.
    pub fn statement(&mut self, statement: &mut Statement) -> Result<(), String>{
        match statement {
        //    &mut Statement::Assignment(ref mut a) => self.assignment(a),
            &mut Statement::Declaration(ref mut  d) => self.declaration(d),
        //    &mut Statement::ExprStatement(ref mut e) => self.expression(e),
        //    &mut Statement::Scope(ref mut  s) => self.scope(s),
        //    &mut Statement::IfStatement(ref mut i) => self.if_statement(i),
        //    &mut Statement::WhileStatement(ref mut  i) => self.while_statement(i),
        //    &mut Statement::BreakStatement => Ok(()),
        //    &mut Statement::ReturnStatement(ref mut e) =>self.expression(e),
            ref a => Err(format!("other statements are not supported for now : {:?}", a).to_string()),
        }
    }

    /// Parses a declaration.
    /// Resolve the expression's types.
    /// checks that types match.
    /// creates a new variable with the given type in the scope.
    pub fn declaration(&mut self, decl : &mut Declaration) ->Result<(), String>{
        let val_type = decl.val_type().to_string();
        self.expression(decl.expr_mut())?;
        self.check_type(decl.expr(), &val_type)?;
        self.create_var(TypedVar::new(val_type, decl.name().to_string()));
        Ok(())
    }

    /// Sets the expression's return type.
    /// Sets the type of incoming and outcoming variables so the compiler will know what it needs to.
    pub fn expression(&mut self, expr : &mut Expr) -> Result<(), String>{
        let current_type = expr.return_type().to_string();
        let tp = match expr.expr_mut() {
            &mut ExprEnum::Literal(_) => Ok(current_type),
            &mut ExprEnum::Unary(ref mut u) => self.unary(u),
//            &mut ExprEnum::Binary(ref mut b) => self.binary(b),
//            &mut ExprEnum::Identifier(ref mut i) => self.identifier(i),
 //           &mut ExprEnum::FunctionCall(ref mut f) => self.function_call(f),
            _ => Err("fail".to_string()),
        }?;
        expr.set_type(tp);
        Ok(())
    }

    /// Find the return type of a unary expression and returns it.
    pub fn unary(&mut self, exp : &mut UnaryExpr) -> Result<String, String>{
        self.expression(exp.expression_mut())?;
        let exp_res = exp.expression().return_type();
        match exp.operator().get_type() {
            &TokenType::MINUS => match exp_res.as_ref() {
                "num" => Ok(String::from("num")),
                "string" => Err(String::from("Cant apply operator '-' to String")),
                _ => Err(String::from("Operator '-' supported only for primitives")),
            },
            &TokenType::BANG => match exp_res.as_ref() {
                "num" => Ok(String::from("num")),
                "str" => Ok(String::from("str")),
                _ => Err(String::from("Operator '!' supported only for primitives")),
            },
            e => Err(format!("operator {:?} can not be aplied to one value", e))
        }
    }
}

// so we traverse the tree and returns an option<string>, if Some(s) sets the type to S. (we will see).

/*
    /// Evaluates the given expression.
    /// Returns a litteral if possible,
    /// if any fail occurs, returns an error.
    pub fn evaluate(&self, expr : &Expr) -> Result<LiteralExpr, String>{
        match expr.expr() {
            &ExprEnum::Literal(ref l) => Ok(l.clone()),
            &ExprEnum::Unary(ref u) => self.unary(u),
            &ExprEnum::Binary(ref b) => self.binary(b),
            &ExprEnum::Identifier(ref i) => self.identifier(i),
            &ExprEnum::FunctionCall(ref f) => self.function_call(f),
        }
    }

    /// Calls a function.
    pub fn function_call(&self, func : &FunctionCall) -> Result<LiteralExpr, String>{
        if self.native_functions.contains_key(func.name()){
            self.native_func(func)
        } else {
            self.user_defined_func(func.name(), func.args().iter().map(|a|
                self.evaluate(a)).collect())
        }
    }

    pub fn native_func(&self, func : &FunctionCall) -> Result<LiteralExpr, String>{
        let mut args = vec![];
        for i in func.args() {
            args.push(self.evaluate(i)?);
        }
        self.native_functions.get(func.name()).unwrap()(args)
    }

    /// Calls a function created by the user.
    pub fn user_defined_func(&self, name : &String, args_given : Vec<Result<LiteralExpr, String>>) -> Result<LiteralExpr, String>{
        let depth = self.scopes.borrow().last().unwrap().depth;
        self.scopes.borrow_mut().push(Scope::new(1));
        let actual_function = self.functions.get(name)
            .ok_or("could not find function in scope".to_string())?;
        let actual_args = actual_function.args();
        for i in 0..args_given.len() {
            self.create_var(&actual_args[i].name(), args_given[i].clone()?);
        }
        for s in actual_function.scope() {
            let res = self.run_statement(s)?;
            match res {
                StatementResult::Return(l) => { self.scopes.borrow_mut().pop(); return Ok(l)},
                StatementResult::Break => Err("can't break a function like that you silly".to_string()),
                _ => Ok(())
            }?;
        }
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
            &TokenType::EqualEqual => exp_left.equals(&exp_right),
            e => Err(format!("operator {:?} can not be applied to binbary expression", e))
        }
    }


    /// Parses a declaration.
    pub fn declaration(&self, decl : &Declaration) ->Result<StatementResult, String>{
        let name = decl.name();
        let res = self.evaluate(decl.expr())?;
        self.create_var(name, res);
        Ok(StatementResult::Empty)
    }
    /// Interprets an if statement.
    pub fn if_statement(&self, statement : &IfStatement) -> Result<StatementResult, String> {
        let res = self.evaluate(statement.condition())?;
        if self.is_true(&res) {
            let res = self.run_statement(statement.statement())?;
            if res.is_quit(){
                return Ok(res);
            }
        }
        Ok(StatementResult::Empty)
    }
    /// Interprets an if statement.
    pub fn while_statement(&self, statement : &WhileStatement) -> Result<StatementResult, String> {
        let mut res = self.evaluate(statement.condition())?;
        while self.is_true(&res) {
            self.run_statement(statement.statement())?;
            let r = self.run_statement(statement.statement())?;
            if r.is_quit(){
                return Ok(r);
            }
            res = self.evaluate(statement.condition())?;
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
        let var_name = assignment.identifier().to_string();
        self.set_var(&var_name, res);
        Ok(StatementResult::Empty)
    }

*/