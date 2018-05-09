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
    program : HashMap<String, FunctionDecl> ,
    scopes : Vec<Scope>,
}

impl TypeChecker {
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

    /// Resolve types if possible
    pub fn resolve(&mut self, program : HashMap<String, FunctionDecl>) -> Result<(), String>{
        self.add_lib("base");
        //self.program = program.clone();
        // typecheck the program
        for func in program{
            //self.function_decl(&func)?;
        }
        Ok(())
    }


}
