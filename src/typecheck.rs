use expression::{BinaryExpr, Expr, ExprEnum, FunctionCall, LiteralExpr, UnaryExpr, Operator};
use native::get_native_types;
use statement::{Assignment, Declaration, FunctionDecl, IfStatement, Statement, StatementResult,
                TypedVar, WhileStatement};
use std::collections::HashMap;
use token::TokenType;

/// Represents a scope with its variables.
#[derive(Debug)]
struct Scope {
    variables: HashMap<String, TypedVar>,
    /// This represents the number of scopes that shares variables with this one.
    depth: usize,
}
impl Scope {
    /// Creates a new scope.
    pub fn new(depth: usize) -> Self {
        Scope {
            depth: depth,
            variables: HashMap::new(),
        }
    }
    /// Checks if the given variable exists in scope.
    pub fn has_var(&self, var_name: &str) -> bool {
        self.variables.contains_key(var_name).clone()
    }
    /// Returns the given variable.
    pub fn get_var(&self, var_name: &str) -> Option<TypedVar> {
        let res = self.variables.get(var_name);
        match res {
            Some(t) => Some(t.clone()),
            None => None,
        }
    }
    /// Creates a new variable with the given name.
    pub fn create_var(&mut self, var: TypedVar) {
        self.variables.insert(var.name().to_string(), var);
    }
}

/// The type checker
/// Contains a programm and functions to resolve types/verify consistency.
/// Also check for lvalues and assignment.
pub struct TypeChecker {
    native_functions: HashMap<String, (String, Vec<TypedVar>)>,
    functions: HashMap<String, FunctionDecl>,
    scopes: Vec<Scope>,
}

impl TypeChecker {
    /// Creates a new typechekcer object.
    pub fn new() -> Self {
        TypeChecker {
            scopes: vec![Scope::new(1)],
            functions: HashMap::new(),
            native_functions: HashMap::new(),
        }
    }
    /// Add a lib to the program.
    pub fn add_lib(&mut self, lib: &str) {
        for f in get_native_types(lib) {
            self.native_functions.insert(f.name(), (f.ret(), f.args()));
        }
    }
    /// Creates a variable in the current scope.
    pub fn create_var(&mut self, var: TypedVar) {
        self.scopes.last_mut().unwrap().create_var(var);
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn has_var(&self, var_name: &str) -> bool {
        let current_scope = self.scopes.last().unwrap().depth;
        let len = self.scopes.len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes[len - sc - 1].has_var(var_name) {
                found = true;
            }
        }
        found
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn get_var(&self, var_name: &str) -> Option<TypedVar> {
        let current_scope = self.scopes.last().unwrap().depth;
        let len = self.scopes.len();
        let mut found = false;
        for sc in 0..current_scope {
            if self.scopes[len - sc - 1].has_var(var_name) {
                found = true;
                return self.scopes[len - sc - 1].get_var(var_name);
            }
        }
        None
    }
    /// Checks if the type of the given expression matches with the given type.
    pub fn check_type(&self, expr: &Expr, expected: &String) -> Result<(), String> {
        if expr.return_type() != expected && expected != "any" {
            return Err(
                format!("Expected : {}, got : {}", expected, expr.return_type()).to_string(),
            );
        }
        Ok(())
    }

    /// Resolve types if possible
    /// The aim is to traverse the tree and resolve the return type of all expressions.
    pub fn resolve(&mut self, program: &mut HashMap<String, FunctionDecl>) -> Result<(), String> {
        self.add_lib("base");
        self.functions = program.clone();
        for (name, mut func) in program {
            self.function(&mut func)?;
        }
        Ok(())
    }

    /// Resolve/check types for a function declaration
    /// This does two things :
    ///     Checks statements inside.
    ///     Checks the return type
    pub fn function(&mut self, func: &mut FunctionDecl) -> Result<(), String> {
        let ret_type = func.ret_type().clone();
        let depth = self.scopes.len();
        self.scopes.push(Scope::new(depth));
        for arg in func.args() {
            self.create_var(arg.clone());
        }
        for st in func.scope_mut() {
            self.statement(st)?;
            if let &mut Statement::ReturnStatement(ref mut expr) = st {
                self.check_type(&expr, &ret_type)?;
            }
        }
        self.scopes.pop();
        Ok(())
    }
    /// Resolve what needs to be resolved in a statement.
    pub fn statement(&mut self, statement: &mut Statement) -> Result<(), String> {
        match statement {
            &mut Statement::Assignment(ref mut a) => self.assignment(a),
            &mut Statement::Declaration(ref mut d) => self.declaration(d),
            &mut Statement::ExprStatement(ref mut e) => self.expression(e),
            &mut Statement::Scope(ref mut s) => self.scope(s),
            &mut Statement::IfStatement(ref mut i) => self.if_statement(i),
            &mut Statement::WhileStatement(ref mut i) => self.while_statement(i),
            &mut Statement::BreakStatement => Ok(()),
            &mut Statement::ReturnStatement(ref mut e) => self.expression(e),
            ref a => {
                Err(format!("other statements are not supported for now : {:?}", a).to_string())
            }
        }
    }

    pub fn if_statement(&mut self, if_statement: &mut IfStatement) -> Result<(), String> {
        self.expression(if_statement.condition_mut())?;
        self.statement(if_statement.statement_mut())?;
        Ok(())
    }

    pub fn while_statement(&mut self, while_statement: &mut WhileStatement) -> Result<(), String> {
        self.expression(while_statement.condition_mut())?;
        self.statement(while_statement.statement_mut())?;
        Ok(())
    }

    /// Checks a scope
    /// checks every statement in the scope (easy this one).
    pub fn scope(&mut self, scope: &mut Vec<Statement>) -> Result<(), String> {
        let depth = self.scopes.len();
        self.scopes.push(Scope::new(depth));
        for st in scope {
            self.statement(st)?;
        }
        self.scopes.pop();
        Ok(())
    }

    /// Parses a declaration.
    /// Resolve the expression's types.
    /// checks that types match.
    /// creates a new variable with the given type in the scope.
    pub fn declaration(&mut self, decl: &mut Declaration) -> Result<(), String> {
        let val_type = decl.val_type().to_string();
        self.expression(decl.expr_mut())?;
        self.check_type(decl.expr(), &val_type)?;
        self.create_var(TypedVar::new(val_type, decl.name().to_string()));
        Ok(())
    }

    /// Checks the assignment :
    /// First checks the expression assigned.
    /// then what it is assigned to (must be a lvalue).
    /// then if they match.
    pub fn assignment(&mut self, assignment: &mut Assignment) -> Result<(), String> {
        self.expression(assignment.expr_mut())?;
        self.expression(assignment.assignee_mut())?;
        if !self.is_assignee(assignment.assignee()) {
            return Err(format!("can only assign to pointer or local variables "));
        }
        self.check_type(assignment.expr(), assignment.assignee().return_type())?;
        Ok(())
    }

    pub fn is_assignee(&self, expression: &Expr) -> bool {
        expression.is_identifier() || self.is_pointer(expression)
    }

    /// Sets the expression's return type.
    /// Sets the type of incoming and outcoming variables so the compiler will know what it needs to.
    pub fn expression(&mut self, expr: &mut Expr) -> Result<(), String> {
        let current_type = expr.return_type().to_string();
        let tp = match expr.expr_mut() {
            &mut ExprEnum::Literal(_) => Ok(current_type),
            &mut ExprEnum::Unary(ref mut u) => self.unary(u),
            &mut ExprEnum::Binary(ref mut b) => self.binary(b),
            &mut ExprEnum::Identifier(ref mut i) => self.identifier(i),
            &mut ExprEnum::FunctionCall(ref mut f) => self.function_call(f),
            _ => Err("fail".to_string()),
        }?;
        expr.set_type(tp);
        Ok(())
    }

    /// Returns the type of the given identifier if it exists in scope.
    pub fn identifier(&mut self, id: &String) -> Result<String, String> {
        match self.has_var(id) {
            true => Ok(self.get_var(id).unwrap().type_var().to_string()),
            false => Err(String::from(format!("Unknown variable : {}", id))),
        }
    }

    pub fn get_function(&self, name: &str) -> Result<(String, &Vec<TypedVar>), String> {
        match self.functions.get(name) {
            Some(f) => Ok((f.ret_type().to_string(), &f.args())),
            None => match self.native_functions.get(name) {
                Some((ret, f)) => Ok((ret.clone(), f)),
                None => Err(String::from(format!("Unknown function : {:?}", name))),
            },
        }
    }

    /// Find the return type of a function call expression and returns it.
    /// Checks that arguments lists are the same size.
    /// Checks for arguments given to the function.
    /// And well... This is a bit embarassing...
    /// The ugly hack with the closure feels a little bit odd.
    pub fn function_call(&mut self, exp: &mut FunctionCall) -> Result<String, String> {
        let (ret, args) = {
            let (r, a) = self.get_function(exp.name())?;
            (r.clone(), a.clone())
        };
        if let Some(t) = args.first() {
            if t.type_var() == "any" {
                return Ok(ret.to_string());
            }
        }
        let (args_count_given, args_count_expected) = (exp.args().len(), args.len());
        if args_count_expected != args_count_given {
            return Err(String::from(format!(
                "Error : expected {} arguments, {} given",
                args_count_expected, args_count_given
            )));
        }
        for i in 0..args_count_given {
            self.expression(&mut exp.args_mut()[i])?;
            self.check_type(&exp.args_mut()[i], args[i].type_var())?;
        }
        Ok(ret.to_string())
    }

    /// Find the return type of a unary expression and returns it.
    pub fn unary(&mut self, exp: &mut UnaryExpr) -> Result<String, String> {
        self.expression(exp.expression_mut())?;
        let exp_res = exp.expression().return_type();
        match exp.operator() {
            Operator::MINUS => match exp_res.as_ref() {
                "num" => Ok(String::from("num")),
                "str" => Err(String::from("Cant apply operator '-' to String")),
                _ => Err(String::from("Operator '-' supported only for primitives")),
            },
            Operator::Not => match exp_res.as_ref() {
                "num" => Ok(String::from("num")),
                "str" => Ok(String::from("str")),
                _ => Err(String::from("Operator '!' supported only for primitives")),
            },
            e => Err(format!("operator {:?} can not be aplied to one value", e)),
        }
    }

    /// Find the return type of a binary expression and returns it.
    pub fn binary(&mut self, exp: &mut BinaryExpr) -> Result<String, String> {
        self.expression(exp.lhs_mut())?;
        self.expression(exp.rhs_mut())?;
        let exp_res = (
            exp.lhs().return_type().as_ref(),
            exp.rhs().return_type().as_ref(),
        );
        match exp.operator() {
            Operator::MINUS => match exp_res {
                ("num", "num") => Ok(String::from("num")),
                ("str", _) => Err(String::from("Cant apply operator '-' to String")),
                (_, "str") => Err(String::from("Cant apply operator '-' to String")),
                _ => Err(String::from("Operator '-' supported only for primitives")),
            },
            Operator::PLUS => match exp_res {
                ("num", "num") => Ok(String::from("num")),
                ("str", _) => Ok(String::from("str")),
                (_, "str") => Err(String::from("str")),
                _ => Err(String::from("Operator '+' supported only for primitives")),
            },
            Operator::STAR => match exp_res {
                ("num", "num") => Ok(String::from("num")),
                ("str", "num") => Ok(String::from("str")),
                ("num", "str") => Ok(String::from("str")),
                ("str", "str") => Err(String::from("Cannot multiply String by String")),
                _ => Err(String::from("Operator '*' supported only for primitives")),
            },
            Operator::SLASH => match exp_res {
                ("num", "num") => Ok(String::from("num")),
                ("str", _) => Err(String::from("Cant apply operator '/' to String")),
                (_, "str") => Err(String::from("Cant apply operator '/' to String")),
                _ => Err(String::from("Operator '/' supported only for primitives")),
            },
            Operator::GreaterEqual => Ok(String::from("num")),
            Operator::GREATER => Ok(String::from("num")),
            Operator::LessEqual => Ok(String::from("num")),
            Operator::LESS => Ok(String::from("num")),
            Operator::EqualEqual => Ok(String::from("num")),
            Operator::NotEqual => Ok(String::from("num")),
            Operator::AndAnd => Ok(String::from("num")),
            Operator::INDEX => {
                println!("exp res is : {:?}", exp_res);
                if exp_res == ("slice", "num"){
                    Ok(String::from("num"))
                } else if exp_res.0 == "slice"{
                    Err(format!("can not use {} to index slice", exp_res.1))
                } else {
                    Err(format!("can not index {}", exp_res.0))
                }
            }
            e => Err(format!("operator {:?} can not be aplied to two value", e)),
        }
    }

    /// Checks if the type is a pointer.
    /// This can happen if :
    /// The type is a number/char from a slice (indexed)
    /// The type is a number/char from an object (with . operator) // not yet implemented.
    pub fn is_pointer(&self, expr : &Expr) -> bool {
        if expr.return_type() == "num"{
            if let ExprEnum::Binary(BinaryExpr{operator: Operator::INDEX, ..}) = expr.expr(){
                return true;
            } else {
                return false;
            }
        }
        return true;
    }
}
