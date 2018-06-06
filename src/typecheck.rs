use expression::{BinaryExpr, Callee, Expr, ExprEnum, FunctionCall, Operator, UnaryExpr};
use native::get_native_types;
use statement::{Assignment, Declaration, FunctionDecl, IfStatement, Statement, WhileStatement};
use std::collections::HashMap;
use types::{LisaaType, TypedVar};

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
/// Contains a program and functions to resolve types/verify consistency.
/// Also check for lvalues and assignment.
pub struct TypeChecker {
    functions: HashMap<String, FunctionDecl>,
    scopes: Vec<Scope>,
}

impl TypeChecker {
    /// Creates a new typechekcer object.
    pub fn new() -> Self {
        TypeChecker {
            scopes: vec![Scope::new(1)],
            functions: HashMap::new(),
        }
    }
    /// Add a lib to the program.
    pub fn add_lib(&mut self, lib: &str) {
        for f in get_native_types(lib) {
            self.functions.insert(f.name().to_owned(), f);
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
        for sc in 0..current_scope {
            if self.scopes[len - sc - 1].has_var(var_name) {
                return self.scopes[len - sc - 1].get_var(var_name);
            }
        }
        None
    }
    /// Checks if the type of the given expression matches with the given type.
    pub fn check_type(&self, expr: &Expr, expected: &LisaaType) -> Result<(), String> {
        let (lhs, rhs) = (expr.return_type().max_deref(), expected.max_deref());
        if !lhs.0.is_equivalent(&rhs.0) {
            return Err(format!(
                "Expected : {}, got : {}",
                expected,
                expr.return_type()
            ));
        }
        Ok(())
    }

    /// Resolve types if possible
    /// The aim is to traverse the tree and resolve the return type of all expressions.
    pub fn resolve(&mut self, program: &mut HashMap<String, FunctionDecl>) -> Result<(), String> {
        self.functions = program.clone();
        self.add_lib("base");
        for (_, mut func) in program {
            self.function(&mut func)?;
        }
        Ok(())
    }

    /// Resolve/check types for a function declaration
    /// This does two things :
    ///     Checks statements inside.
    ///     Checks the return type
    pub fn function(&mut self, func: &mut FunctionDecl) -> Result<(), String> {
        let (ret_type, name) = (func.ret_type().clone(), func.name().to_string());
        let depth = self.scopes.len();
        self.scopes.push(Scope::new(depth));
        for arg in func.args() {
            self.create_var(arg.clone());
        }
        for st in func.scope_mut() {
            self.statement(st)?;
            if let &mut Statement::ReturnStatement(ref mut expr) = st {
                self.check_type(&expr, &ret_type).map_err(|_| {
                    format!(
                        "line : {}\n\tFunction {} returns {} but value is of type {}",
                        &expr.get_line(),
                        name,
                        &ret_type,
                        &expr.return_type()
                    )
                })?;
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
            &mut Statement::Native(_) => Ok(()),
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
        let val_type = decl.val_type().clone();
        self.expression(decl.expr_mut())?;
        self.check_type(decl.expr(), &val_type)?;
        self.create_var(TypedVar::new(val_type.clone(), decl.name().to_string()));
        Ok(())
    }

    /// Checks the assignment :
    /// First checks the expression assigned.
    /// then what it is assigned to (must be a lvalue).
    /// then if they match.
    /// If we assign to a dereferenced value we need to asign behind the pointer.
    pub fn assignment(&mut self, assignment: &mut Assignment) -> Result<(), String> {
        self.expression(assignment.expr_mut())?;
        self.expression(assignment.assignee_mut())?;
        if !self.is_assignee(assignment.assignee()) {
            return Err(format!("can only assign to pointer or local variables "));
        }
        self.check_type(assignment.expr(), &assignment.assignee().return_type())?;
        self.deref_assignment(assignment.assignee_mut());
        Ok(())
    }

    pub fn deref_assignment(&mut self, assignement: &mut Expr) {
        let mut a = assignement;
        if let &mut ExprEnum::Deref(ref mut d) = a.expr_mut() {
            d.set_assigned();
            self.deref_assignment(d.inner_mut());
        }
    }

    pub fn is_assignee(&self, expression: &Expr) -> bool {
        expression.is_identifier() || self.is_pointer(expression) || expression.is_deref()
    }

    /// Sets the expression's return type.
    /// Sets the type of incoming and outcoming variables so the compiler will know what it needs to.
    pub fn expression(&mut self, expr: &mut Expr) -> Result<(), String> {
        let ret_type = expr.return_type_uncheck().clone();
        let tp = match expr.expr_mut() {
            &mut ExprEnum::Literal(_) => Ok(ret_type.unwrap()),
            &mut ExprEnum::Unary(ref mut u) => self.unary(u),
            &mut ExprEnum::Binary(ref mut b) => self.binary(b),
            &mut ExprEnum::GetAttr(ref mut b) => self.getattr(b),
            &mut ExprEnum::Identifier(ref mut i) => self.identifier(i),
            &mut ExprEnum::FunctionCall(ref mut f) => self.function_call(f),
            &mut ExprEnum::Deref(ref mut d) => {
                self.expression(d.inner_mut())?;
                Ok(d.inner().return_type())
            }
        }?;
        expr.set_type(tp);
        Ok(())
    }

    /// Parses a ___.___
    pub fn getattr(&mut self, expr: &mut BinaryExpr) -> Result<LisaaType, String> {
        if expr.operator() != Operator::Get {
            return Err(format!("Not a . operator"));
        } else {
            self.expression(expr.lhs_mut())?;
            let rhs = expr.rhs().get_identifier()?;
            expr.lhs().return_type().get_attr(rhs)
        }
    }

    /// Returns the type of the given identifier if it exists in scope.
    pub fn identifier(&mut self, id: &String) -> Result<LisaaType, String> {
        match self.get_var(id) {
            Some(ref var) => Ok(var.type_var().clone().unwrap()),
            None => Err(String::from(format!("Unknown variable : {}", id))),
        }
    }

    pub fn get_function_name(&mut self, func: &mut FunctionCall) -> Result<String, String> {
        match func.callee_mut() {
            &mut Callee::StaticFunc(ref mut s) => Ok(s.to_owned()),
            &mut Callee::Method(ref mut e) => {
                let line = e.get_line();
                self.expression(e)?;
                e.return_type()
                    .function_name()
                    .map_err(|()| format!("Line : {}\tNot a method\n", line))
            }
        }
    }

    pub fn get_function(
        &mut self,
        func: &mut FunctionCall,
    ) -> Result<(LisaaType, &Vec<TypedVar>), String> {
        let name = self.get_function_name(func)?;
        func.set_name(&name);
        match self.functions.get(&name) {
            Some(f) => Ok((f.ret_type().clone(), f.args())),
            None => Err(String::from(format!("Unknown function : {:?}", name))),
        }
    }

    /// Find the return type of a function call expression and returns it.
    /// Checks that arguments lists are the same size.
    /// Checks for arguments given to the function.
    /// And well... This is a bit embarassing...
    /// The ugly hack with the closure feels a little bit odd.
    pub fn function_call(&mut self, exp: &mut FunctionCall) -> Result<LisaaType, String> {
        let (ret, args) = {
            let (r, a) = self.get_function(exp)?;
            (r.clone(), a.clone())
        };
        let (args_count_given, args_count_expected) = (exp.args().len(), args.len());
        if args_count_expected != args_count_given {
            return Err(String::from(format!(
                "Error : expected {} arguments, {} given",
                args_count_expected, args_count_given
            )));
        }
        for i in 0..args_count_given {
            self.expression(&mut exp.args_mut()[i])?;
            self.check_type(&exp.args_mut()[i], &args[i].type_var().clone().unwrap())?;
        }
        Ok(ret)
    }

    /// Find the return type of a unary expression and returns it.
    pub fn unary(&mut self, exp: &mut UnaryExpr) -> Result<LisaaType, String> {
        self.expression(exp.expression_mut())?;
        let exp_res = exp.expression().return_type();
        match exp.operator() {
            Operator::MINUS => match exp_res {
                LisaaType::Num => Ok(LisaaType::Num),
                _ => Err(String::from("Operator '-' supported only for primitives")),
            },
            Operator::Not => match exp_res {
                LisaaType::Num => Ok(LisaaType::Num),
                _ => Err(String::from("Operator '!' supported only for primitives")),
            },
            e => Err(format!("operator {:?} can not be aplied to one value", e)),
        }
    }

    /// Find the return type of a binary expression and returns it.
    pub fn binary(&mut self, exp: &mut BinaryExpr) -> Result<LisaaType, String> {
        self.expression(exp.lhs_mut())?;
        self.expression(exp.rhs_mut())?;
        let exp_res = (exp.lhs().return_type(), exp.rhs().return_type());
        match exp.operator() {
            Operator::MINUS => match exp_res {
                (LisaaType::Num, LisaaType::Num) => Ok(LisaaType::Num),
                _ => Err(String::from("Operator '-' supported only for num")),
            },
            Operator::PLUS => match exp_res {
                (LisaaType::Num, LisaaType::Num) => Ok(LisaaType::Num),
                _ => Err(String::from("Operator '+' supported only for num")),
            },
            Operator::STAR => match exp_res {
                (LisaaType::Num, LisaaType::Num) => Ok(LisaaType::Num),
                _ => Err(String::from("Operator '*' supported only for num")),
            },
            Operator::SLASH => match exp_res {
                (LisaaType::Num, LisaaType::Num) => Ok(LisaaType::Num),
                _ => Err(String::from("Operator '*' supported only for num")),
            },
            Operator::GreaterEqual => Ok(LisaaType::Num),
            Operator::GREATER => Ok(LisaaType::Num),
            Operator::LessEqual => Ok(LisaaType::Num),
            Operator::LESS => Ok(LisaaType::Num),
            Operator::EqualEqual => Ok(LisaaType::Num),
            Operator::NotEqual => Ok(LisaaType::Num),
            Operator::AndAnd => Ok(LisaaType::Num),
            Operator::INDEX => match exp_res {
                (LisaaType::Slice(box inner), LisaaType::Num) => Ok(inner.clone()),
                _ => Err(String::from("Can only index slice using num")),
            },
            e => Err(format!("operator {:?} can not be aplied to two value", e)),
        }
    }

    /// Checks if the type is a pointer.
    /// This can happen if :
    /// The type is a number/char from a slice (indexed)
    /// The type is a number/char from an object (with . operator) // not yet implemented.
    pub fn is_pointer(&self, expr: &Expr) -> bool {
        if let LisaaType::Pointer(_) = expr.return_type() {
            return true;
        }
        return false;
    }
}
