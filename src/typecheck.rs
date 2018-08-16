use expression::{BinaryExpr, Callee, Expr, ExprEnum, FunctionCall, Operator, UnaryExpr};
use generic_inference::Inferer;
use native::{get_any_trait, get_native_funcs, get_native_types};
use statement::{
    Assignment, ClassDecl, Declaration, FunctionDecl, FunctionSig, IfStatement, Program, Statement,
    TraitDecl, TypeParam, WhileStatement,
};
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use types::{LisaaType, TypedVar};

/// Represents a scope with its variables.
#[derive(Debug)]
pub struct Scope {
    type_params: HashMap<String, String>,
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
            type_params: HashMap::new(),
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

    /// Creates a new variable with the given name.
    pub fn create_type_param(&mut self, name: String, t: String) {
        self.type_params.insert(name.clone(), t);
    }
}

/// The type checker
/// Contains a program and functions to resolve types/verify consistency.
/// Also check for lvalues and assignment.
pub struct TypeChecker {
    called_functions: HashMap<FunctionSig, (FunctionDecl, Vec<TypeParam>, Vec<LisaaType>)>,
    functions: HashMap<FunctionSig, FunctionDecl>,
    current_morphisation : HashMap<String, LisaaType>,
    /// Returns all the local functions that can be called on generics
    local_functions: HashSet<FunctionSig>,
    classes: HashMap<String, ClassDecl>,
    traits: HashMap<String, HashMap<String, FunctionSig>>,
    scopes: Vec<Scope>,
}

impl TypeChecker {
    /// Creates a new typechekcer object.
    pub fn new() -> Self {
        TypeChecker {
            called_functions: HashMap::new(),
            scopes: vec![Scope::new(1)],
            functions: HashMap::new(),
            local_functions: HashSet::new(),
            current_morphisation : HashMap::new(),
            classes: HashMap::new(),
            traits: HashMap::new(),
        }
    }
    /// Add a lib to the program.
    pub fn add_natives(&mut self, lib: &str) {
        for f in get_native_funcs(lib) {
            self.functions.insert(f.signature().clone(), f);
        }
        for c in get_native_types() {
            self.classes.insert(c.name().to_owned(), c);
        }
    }
    /// Creates a variable in the current scope
    pub fn create_var(&mut self, var: TypedVar) {
        self.scopes.last_mut().unwrap().create_var(var);
    }

    /// Creates a type parameter in the current scope
    pub fn create_type_param(&mut self, name: String, t: String) {
        self.scopes.last_mut().unwrap().create_type_param(name, t);
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
                "Line : {}, Expected : {}, got : {}",
                expr.get_line(),
                expected,
                expr.return_type()
            ));
        }
        Ok(())
    }

    /// Resolve types if possible
    /// The aim is to traverse the tree and resolve the return type of all expressions.
    pub fn resolve(&mut self, program: &mut Program) -> Result<(), String> {
        self.functions = program.functions().clone();
        self.classes = program.classes().clone();
        self.add_natives("base");
        program
            .traits_mut()
            .insert("Any".to_string(), get_any_trait());
        self.complete_traits(program.traits())?;
        for (_, mut func) in program.functions_mut() {
            self.function(&mut func)?;
        }
        let funcs = match program.functions().get(&FunctionSig::new(
            vec![],
            vec![],
            LisaaType::Void,
            "main".to_string(),
            None,
        )) {
            Some(main) => Ok(self.monomorphise(main.clone())?),
            None => Err("No main function found".to_string()),
        }?;
        program.set_functions(funcs);
        Ok(())
    }

    /// Monomorphise all functions and return them all
    ///
    pub fn monomorphise(
        &mut self,
        mut main: FunctionDecl,
    ) -> Result<HashMap<FunctionSig, FunctionDecl>, String> {
        self.called_functions = HashMap::new();
        self.function(&mut main)?;
        let mut morphised = HashMap::new();
        morphised.insert(main.signature().clone(), main);
        while self.called_functions.len() > 0 {
            let (key, mut new_decl, params, actual) = {
                let (key, mut decl) = self.called_functions.iter_mut().next().unwrap();
                (key.clone(), decl.0.clone(), decl.1.clone(), decl.2.clone())
            };
            self.called_functions.remove(&key);
            if !morphised.contains_key(&key) {
                if !new_decl.inline {
                    self.set_current_morphisation(params, actual);
                    self.function(&mut new_decl)?;
                }
                morphised.insert(key, new_decl);
                //println!("called functions by main : {:?}", self.called_functions.iter().map(|(s, d)|&s.name).collect::<Vec<&String>>());
            }
        }
        Ok(morphised)
    }

    /// sets the current morphisation's values :
    pub fn set_current_morphisation(&mut self, params : Vec<TypeParam>, actual : Vec<LisaaType>){
        self.current_morphisation = HashMap::new();
        for (generic, actual) in params.iter().zip(actual.into_iter()){
            self.current_morphisation.insert(generic.name().clone(), actual);
        }
    }

    /// Complete all the traits by replacing the inner traits by a list of methods.
    /// when encountering a trait, if it has uncompleted sub_traits add them to the queue else complete
    /// it and remove it from the queue
    /// absolutely unoptimised but should not be a problem
    pub fn complete_traits(&mut self, traits: &HashMap<String, TraitDecl>) -> Result<(), String> {
        let mut queue = Vec::from_iter(traits.iter().map(|(name, t)| name));
        while let Some(name) = queue.pop() {
            if !self.traits.contains_key(name) {
                let uncompleted = traits
                    .get(name)
                    .ok_or(format!("unknown trait : {:?}", name.clone()))?
                    .sub_traits()
                    .iter()
                    .filter(|&t| !self.traits.contains_key(t))
                    .collect::<Vec<&String>>();
                if uncompleted.len() == 0 {
                    self.insert_trait(name, traits.get(name).unwrap());
                } else {
                    queue.push(name);
                    uncompleted.iter().for_each(|&s| queue.push(s));
                }
            }
        }
        Ok(())
    }

    /// Insert a trait in the type checker.
    pub fn insert_trait(&mut self, name: &String, t: &TraitDecl) {
        let mut new_map = HashMap::new();
        for sub in t.sub_traits() {
            new_map.extend(self.traits.get(sub).unwrap().clone().into_iter())
        }
        for sub in t.methods() {
            new_map.insert(sub.0.clone(), sub.1.clone());
        }
        self.traits.insert(name.clone(), new_map);
    }

    pub fn get_trait_by_name(
        &self,
        name: &String,
    ) -> Result<&HashMap<String, FunctionSig>, String> {
        self.traits
            .get(name)
            .ok_or(format!("could not find trait {:?}", name))
    }

    /// Resolve/check types for a function declaration
    /// TODO: add checks that type params appear in arguments.
    pub fn function(&mut self, func: &mut FunctionDecl) -> Result<(), String> {
        self.add_function_in_scope(func);
        self.add_func_type_args(func)?;
        self.check_function(func)?;
        self.leave_function(func);
        Ok(())
    }

    /// leave the function, remove the scope and the functions for type arguments.
    pub fn leave_function(&mut self, func: &mut FunctionDecl) {
        self.scopes.pop();
        self.local_functions = HashSet::new();
    }

    /// typechecks the function.
    pub fn check_function(&mut self, func: &mut FunctionDecl) -> Result<(), String> {
        let (ret_type, name) = (func.ret_type().clone(), func.name().to_string());
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
        Ok(())
    }

    /// Adds the type arguments and theyre constructors in scope.
    pub fn add_func_type_args(&mut self, func: &mut FunctionDecl) -> Result<(), String> {
        for arg in func.type_args() {
            let trait_name = arg.trait_name().clone();
            self.create_type_param(arg.name().clone(), trait_name);
            self.add_local_funcs_for(arg)?;
        }
        Ok(())
    }

    /// Add all the local functions of the given generic type
    /// create functions of the form T::add(T) -> T
    pub fn add_local_funcs_for(&mut self, arg: &TypeParam) -> Result<(), String> {
        for (name, sig) in self
            .traits
            .get(arg.trait_name())
            .ok_or(format!("Unknown trait : {}", arg.trait_name()))?
            .iter()
        {
            let full_name = if name == &"".to_string() {
                arg.name().clone()
            } else {
                format!("{}::{}", arg.name(), name)
            };
            let sig = FunctionSig::new_simple_args(
                Self::replace_big_self_in_generics(&sig.type_args, arg.name()), // the arguments of the signature
                Self::replace_big_self(&sig.args, arg.name()), // the arguments but replace Self by th,
                Self::replace_big_self_once(&sig.ret_type, arg.name()),
                full_name.clone(),
                if &full_name == name {
                    None
                } else {
                    Some(LisaaType::Class(arg.name().clone(), vec![]))
                },
            );
            self.local_functions.insert(sig);
        }
        Ok(())
    }
    /// Replace big self by a type parameter in a list of types
    pub fn replace_big_self_in_generics(
        orig: &Vec<TypeParam>,
        type_name: &String,
    ) -> Vec<TypeParam> {
        orig.iter()
            .map(|a| {
                TypeParam::new(
                    a.name().replace("Self", type_name.as_ref()),
                    a.trait_name().clone(),
                )
            })
            .collect()
    }
    /// Replace big self by a type parameter in a list of types
    pub fn replace_big_self(orig: &Vec<LisaaType>, type_name: &String) -> Vec<LisaaType> {
        orig.iter()
            .map(|a| Self::replace_big_self_once(a, type_name))
            .collect()
    }
    /// Replace big self by a type parameter in a list of types
    pub fn replace_big_self_once(arg: &LisaaType, type_name: &String) -> LisaaType {
        match arg {
            &LisaaType::Class(ref class_name, ref e) => LisaaType::Class(
                if class_name == &"Self".to_string() {
                    type_name.clone()
                } else {
                    class_name.clone()
                },
                e.clone(),
            ),
            e => e.clone(),
        }
    }
    /// Add the parameters of the function in scope (including self etc...)
    pub fn add_function_in_scope(&mut self, func: &mut FunctionDecl) {
        let depth = self.scopes.len();
        self.scopes.push(Scope::new(depth));
        func.self_type()
            .clone()
            .map(|t| self.create_var(TypedVar::new(t, "self".to_string())));
        for arg in func.args() {
            self.create_var(arg.clone());
        }
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
        let val_type = self.replace_gen(decl.val_type());
        self.expression(decl.expr_mut())?;
        self.check_type(decl.expr(), &val_type)?;
        self.create_var(TypedVar::new(val_type.clone(), decl.name().to_string()));
        Ok(())
    }


    /// Replace all generics in a type.
    ///used in assignements.
    pub fn replace_gen(&self, declared_type : &LisaaType) -> LisaaType {
        match declared_type {
            LisaaType::Pointer(ref t) => LisaaType::Pointer(Box::new(self.replace_gen(t))),
            LisaaType::Class(ref n, ref args) => match self.current_morphisation.get(n){
                Some(actual_type) => actual_type.clone(),
                None => LisaaType::Class(n.clone(), args.iter().map(|a|self.replace_gen(a)).collect()),
            }
            other => other.clone()
        }
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
        let a = assignement;
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
        let line = expr.get_line();
        let tp = match expr.expr_mut() {
            &mut ExprEnum::Literal(_) => Ok(ret_type.unwrap()),
            &mut ExprEnum::Unary(ref mut u) => self.unary(u),
            &mut ExprEnum::GetAttr(ref mut b) => self.getattr(b),
            &mut ExprEnum::Identifier(ref mut i) => self.identifier(i, line),
            &mut ExprEnum::FunctionCall(ref mut f) => self.function_call(f, line),
            &mut ExprEnum::Deref(ref mut d) => {
                self.expression(d.inner_mut())?;
                Ok(d.inner().return_type())
            }
        }?;
        expr.set_type(self.replace_gen(&tp));
        Ok(())
    }

    /// Parses a ___.___
    pub fn getattr(&mut self, expr: &mut BinaryExpr) -> Result<LisaaType, String> {
        if expr.operator() != Operator::Get {
            return Err(format!("Not a . operator"));
        } else {
            self.expression(expr.lhs_mut())?;
            let rhs = expr.rhs().get_identifier()?;
            expr.lhs()
                .return_type()
                .get_attr(
                    rhs,
                    &self.get_classes_with_type_params(),
                    &self.get_functions_with_type_params(),
                )
                .map_err(|e| format!("{} line {}", e, expr.lhs().get_line()))
        }
    }

    pub fn get_classes_with_type_params(&self) -> HashMap<String, ClassDecl> {
        let mut new_map = self.classes.clone();
        for (name, _) in self.scopes.last().unwrap().type_params.iter() {
            new_map.insert(name.clone(), ClassDecl::new(name.clone(), vec![], vec![]));
        }
        new_map
    }

    /// Returns a hashmap of all the functions currently in scope
    pub fn get_functions_with_type_params(&self) -> HashMap<FunctionSig, FunctionDecl> {
        let mut new_map = self.functions.clone();
        for (type_name, trait_name) in self.scopes.last().unwrap().type_params.iter() {
            for (func_name, func_sig) in self.traits.get(trait_name).unwrap() {
                let complete_name = format!("{}::{}", type_name, func_name);
                let func_decl = FunctionDecl::from_sig(complete_name, func_sig.clone());
                new_map.insert(func_decl.signature().clone(), func_decl);
            }
        }
        new_map
    }

    /// Returns the type of the given identifier if it exists in scope.
    pub fn identifier(&mut self, id: &String, line : usize) -> Result<LisaaType, String> {
        match self.get_var(id) {
            Some(ref var) => Ok(var.type_var().clone().unwrap()),
            None => Err(String::from(format!("Unknown variable : {} line {}", id, line))),
        }
    }

    /// Returns the name of the called function.
    pub fn get_function_name(&mut self, func: &mut FunctionCall) -> Result<String, String> {
        match func.callee_mut() {
            &mut Callee::StaticFunc(ref mut s) => Ok(s.to_owned()),
            &mut Callee::Method(ref mut e) => {
                let line = e.get_line();
                self.expression(e)?;
                e.return_type().function_name().map_err(|()| {
                    format!("Line : {}\tNot a method : {:?}\n", line, e.return_type())
                })
            }
        }
    }

    /// Find the return type of a function call expression and returns it.
    /// Checks that arguments lists are the same size.
    /// Checks for arguments given to the function.
    pub fn function_call(&mut self, exp: &mut FunctionCall, line : usize) -> Result<LisaaType, String> {
        let args_count_given = exp.args().len();
        let mut given_types = vec![];
        for i in 0..args_count_given {
            self.expression(&mut exp.args_mut()[i])?;
            given_types.push(exp.args_mut()[i].return_type());
        }
        let name = self.get_function_name(exp)?;
        let type_args = exp.type_args().clone();
        let (sig, decl) = Inferer::new(
            &self.functions,
            &self.local_functions,
            given_types,
            exp.type_args().clone(),
            exp.callee().get_caller_type(),
            name,
            &self.traits,
        ).infer(line)?;
        let to_ins = self.try_insert_called_function(&sig, &decl);
        exp.set_signature(sig.clone());
        if let Some(val) = to_ins {
            self.called_functions.insert(sig, val);
        }
        Ok(exp.signature().return_type().clone())
    }

    pub fn try_insert_called_function(
        &self,
        sig: &FunctionSig,
        decl: &Option<(&FunctionDecl, Vec<TypeParam>, Vec<LisaaType>)>,
    ) -> Option<(FunctionDecl, Vec<TypeParam>, Vec<LisaaType>)> {
        if !self.called_functions.contains_key(&sig) {
            if let &Some((ref d, ref gen, ref act)) = decl {
                let mut new_decl = d.to_owned().clone();
                new_decl.set_signature(sig.clone());
                return Some((new_decl, gen.clone(), act.clone()));
            }
        }
        None
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
