//! The module for statement.
use expression::Expr;
use std::collections::HashMap;
use types::{LisaaType, TypedVar};
use vm::OP;

/// Represents a program.
/// Classes and functions.
#[derive(Debug, Clone)]
pub struct Program {
    functions: HashMap<FunctionSig, FunctionDecl>,
    classes: HashMap<String, ClassDecl>,
    traits: HashMap<String, TraitDecl>,
}
impl Program {
    /// Creates an empty program.
    pub fn empty() -> Self {
        Program {
            functions: HashMap::new(),
            classes: HashMap::new(),
            traits: HashMap::new(),
        }
    }
    /// Creates a new program with the given classes and functions.
    pub fn new(
        funcs: HashMap<FunctionSig, FunctionDecl>,
        classes: HashMap<String, ClassDecl>,
        traits: HashMap<String, TraitDecl>,
    ) -> Self {
        Program {
            functions: funcs,
            classes: classes,
            traits: traits,
        }
    }
    /// Get the classes in the program.
    pub fn classes(&self) -> &HashMap<String, ClassDecl> {
        &self.classes
    }
    /// Get the functions in the program.
    pub fn traits(&self) -> &HashMap<String, TraitDecl> {
        &self.traits
    }
    /// Get the functions in the program.
    pub fn traits_mut(&mut self) -> &mut HashMap<String, TraitDecl> {
        &mut self.traits
    }
    /// Get the functions in the program.
    pub fn functions(&self) -> &HashMap<FunctionSig, FunctionDecl> {
        &self.functions
    }
    /// Get the functinos in the program as mutable.
    pub fn functions_mut(&mut self) -> &mut HashMap<FunctionSig, FunctionDecl> {
        &mut self.functions
    }

    /// Takes all the classes methods and add them to the program's functions.
    pub fn initiate_methods(&mut self) {
        for c in self.classes.iter() {
            let cons = c.1.get_constructor();
            self.functions.insert(cons.signature().to_owned(), cons);
        }
    }
    /// Merges this program with an other.
    pub fn merge(
        &mut self,
        Program {
            functions,
            classes,
            traits,
        }: Program,
    ) -> Result<(), String> {
        for func in functions {
            if self.functions.get(&func.0).is_some() {
                return Err(format!("function already exists : {:?}", func.0));
            } else {
                self.functions.insert(func.0, func.1);
            }
        }
        for class in classes {
            if self.classes.get(&class.0).is_some() {
                return Err(format!("class already exists : {:?}", class.0));
            } else {
                self.classes.insert(class.0, class.1);
            }
        }
        for t in traits {
            if self.traits.get(&t.0).is_some() {
                return Err(format!("class already exists : {:?}", t.0));
            } else {
                self.traits.insert(t.0, t.1);
            }
        }
        Ok(())
    }
}

/// An element in the code
#[derive(Debug, Clone)]
pub enum Element {
    /// A function declaration.
    Function(FunctionDecl),
    /// A class declaration.
    Class(ClassDecl),
    /// Import a module
    Import(String),
    /// declares a trait
    Trait(TraitDecl),
}

/// A trait declaration
#[derive(Debug, Clone)]
pub struct TraitDecl {
    name: String,
    sub_traits: Vec<String>,
    methods: HashMap<String, FunctionSig>,
}

impl TraitDecl {
    /// Creates a new trait decl with the given name, sub traits and methods
    pub fn new(
        name: String,
        mut sub_traits: Vec<String>,
        methods: HashMap<String, FunctionSig>,
    ) -> Self {
        if name != "Any" {
            sub_traits.push("Any".to_string());
        }
        TraitDecl {
            name: name,
            sub_traits: sub_traits,
            methods: methods,
        }
    }
    /// Returns the name of the class.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the name of the class.
    pub fn methods(&self) -> &HashMap<String, FunctionSig> {
        &self.methods
    }
    ///  of the class.
    pub fn sub_traits(&self) -> &Vec<String> {
        &self.sub_traits
    }
}

/// A class declaration
#[derive(Debug, Clone)]
pub struct ClassDecl {
    name: String,
    attributes: Vec<Declaration>,
    type_params: Vec<TypeParam>,
}

impl ClassDecl {
    /// Creates a new class with the given arguments.
    pub fn new(name: String, attrs: Vec<Declaration>, type_params: Vec<TypeParam>) -> Self {
        ClassDecl {
            name: name,
            attributes: attrs,
            type_params: type_params,
        }
    }
    /// Returns the type of object, this is a number between 0 and 64 for which the bits represents
    /// the positions of the pointers.
    /// eg (int, ptr, int, char, ptr) would be 010011 -> 2+16+32 => 50
    pub fn get_mem_descriptor(&self) -> u64 {
        self.attributes
            .iter()
            .enumerate()
            .filter(|(i, a)| a.val_type() != &LisaaType::Num && a.val_type() != &LisaaType::Char)
            .map(|(id, a)| 2u64.pow(id as u32))
            .sum::<u64>() + 2u64.pow(self.attributes.len() as u32)
    }
    /// Returns the name of the class.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the name of the class.
    pub fn type_params(&self) -> &Vec<TypeParam> {
        &self.type_params
    }
    /// Get the declaration of an attribute given its name.
    pub fn get_attr(&self, str: &String) -> Option<&Declaration> {
        self.attributes.iter().find(|d| &d.val_name == str)
    }
    /// Get the declaration of an attribute given its name.
    pub fn get_attr_index(&self, str: &String) -> usize {
        self.attributes
            .iter()
            .position(|d| &d.val_name == str)
            .expect("attr not found")
    }
    /// Returns the constructor's function.
    pub fn get_constructor(&self) -> FunctionDecl {
        FunctionDecl {
            self_type: None,
            name: self.name.clone(),
            signature: FunctionSig::new(
                self.type_params.clone(),
                vec![],
                LisaaType::Class(
                    self.name.clone(),
                    self.type_params
                        .iter()
                        .map(|t| LisaaType::Class(t.name().to_string(), vec![]))
                        .collect(),
                ),
                self.name.clone(),
            ),
            inline: false,
            scope: self.create_constructor_scope(),
            arguments: vec![],
        }
    }
    /// Creates the code for the constructor.
    /// First allocates some space.
    /// Then performs the assignments/expressions in the stack.
    /// Then assign the variables in the heap.
    /// The object's pointer is stored at len+2 in the stack.
    pub fn create_constructor_scope(&self) -> Statement {
        let mut scope: Vec<Statement> = self
            .attributes
            .iter()
            .map(|d| Statement::Declaration(d.clone()))
            .collect();
        let len = self.attributes.len();
        scope.push(Statement::Native(vec![
            OP::PushNum(len as f64),
            OP::AllocObj(self.get_mem_descriptor()),
        ]));
        for i in 0..len {
            scope.push(Statement::Native(vec![
                OP::Bring(i + 3),
                OP::Bring(len + 3),
                OP::PushNum(i as f64),
                OP::Add,
                OP::SetHeap,
            ]));
        }
        scope.push(Statement::Native(vec![
            OP::Set(0),
            OP::PopN(len),
            OP::SetOffset,
            OP::GotoTop,
        ]));
        Statement::Scope(scope)
    }
}
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
/// A type parameter given to a function.
pub struct TypeParam {
    name: String,
    trait_name: String,
}

impl TypeParam {
    /// creates a new type parameter associating a name and a trait
    pub fn new(name: String, trait_name: String) -> Self {
        TypeParam {
            name: name,
            trait_name: trait_name,
        }
    }
    /// Returns the name given to the type parameter
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the trait's name
    pub fn trait_name(&self) -> &String {
        &self.trait_name
    }
}

/// A function declaration
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FunctionSig {
    /// The type parameters of the function <Y, U, T>, name
    pub type_args: Vec<TypeParam>,
    /// The arguments taken by the function
    pub args: Vec<LisaaType>,
    /// The return type of the function.
    pub ret_type: LisaaType,
    /// The name
    pub name: String,
}
impl FunctionSig {
    /// Creates a new function signature.
    pub fn new(
        type_args: Vec<TypeParam>,
        args: Vec<TypedVar>,
        ret_type: LisaaType,
        name: String,
    ) -> Self {
        FunctionSig {
            type_args: type_args,
            args: args.iter().map(|a| a.type_var().clone().unwrap()).collect(),
            ret_type: ret_type,
            name: name,
        }
    }

    /// Creates a new function signature.
    pub fn new_simple_args(
        type_args: Vec<TypeParam>,
        args: Vec<LisaaType>,
        ret_type: LisaaType,
        name: String,
    ) -> Self {
        FunctionSig {
            type_args: type_args,
            args: args,
            ret_type: ret_type,
            name: name,
        }
    }

    ///
    pub fn is_equivalent_to(&self, other: FunctionSig) -> bool {
        unimplemented!()
    }
    ///
    pub fn name(&self) -> &String {
        &self.name
    }
    ///
    pub fn args_as_typevar(&self) -> Vec<TypedVar> {
        self.args
            .iter()
            .map(|a| TypedVar::new(a.clone(), "".to_string()))
            .collect()
    }
    ///
    pub fn return_type(&self) -> &LisaaType {
        &self.ret_type
    }
}

/// A function declaration
#[derive(Debug, Clone)]
pub struct FunctionDecl {
    /// Tells is the function is inline, if it is there are no return and goto instruction,
    /// The return value will simply be put on the top of the stack.
    pub inline: bool,
    /// The name of the function.
    pub name: String,
    /// The scope of the function, the statement inside it.
    pub scope: Statement,
    /// If the function is a method, the type of self.
    pub self_type: Option<LisaaType>,
    /// The signature of the function
    pub signature: FunctionSig,
    /// The function argument's names
    pub arguments: Vec<TypedVar>,
}

impl FunctionDecl {
    /// Creates a new function declaration.
    pub fn new(
        name: String,
        type_args: Vec<TypeParam>,
        args: Vec<TypedVar>,
        scope: Statement,
        ret_type: LisaaType,
    ) -> Self {
        FunctionDecl {
            inline: false,
            name: name.clone(),
            signature: FunctionSig::new(type_args, args.clone(), ret_type, name),
            scope: scope,
            self_type: None,
            arguments: args,
        }
    }

    /// Creates a new function declaration.
    pub fn from_sig(name: String, sig: FunctionSig) -> Self {
        let args_typevar = sig.args_as_typevar();
        FunctionDecl {
            inline: false,
            name: name.clone(),
            signature: FunctionSig::new(sig.type_args, args_typevar.clone(), sig.ret_type, name),
            scope: Statement::Native(vec![]),
            self_type: None,
            arguments: args_typevar,
        }
    }

    /// Creates a new function declaration.
    pub fn new_complete(
        self_type: Option<LisaaType>,
        inline: bool,
        name: String,
        type_args: Vec<TypeParam>,
        args: Vec<TypedVar>,
        scope: Statement,
        ret_type: LisaaType,
    ) -> Self {
        FunctionDecl {
            self_type: self_type,
            inline: inline,
            name: name.clone(),
            signature: FunctionSig::new(type_args, args.clone(), ret_type, name),
            scope: scope,
            arguments: args,
        }
    }
    /// set the type of the "self" if it is a method.
    pub fn set_self(&mut self, tp: LisaaType) {
        self.self_type = Some(tp)
    }
    /// returns the return type of the function.
    pub fn signature(&self) -> &FunctionSig {
        &self.signature
    }
    /// returns the return type of the function.
    pub fn ret_type(&self) -> &LisaaType {
        &self.signature.ret_type
    }
    /// Returns the name of the function.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the arguments of a function.
    #[allow(dead_code)]
    pub fn type_args(&self) -> &Vec<TypeParam> {
        &self.signature.type_args
    }
    /// Returns the arguments of a function.
    #[allow(dead_code)]
    pub fn args(&self) -> &Vec<TypedVar> {
        &self.arguments
    }
    /// Returns the scope of the function.
    /// TODO : this unwrap ?
    pub fn scope(&self) -> &Vec<Statement> {
        let val = match &self.scope {
            &Statement::Scope(ref v) => Some(v),
            _ => None,
        };
        val.unwrap()
    }
    /// Returns the scope of the function.
    /// TODO : this unwrap ?
    pub fn scope_mut(&mut self) -> &mut Vec<Statement> {
        let val = match &mut self.scope {
            &mut Statement::Scope(ref mut v) => Some(v),
            _ => None,
        };
        val.unwrap()
    }
    /// Checks if the function must be inlined.
    pub fn is_inline(&self) -> bool {
        self.inline
    }
}

#[derive(Debug, Clone)]
/// The enum for statements.
pub enum Statement {
    /// an expression followed by a semicolon.
    ExprStatement(Expr),
    /// An assignment.
    Assignment(Assignment),
    /// A declaration.
    Declaration(Declaration),
    /// A scope
    Scope(Vec<Statement>),
    /// An if statement with the close and ... the elses.
    IfStatement(IfStatement),
    /// An while statement with the close and ... the elses.
    WhileStatement(WhileStatement),
    /// A break statement.
    BreakStatement,
    /// A return statement,
    ReturnStatement(Expr),
    /// Some bytecode, used in native expressions.
    Native(Vec<OP>),
}

#[derive(Debug, Clone)]
/// An assignment is an identifier plus an expression.
/// The assignee cannot be a literal or the result of a literal value.
pub struct Assignment {
    assignee: Expr,
    expr: Expr,
}

impl Assignment {
    /// Creates a new assignment.
    pub fn new(assignee: Expr, expr: Expr) -> Self {
        Assignment {
            assignee: assignee,
            expr: expr,
        }
    }
    /// Returns the value assigned to.
    pub fn assignee(&self) -> &Expr {
        &self.assignee
    }
    /// Returns the value assigned to.
    pub fn assignee_mut(&mut self) -> &mut Expr {
        &mut self.assignee
    }
    /// Returns the expression.
    pub fn expr(&self) -> &Expr {
        &self.expr
    }
    /// Returns the expression.
    pub fn expr_mut(&mut self) -> &mut Expr {
        &mut self.expr
    }
}

#[derive(Debug, Clone)]
/// A declaration is an assignment. no null values
pub struct Declaration {
    val_name: String,
    val_type: LisaaType,
    assignment: Assignment,
}

impl Declaration {
    /// Creates a new declaration.
    pub fn new(val_type: LisaaType, identifier: String, assignment: Assignment) -> Self {
        Declaration {
            val_type: val_type,
            val_name: identifier,
            assignment: assignment,
        }
    }
    /// Returns the assignment.
    pub fn assignment(&self) -> &Assignment {
        &self.assignment
    }
    /// Returns the name of the variable assigned to.
    pub fn name(&self) -> &String {
        &self.val_name
    }
    /// Returns the expression assigned to the value.
    pub fn expr_mut(&mut self) -> &mut Expr {
        self.assignment.expr_mut()
    }
    /// Returns the expression assigned to the value.
    pub fn expr(&self) -> &Expr {
        self.assignment.expr()
    }
    /// Returns the expression assigned to the value.
    pub fn val_type(&self) -> &LisaaType {
        &self.val_type
    }
}
/// Represents an if statement, its condition and the statement to exeute if it is true.
#[derive(Debug, Clone)]
pub struct IfStatement {
    cond: Expr,
    statement: Box<Statement>,
}
impl IfStatement {
    /// Creates a new if statement with the following condition and statement to execute.
    pub fn new(cond: Expr, statement: Statement) -> Self {
        IfStatement {
            cond: cond,
            statement: Box::new(statement),
        }
    }
    /// Returns the condition to execute.
    pub fn condition(&self) -> &Expr {
        &self.cond
    }
    /// Returns the statement to execute.
    pub fn statement(&self) -> &Statement {
        &*self.statement
    }
    /// Returns the condition to execute.
    pub fn condition_mut(&mut self) -> &mut Expr {
        &mut self.cond
    }
    /// Returns the statement to execute.
    pub fn statement_mut(&mut self) -> &mut Statement {
        &mut *self.statement
    }
}

/// Represents an while statement, its condition and the statement to exeute if it is true.
#[derive(Debug, Clone)]
pub struct WhileStatement {
    cond: Expr,
    statement: Box<Statement>,
}
impl WhileStatement {
    /// Creates a new while statement with the following condition and statement to execute.
    pub fn new(cond: Expr, statement: Statement) -> Self {
        WhileStatement {
            cond: cond,
            statement: Box::new(statement),
        }
    }
    /// Returns the condition to execute.
    pub fn condition(&self) -> &Expr {
        &self.cond
    }
    /// Returns the statement to execute.
    pub fn statement(&self) -> &Statement {
        &*self.statement
    }
    /// Returns the condition to execute.
    pub fn condition_mut(&mut self) -> &mut Expr {
        &mut self.cond
    }
    /// Returns the statement to execute.
    pub fn statement_mut(&mut self) -> &mut Statement {
        &mut *self.statement
    }
}

impl Statement {
    /// Checks if the statement will make the scope quit.
    pub fn into_decl(self) -> Declaration {
        match self {
            Statement::Declaration(d) => d,
            _ => panic!("not a declaration wtf ?"),
        }
    }

    /// A for statement is just a init cond followed by a while
    pub fn for_statement(
        init: Statement,
        cond: Expr,
        repeat: Statement,
        inner: Statement,
    ) -> Statement {
        let inner_scope = Statement::Scope(vec![inner, repeat]);
        let inner_while = Statement::WhileStatement(WhileStatement::new(cond, inner_scope));
        Statement::Scope(vec![init, inner_while])
    }
}
