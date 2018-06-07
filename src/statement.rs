//! The module for statement.
use expression::{Expr, LiteralExpr};
use std::collections::HashMap;
use types::{LisaaType, TypedVar};
use vm::OP;

/// Represents a program.
/// Classes and functions.
#[derive(Debug, Clone)]
pub struct Program {
    functions: HashMap<String, FunctionDecl>,
    classes: HashMap<String, ClassDecl>,
}
impl Program {
    /// Creates a new program with the given classes and functions.
    pub fn new(funcs: HashMap<String, FunctionDecl>, classes: HashMap<String, ClassDecl>) -> Self {
        Program {
            functions: funcs,
            classes: classes,
        }
    }
    /// Get the classes in the program.
    pub fn classes(&self) -> &HashMap<String, ClassDecl> {
        &self.classes
    }
    /// Get the classes as mutable.
    pub fn classes_mut(&mut self) -> &mut HashMap<String, ClassDecl> {
        &mut self.classes
    }
    /// Get the functions in the program.
    pub fn functions(&self) -> &HashMap<String, FunctionDecl> {
        &self.functions
    }
    /// Get the functinos in the program as mutable.
    pub fn functions_mut(&mut self) -> &mut HashMap<String, FunctionDecl> {
        &mut self.functions
    }
    /// Takes all the classes methods and add them to the program's functions.
    pub fn initiate_methods(&mut self){
        for c in self.classes.iter() {
            let cons = c.1.get_constructor();
            self.functions.insert(cons.name().to_owned(), cons);
        }
    }
}

/// An element in the code
#[derive(Debug, Clone)]
pub enum Element {
    /// A function declaration.
    Function(FunctionDecl),
    /// A class declaration.
    Class(ClassDecl),
}

/// A class declaration
#[derive(Debug, Clone)]
pub struct ClassDecl {
    name: String,
    attributes: Vec<Declaration>,
}

impl ClassDecl {
    /// Creates a new class with the given arguments.
    pub fn new(name: String, attrs: Vec<Declaration>) -> Self {
        ClassDecl {
            name: name,
            attributes: attrs,
        }
    }
    /// Returns the name of the class.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the constructor's function.
    pub fn get_constructor(&self) -> FunctionDecl {
        FunctionDecl {
            name: self.name.clone(),
            args: vec![],
            type_args: vec![],
            inline: false,
            ret_type: LisaaType::Class(self.name.clone()),
            scope: self.create_constructor_scope(),
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
            OP::AllocObj,
        ]));
        for i in 0..len {
            scope.push(Statement::Native(vec![
                OP::Bring(i+3),
                OP::Bring(len + 3),
                OP::PushNum(i as f64),
                OP::Add,
                OP::SetHeap,
            ]));
        }
        scope.push(Statement::Native(vec![OP::Set(0)]));
        Statement::Scope(scope)
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
    /// The type parameters of the function <Y, U, T>
    pub type_args: Vec<LisaaType>,
    /// The arguments taken by the function
    pub args: Vec<TypedVar>,
    /// The scope of the function, the statement inside it.
    pub scope: Statement,
    /// The return type of the function.
    pub ret_type: LisaaType,
}

impl FunctionDecl {
    /// Creates a new function declaration.
    pub fn new(
        name: String,
        type_args: Vec<LisaaType>,
        args: Vec<TypedVar>,
        scope: Statement,
        ret_type: LisaaType,
    ) -> Self {
        FunctionDecl {
            inline: false,
            name: name,
            type_args: type_args,
            args: args,
            scope: scope,
            ret_type: ret_type,
        }
    }
    /// returns the return type of the function.
    pub fn ret_type(&self) -> &LisaaType {
        &self.ret_type
    }
    /// Returns the name of the function.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the arguments of a function.
    pub fn type_args(&self) -> &Vec<TypedVar> {
        &self.args
    }
    /// Returns the arguments of a function.
    pub fn args(&self) -> &Vec<TypedVar> {
        &self.args
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

/// The result of a statement.
#[derive(Debug, Clone)]
pub enum StatementResult {
    /// The statement does not return anything.
    Empty,
    /// The statement Is a break statement.
    Break,
    /// The statement was broken by a return.
    Return(LiteralExpr),
}

impl Statement{
    /// Checks if the statement will make the scope quit.
    pub fn into_decl(self) -> Declaration {
        match self {
            Statement::Declaration(d) => d,
            _ => panic!("not a declaration wtf ?"),
        }
    }
}
