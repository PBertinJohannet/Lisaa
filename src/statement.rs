//! The module for statement.
use expression::{Expr, LiteralExpr};
use token::Token;


/// A function declaration
#[derive(Debug)]
pub struct FunctionDecl{
    name : String,
    args : Vec<String>,
    scope : Statement,
}

impl FunctionDecl {
    /// Creates a new function declaration.
    pub fn new(name : String, args : Vec<String>, scope : Statement) -> Self {
        FunctionDecl {
            name : name,
            args : args,
            scope : scope,
        }
    }
    /// Returns the name of the function.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the arguments of a function.
    pub fn args(&self) -> &Vec<String> {
        &self.args
    }
    /// Returns the scope of the function.
    pub fn scope(&self) -> &Vec<Statement >{
        let val = match &self.scope {
            &Statement::Scope(ref v) => Some(v),
            _ => None,
        };
        val.unwrap()
    }
}

#[derive(Debug)]
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
}



#[derive(Debug)]
/// An assignment is an identifier plus an expression.
pub struct Assignment {
    identifier : String,
    expr : Expr,
}

impl Assignment {
    /// Creates a new assignment.
    pub fn new(identifier : String, expr : Expr) -> Self{
        Assignment {
            identifier : identifier,
            expr : expr,
        }
    }
    /// Returns the identifier.
    pub fn identifier(&self) -> &String {
        &self.identifier
    }
    /// Returns the expression.
    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug)]
/// A declaration is an assignment. no null values
pub struct Declaration {
    val_name : String,
    val_type : String,
    assignment : Assignment,
}

impl Declaration {
    /// Creates a new declaration.
    pub fn new(val_type : String, identifier : String, assignment : Assignment) -> Self {
        Declaration {
            val_type : val_type,
            val_name : identifier,
            assignment : assignment,
        }
    }
    /// Returns the name of the variable assigned to.
    pub fn name(&self) -> &String {
        &self.val_name
    }
    /// Returns the expression assigned to the value.
    pub fn expr(&self) -> &Expr {
        self.assignment.expr()
    }
}
/// Represents an if statement, its condition and the statement to exeute if it is true.
#[derive(Debug)]
pub struct IfStatement {
    cond : Expr,
    statement : Box<Statement>,
}
impl IfStatement {
    /// Creates a new if statement with the following condition and statement to execute.
    pub fn new(cond : Expr, statement : Statement) -> Self {
        IfStatement{
            cond : cond,
            statement : Box::new(statement),
        }
    }
    /// Returns the condition to execute.
    pub fn condition(&self) -> &Expr {
        &self.cond
    }
    /// Returns the statement to execute.
    pub fn statement(&self) -> &Statement{
        &*self.statement
    }
}

/// Represents an while statement, its condition and the statement to exeute if it is true.
#[derive(Debug)]
pub struct WhileStatement {
    cond : Expr,
    statement : Box<Statement>,
}
impl WhileStatement {
    /// Creates a new while statement with the following condition and statement to execute.
    pub fn new(cond : Expr, statement : Statement) -> Self {
        WhileStatement{
            cond : cond,
            statement : Box::new(statement),
        }
    }
    /// Returns the condition to execute.
    pub fn condition(&self) -> &Expr {
        &self.cond
    }
    /// Returns the statement to execute.
    pub fn statement(&self) -> &Statement{
        &*self.statement
    }
}

/// The result of a statement.
#[derive(Debug)]
pub enum StatementResult {
    /// The statement does not return anything.
    Empty,
    /// The statement Is a break statement.
    Break,
    /// The statement was broken by a return.
    Return(LiteralExpr),
}

impl StatementResult {
    /// Checks if the statement will make the scope quit.
    pub fn is_quit(&self) -> bool {
        match self {
            &StatementResult::Empty => false,
            _ => true,
        }
    }
}