//! The module for statement.
use expression::{Expr, LiteralExpr};
use types::{LisaaType, Class, TypedVar};
/// A function declaration
#[derive(Debug, Clone)]
pub struct FunctionDecl {
    name: String,
    type_args : Vec<LisaaType>,
    args: Vec<TypedVar>,
    scope: Statement,
    ret_type: LisaaType,
}

impl FunctionDecl {
    /// Creates a new function declaration.
    pub fn new(name: String, type_args : Vec<LisaaType>, args: Vec<TypedVar>, scope: Statement, ret_type: LisaaType) -> Self {
        FunctionDecl {
            name: name,
            type_args : type_args,
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

impl StatementResult {
    /// Checks if the statement will make the scope quit.
    pub fn is_quit(&self) -> bool {
        match self {
            &StatementResult::Empty => false,
            _ => true,
        }
    }
}
