//! The module for statement.
use expression::Expr;
use token::Token;

#[derive(Debug)]
/// The enum for statements.
pub enum Statement {
    /// an expression followed by a semicolon.
    ExprStatement(Expr),
    /// An assignment.
    Assignment(Assignment),
    /// A declaration.
    Declaration(Declaration),
}


#[derive(Debug)]
/// An assignment is an identifier plus an expression.
pub struct Assignment {
    identifier : Token,
    expr : Expr,
}

impl Assignment {
    /// Creates a new assignment.
    pub fn new(identifier : Token, expr : Expr) -> Self{
        Assignment {
            identifier : identifier,
            expr : expr,
        }
    }
    /// Returns the identifier.
    pub fn identifier(&self) -> &Token {
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
    val_type : Token,
    identifier : Token,
    expr : Option<Expr>,
}

impl Declaration {
    /// Creates a new declaration.
    pub fn new(val_type : Token, identifier : Token, expr : Option<Expr>) -> Self {
        Declaration {
            val_type : val_type,
            identifier : identifier,
            expr : expr,
        }
    }
}
