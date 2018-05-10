//! The module containing the code for the different expressions.
//!

use token::Token;
use std::fmt;

#[derive(Debug, Clone)]
/// The base for an expression.
pub enum ExprEnum {
    /// The unary expressions, see below.
    Unary(UnaryExpr),
    /// The binary expressions, see below.
    Binary(BinaryExpr),
    /// The literals, see below.
    Literal(LiteralExpr),
    /// an identifier
    Identifier(String),
    /// A function call
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone)]
pub struct Expr {
    expr : ExprEnum,
    return_type : String,
}


impl Expr {
   pub fn binary(lhs: Expr, operator: Operator, rhs: Expr) -> Self {
        Expr {
            expr : ExprEnum::Binary(BinaryExpr::new(lhs, operator, rhs)),
            return_type : "var".to_string()
        }
    }
    pub fn expr(&self) -> &ExprEnum {
        &self.expr
    }
    pub fn expr_mut(&mut self) -> &mut ExprEnum {
        &mut self.expr
    }
    pub fn set_type(&mut self, ret_type : String){
        self.return_type = ret_type;
    }
    pub fn return_type(&self) -> &String {
        &self.return_type
    }
    pub fn function_call(name : String, args : Vec<Expr>) -> Self {
        Expr {
            expr : ExprEnum::FunctionCall(FunctionCall::new(name, args)),
            return_type : "var".to_string()
        }
    }
    pub fn unary(operator: Operator, expr: Expr) -> Self {
        Expr {
            expr : ExprEnum::Unary(UnaryExpr::new(operator, expr)),
            return_type : "var".to_string()
        }
    }
    pub fn number(num : f64) -> Self {
        Expr {
            expr : ExprEnum::Literal(LiteralExpr::NUMBER(num)),
            return_type : "num".to_string()
        }
    }
    pub fn string(string : String) -> Self {
        Expr {
            expr : ExprEnum::Literal(LiteralExpr::STRING(string)),
            return_type : "str".to_string()
        }
    }
    pub fn identifier(string : String) -> Self {
        Expr {
            expr : ExprEnum::Identifier(string),
            return_type : "var".to_string()
        }
    }
    pub fn get_identifier(&self) -> Result<&String, String> {
        match self.expr {
            ExprEnum::Identifier(ref i) =>Ok(i),
            _ => Err("expected identifier".to_string())
        }
    }
}

/// Represents a function call in the code.
#[derive(Debug, Clone)]
pub struct FunctionCall {
    name : String,
    args : Vec<Expr>
}

impl FunctionCall {
    /// Creates a new function call expression
    pub fn new(name : String, args : Vec<Expr>) -> Self {
        FunctionCall {
            name : name,
            args : args,
        }
    }
    /// Returns the name.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the list of arguments.
    pub fn args(&self) -> &Vec<Expr>{
        &self.args
    }
}

/// Operators are represented by tokens for now.
pub type Operator = Token;

#[derive(Debug, Clone)]
/// An unary expression contains only an operator and the expression.
pub struct UnaryExpr {
    operator: Operator,
    expr: Box<Expr>,
}
impl UnaryExpr {
    /// Creates a new unary expression.
    pub fn new(operator: Operator, expr: Expr) -> Self {
        UnaryExpr {
            operator: operator,
            expr: Box::new(expr),
        }
    }
    /// Returns the expression.
    pub fn expression(&self) -> &Expr {
        &*self.expr
    }
    /// Returns the expression.
    pub fn expression_mut(&mut self) -> &mut Expr {
        &mut *self.expr
    }
    /// Returns the operator.
    pub fn operator(&self) -> Operator {
        self.operator.clone()
    }
}
#[derive(Debug, Clone)]
/// An unary expression contains an operator and two expressions.
pub struct BinaryExpr {
    lhs: Box<Expr>,
    operator: Operator,
    rhs: Box<Expr>,
}
impl BinaryExpr {
    /// Creates a new binary expression.
    pub fn new(lhs: Expr, operator: Operator, rhs: Expr) -> Self {
        BinaryExpr {
            lhs: Box::new(lhs),
            operator: operator,
            rhs: Box::new(rhs),
        }
    }
    /// returns the left hand side of the expression.
    pub fn lhs(&self)  -> &Expr {
        &*self.lhs
    }
    /// Returns the riht hand side of the expression.
    pub fn rhs(&self)  -> &Expr {
        &*self.rhs
    }
    /// Returns the operator.
    pub fn operator(&self) -> Operator {
        self.operator.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
/// An literal expression is a string/number literal.
pub enum LiteralExpr {
    /// Anything from bool to null will be a number
    NUMBER(f64),
    /// Anything else will be a string.
    STRING(String),
}


impl fmt::Display for LiteralExpr {
    /// Formats the error to a user readable format.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LiteralExpr::NUMBER(n) => write!(f, "{}", n),
            &LiteralExpr::STRING(ref s) => write!(f, "{}", s),
        }
    }
}