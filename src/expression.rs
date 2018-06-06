//! The module containing the code for the different expressions.
//!

use std::fmt;
use token::{Token, TokenType};
use types::LisaaType;

#[derive(Debug, Clone)]
/// The base for an expression.
pub enum ExprEnum {
    /// The unary expressions, see below.
    Unary(UnaryExpr),
    /// The binary expressions, see below.
    GetAttr(BinaryExpr),
    /// The binary expressions, see below.
    Binary(BinaryExpr),
    /// The literals, see below.
    Literal(LiteralExpr),
    /// an identifier
    Identifier(String),
    /// A function call
    /// Dynamic calls are not allowed so we have either function call either method calls.
    /// This represents a call for a method from an expression :
    /// (1+1).add(2) for example.
    FunctionCall(FunctionCall),
    /// A dereferenced value contains a
    Deref(Deref),
}
/// Expressions that are lvalues :
/// Identifier
/// Expressions that are not lvalues
/// Literal
/// For other expressions Lvalues are completed in the typecheck for the binary/identifier
#[derive(Debug, Clone)]
pub struct Expr {
    expr: ExprEnum,
    return_type: Option<LisaaType>,
    line: usize,
}

impl Expr {
    pub fn deref(inner: Expr) -> Self {
        let line = inner.get_line();
        Expr {
            expr: ExprEnum::Deref(Deref::new(inner)),
            return_type: None,
            line: line,
        }
    }
    pub fn getattr(lhs: Expr, rhs: Expr, line: usize) -> Self {
        Expr {
            expr: ExprEnum::GetAttr(BinaryExpr::new(lhs, Operator::Get, rhs)),
            return_type: None,
            line: line,
        }
    }
    pub fn binary(lhs: Expr, operator: Operator, rhs: Expr, line: usize) -> Self {
        Expr {
            expr: ExprEnum::Binary(BinaryExpr::new(lhs, operator, rhs)),
            return_type: None,
            line: line,
        }
    }
    pub fn indexing(indexed: Expr, index: Expr, line: usize) -> Self {
        Expr {
            expr: ExprEnum::Binary(BinaryExpr::new(indexed, Operator::INDEX, index)),
            return_type: None,
            line: line,
        }
    }
    pub fn is_identifier(&self) -> bool {
        match self.expr {
            ExprEnum::Identifier(_) => true,
            _ => false,
        }
    }
    pub fn is_deref(&self) -> bool {
        if let ExprEnum::Deref(_) = self.expr() {
            return true;
        }
        return false;
    }
    pub fn expr(&self) -> &ExprEnum {
        &self.expr
    }
    pub fn expr_mut(&mut self) -> &mut ExprEnum {
        &mut self.expr
    }
    pub fn set_type(&mut self, ret_type: LisaaType) {
        self.return_type = Some(ret_type);
    }
    pub fn return_type(&self) -> LisaaType {
        self.return_type.clone().unwrap()
    }
    pub fn return_type_uncheck(&self) -> &Option<LisaaType> {
        &self.return_type
    }
    pub fn method_call(expr: Expr, args: Vec<Expr>, line: usize) -> Self {
        Expr {
            expr: ExprEnum::FunctionCall(FunctionCall::method(expr, args)),
            return_type: None,
            line: line,
        }
    }
    pub fn function_call(name: String, args: Vec<Expr>, line: usize) -> Self {
        Expr {
            expr: ExprEnum::FunctionCall(FunctionCall::function(name, args)),
            return_type: None,
            line: line,
        }
    }
    pub fn unary(operator: Operator, expr: Expr, line: usize) -> Self {
        Expr {
            expr: ExprEnum::Unary(UnaryExpr::new(operator, expr)),
            return_type: None,
            line: line,
        }
    }
    pub fn char(ch: char, line: usize) -> Self {
        Expr {
            expr: ExprEnum::Literal(LiteralExpr::CHAR(ch)),
            return_type: Some(LisaaType::Char),
            line: line,
        }
    }
    pub fn number(num: f64, line: usize) -> Self {
        Expr {
            expr: ExprEnum::Literal(LiteralExpr::NUMBER(num)),
            return_type: Some(LisaaType::Num),
            line: line,
        }
    }
    pub fn string(string: String, line: usize) -> Self {
        Expr {
            expr: ExprEnum::Literal(LiteralExpr::STRING(string)),
            return_type: Some(LisaaType::slice(LisaaType::Char)),
            line: line,
        }
    }
    pub fn identifier(string: String, line: usize) -> Self {
        Expr {
            expr: ExprEnum::Identifier(string),
            return_type: None,
            line: line,
        }
    }
    pub fn get_identifier(&self) -> Result<&String, String> {
        match self.expr {
            ExprEnum::Identifier(ref i) => Ok(i),
            _ => Err("expected identifier".to_string()),
        }
    }
    pub fn get_line(&self) -> usize {
        self.line
    }
}

/// Represents a dereference
/// needs to know if it is assigned something to.
#[derive(Debug, Clone)]
pub struct Deref {
    expr: Box<Expr>,
    assigned: bool,
}
impl Deref {
    pub fn new(expr: Expr) -> Self {
        Deref {
            expr: Box::new(expr),
            assigned: false,
        }
    }
    pub fn inner_mut(&mut self) -> &mut Expr {
        &mut self.expr
    }
    pub fn inner(&self) -> &Expr {
        &self.expr
    }
    pub fn is_assigned(&self) -> bool {
        self.assigned
    }
    pub fn set_assigned(&mut self) {
        self.assigned = true;
    }
}

/// Represents a function call in the code.
#[derive(Debug, Clone)]
pub enum Callee {
    StaticFunc(String),
    Method(Box<Expr>),
}

impl Callee {
    pub fn get_method(&self) -> Option<&Expr>{
        match self {
            &Callee::StaticFunc(_) => None,
            &Callee::Method(ref e) => match e.expr() {
                ExprEnum::GetAttr(b) => Some(b.lhs()),
                _ => panic!("method without getattr ? wtf"),
            },
        }
    }
}

/// Represents a function call in the code.
#[derive(Debug, Clone)]
pub struct  FunctionCall {
    callee: Callee,
    name: String,
    args: Vec<Expr>,
}

impl FunctionCall {
    pub fn method(lhs: Expr, args: Vec<Expr>) -> Self {
        FunctionCall {
            callee: Callee::Method(Box::new(lhs)),
            args: args,
            name: "method".to_owned(),
        }
    }
    /// Creates a new function call expression
    pub fn function(name: String, args: Vec<Expr>) -> Self {
        FunctionCall {
            callee: Callee::StaticFunc(name.clone()),
            args: args,
            name: name,
        }
    }
    pub fn is_method(&self) -> bool {
        match self.callee {
            Callee::StaticFunc(_) => false,
            _ => true,
        }
    }
    pub fn callee(&self) -> &Callee {
        &self.callee
    }
    pub fn callee_mut(&mut self) -> &mut Callee {
        &mut self.callee
    }

    /// Returns the name.
    pub fn set_name(&mut self, name: &String) {
        self.name = name.to_owned();
    }
    /// Returns the name.
    pub fn name(&self) -> &String {
        &self.name
    }
    /// Returns the list of arguments.
    pub fn args(&self) -> &Vec<Expr> {
        &self.args
    }
    /// Returns the list of arguments.
    pub fn args_mut(&mut self) -> &mut Vec<Expr> {
        &mut self.args
    }
}

/// These are the operators
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Operator {
    Not,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    GreaterEqual,
    GREATER,
    LessEqual,
    LESS,
    EqualEqual,
    NotEqual,
    AndAnd,
    INDEX,
    Get,
}

impl Operator {
    pub fn from_token(token: &Token) -> Result<Self, String> {
        match token.get_type() {
            &TokenType::BANG => Ok(Operator::Not),
            &TokenType::PLUS => Ok(Operator::PLUS),
            &TokenType::MINUS => Ok(Operator::MINUS),
            &TokenType::STAR => Ok(Operator::STAR),
            &TokenType::SLASH => Ok(Operator::SLASH),
            &TokenType::GreaterEqual => Ok(Operator::GreaterEqual),
            &TokenType::GREATER => Ok(Operator::GREATER),
            &TokenType::LessEqual => Ok(Operator::LessEqual),
            &TokenType::LESS => Ok(Operator::LESS),
            &TokenType::EqualEqual => Ok(Operator::EqualEqual),
            &TokenType::BangEqual => Ok(Operator::NotEqual),
            &TokenType::ANDAND => Ok(Operator::AndAnd),
            _ => Err(format!(
                "can not convert token : {:?} to operator",
                token.get_type()
            )),
        }
    }
}

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
    pub operator: Operator,
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
    pub fn lhs(&self) -> &Expr {
        &*self.lhs
    }
    /// Returns the riht hand side of the expression.
    pub fn rhs(&self) -> &Expr {
        &*self.rhs
    }
    /// returns the left hand side of the expression as mutable ref.
    pub fn lhs_mut(&mut self) -> &mut Expr {
        &mut *self.lhs
    }
    /// Returns the riht hand side of the expression as mutable ref.
    pub fn rhs_mut(&mut self) -> &mut Expr {
        &mut *self.rhs
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
    /// A slice of chars is a string.
    STRING(String),
    /// A char
    CHAR(char),
}

impl fmt::Display for LiteralExpr {
    /// Formats the error to a user readable format.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LiteralExpr::NUMBER(n) => write!(f, "{}", n),
            &LiteralExpr::STRING(ref s) => write!(f, "{}", s),
            &LiteralExpr::CHAR(ref c) => write!(f, "{}", c),
        }
    }
}
