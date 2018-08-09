//! The module containing the code for the different expressions.
//!

use statement::FunctionSig;
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
    pub fn as_assigned_to(&self) -> Self {
        let mut new = self.clone();
        if let ExprEnum::Deref(ref mut d) = new.expr_mut() {
            d.set_assigned();
        }
        new
    }
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
    /// Now the binary expression turn into a call of a getattr.
    ///
    pub fn binary(lhs: Expr, operator: Operator, rhs: Expr, line: usize) -> Self {
        let inner = Expr::getattr(lhs, Expr::identifier(format!("{}", operator), line), line);
        Expr::method_call(inner, vec![rhs], line)
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
    pub fn constructor_call(func: FunctionCall, line: usize) -> Self {
        Expr {
            expr: ExprEnum::FunctionCall(func),
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
            return_type: Some(LisaaType::Class("String".to_owned(), vec![])),
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
    pub fn get_caller_type(&self) -> Option<LisaaType>{
        match self {
            &Callee::StaticFunc(_) => None,
            &Callee::Method(ref e) => {
                if let (LisaaType::Function(box t, _, _), _) = e.return_type().max_deref(){
                    Some(t)
                } else {
                    panic!("calling a function on a non function type")
                }
            },
        }
    }

    pub fn get_method(&self) -> Option<&Expr> {
        match self {
            &Callee::StaticFunc(_) => None,
            &Callee::Method(ref e) => match e.expr() {
                ExprEnum::GetAttr(b) => Some(b.lhs()),
                ExprEnum::Deref(deref) => match deref.expr.expr() {
                    ExprEnum::GetAttr(attr) => Some(attr.lhs()),
                    _ => panic!(format!("method without getattr ? {:?}", e)),
                },
                _ => panic!(format!("method without getattr ? {:?}", e)),
            },
        }
    }
}

/// Represents a function call in the code.
#[derive(Debug, Clone)]
pub struct FunctionCall {
    callee: Callee,
    name: String,
    args: Vec<Expr>,
    signature: Option<FunctionSig>,
    /// Only if it is a constructor.
    type_args: Vec<LisaaType>,
}

impl FunctionCall {
    pub fn method(lhs: Expr, args: Vec<Expr>) -> Self {
        FunctionCall {
            callee: Callee::Method(Box::new(lhs)),
            args: args,
            name: "method".to_owned(),
            signature: None,
            type_args: vec![],
        }
    }
    /// Creates a new function call expression
    pub fn function(name: String, args: Vec<Expr>) -> Self {
        FunctionCall {
            callee: Callee::StaticFunc(name.clone()),
            args: args,
            name: name,
            signature: None,
            type_args: vec![],
        }
    }
    /// Creates a new function call expression
    pub fn constructor(name: String, args: Vec<Expr>, type_args: Vec<LisaaType>) -> Self {
        FunctionCall {
            callee: Callee::StaticFunc(name.clone()),
            args: args,
            name: name,
            signature: None,
            type_args: type_args,
        }
    }
    pub fn signature(&self) -> FunctionSig {
        self.signature
            .clone()
            .expect("cant get signature of non typechecked function!")
    }
    pub fn signature_mut(&mut self) -> &mut FunctionSig {
        if let Some(ref mut s) = self.signature {
            return s;
        } else {
            panic!("cant get signature of non typechecked function!")
        }
    }
    pub fn set_signature(&mut self, sig: FunctionSig) {
        self.signature = Some(sig);
    }
    pub fn callee(&self) -> &Callee {
        &self.callee
    }
    pub fn callee_mut(&mut self) -> &mut Callee {
        &mut self.callee
    }
    pub fn type_args(&self) -> &Vec<LisaaType> {
        &self.type_args
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
    MOD,
    GreaterEqual,
    GREATER,
    LessEqual,
    LESS,
    EqualEqual,
    NotEqual,
    AndAnd,
    OrOr,
    Or,
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
            &TokenType::MOD => Ok(Operator::MOD),
            &TokenType::GreaterEqual => Ok(Operator::GreaterEqual),
            &TokenType::GREATER => Ok(Operator::GREATER),
            &TokenType::LessEqual => Ok(Operator::LessEqual),
            &TokenType::LESS => Ok(Operator::LESS),
            &TokenType::EqualEqual => Ok(Operator::EqualEqual),
            &TokenType::BangEqual => Ok(Operator::NotEqual),
            &TokenType::ANDAND => Ok(Operator::AndAnd),
            &TokenType::OROR => Ok(Operator::OrOr),
            &TokenType::OR => Ok(Operator::Or),
            _ => Err(format!(
                "can not convert token : {:?} to operator",
                token.get_type()
            )),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Operator::Not => write!(f, "not"),
            &Operator::PLUS => write!(f, "add"),
            &Operator::MINUS => write!(f, "minus"),
            &Operator::STAR => write!(f, "times"),
            &Operator::MOD => write!(f, "modulo"),
            &Operator::SLASH => write!(f, "divide"),
            &Operator::GreaterEqual => write!(f, "ge"),
            &Operator::GREATER => write!(f, "greater"),
            &Operator::LessEqual => write!(f, "le"),
            &Operator::LESS => write!(f, "less"),
            &Operator::EqualEqual => write!(f, "equals"),
            &Operator::NotEqual => write!(f, "ne"),
            &Operator::AndAnd => write!(f, "andand"),
            &Operator::OrOr => write!(f, "oror"),
            &Operator::Or => write!(f, "or"),
            &Operator::INDEX => write!(f, "index"),
            &Operator::Get => write!(f, "get"),
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
    #[allow(dead_code)]
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
