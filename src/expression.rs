use token::{Token, TokenType};
#[derive(Debug)]
pub enum Expr{
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
}

pub type Operator = Token;
#[derive(Debug)]
pub struct UnaryExpr {
    operator : Operator,
    expr : Box<Expr>,
}
impl UnaryExpr {
    pub fn new(operator : Operator, expr : Expr ) -> Self {
        UnaryExpr {
            operator : operator,
            expr : Box::new(expr),
        }
    }
}
#[derive(Debug)]
pub struct BinaryExpr {
    lhs : Box<Expr>,
    operator : Operator,
    rhs : Box<Expr>,
}
impl BinaryExpr {
    pub fn new(lhs : Expr, operator : Operator, rhs : Expr ) -> Self {
        BinaryExpr {
            lhs : Box::new(lhs),
            operator : operator,
            rhs : Box::new(rhs),
        }
    }
}

#[derive(Debug)]
pub struct GroupingExpr {
    exprs : Vec<Expr>
}
#[derive(Debug)]
pub enum LiteralExpr {
    NUMBER(f64),
    STRING(String),
}

impl LiteralExpr {
}
