use token::{TokenType, Token};
use expression::{BinaryExpr, UnaryExpr, GroupingExpr, LiteralExpr};
use expression::Expr;
use std::fmt;

#[derive(Debug)]
pub struct ParseError {
    token : Token,
    message : String,
}

impl ParseError {
    pub fn new(token : Token, message : String) -> Self {
        ParseError {
            token : token,
            message : message,
        }
    }
}


impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error {} : at {:?}:{} line {}", self.message, self.token.get_type(), self.token.get_lexeme(), self.token.get_line())
    }
}

pub struct Parser {
    tokens : Vec<Token>,
    current : usize
}

impl Parser {
    pub fn new(tokens : Vec<Token>) -> Self {
        Parser {
            tokens :tokens,
            current : 0,
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Expr>, Vec<ParseError>>{
        let mut fails = vec![];
        let mut expressions = vec![];
        while !self.is_at_end(){
            match self.expression() {
                Ok(e) => expressions.push(e),
                Err(e) => fails.push(ParseError::new(self.previous(), e)),
            }
        }
        if fails.is_empty(){
            Ok(expressions)
        } else {
            Err(fails)
        }
    }

    pub fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }
    pub fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;
        while self.match_nexts(&[TokenType::EqualEqual, TokenType::BangEqual]){
            let previous = self.previous();
            let right = self.comparison()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }
    pub fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.addition()?;
        while self.match_nexts(&[TokenType::GreaterEqual, TokenType::LessEqual, TokenType::GREATER, TokenType::LESS]){
            let previous = self.previous();
            let right = self.addition()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }
    pub fn addition(&mut self) -> Result<Expr, String> {
        let mut expr = self.multiplication()?;
        while self.match_nexts(&[TokenType::MINUS, TokenType::PLUS]){
            let previous = self.previous();
            let right = self.multiplication()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }
    pub fn multiplication(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;
        while self.match_nexts(&[TokenType::STAR, TokenType::SLASH]){
            let previous = self.previous();
            let right = self.unary()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }
    pub fn unary(&mut self)  -> Result<Expr, String> {
        if self.match_nexts(&[TokenType::MINUS, TokenType::PLUS]){
            let previous = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(UnaryExpr::new(previous.clone(), right)));
        }
        return self.literal()
    }

    pub fn literal(&mut self) -> Result<Expr, String> {
        if self.match_nexts(&[TokenType::LeftParen]) {
            let mut expr = self.expression();
            match self.peek().is_type(&TokenType::RightParen){
                true => {self.advance();expr},
                false => Err("Expected right parenthesis".to_string()),
            }
        } else {
            let token = self.advance();
            match token.get_type(){
                &TokenType::NUMBER => Ok(Expr::Literal(LiteralExpr::NUMBER(token.get_lexeme().parse::<f64>().unwrap()))),
                &TokenType::STRING => Ok(Expr::Literal(LiteralExpr::STRING(token.get_lexeme().to_string()))),
                &TokenType::NIL => Ok(Expr::Literal(LiteralExpr::NUMBER(0.0))),
                &TokenType::FALSE => Ok(Expr::Literal(LiteralExpr::NUMBER(0.0))),
                &TokenType::TRUE => Ok(Expr::Literal(LiteralExpr::NUMBER(1.0))),
                _ => Err("Cant parse literal".to_string())
            }
        }
    }

    pub fn match_nexts(&mut self, tokens : &[TokenType]) -> bool {
        for tp in tokens.iter() {
            if self.check(tp) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    pub fn check(&mut self, token_type : &TokenType) -> bool {
        !(self.is_at_end() || !self.peek().is_type(token_type))

    }

    pub fn peek(&self) -> &Token{
        &self.tokens[self.current]
    }

    pub fn advance(&mut self) -> Token{
        if !self.is_at_end() {
            self.current+=1;
        }
        self.previous().clone()
    }

    pub fn previous(&self) -> Token {
        self.tokens[self.current-1].clone()
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
}
