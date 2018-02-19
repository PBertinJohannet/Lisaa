//! Contains the code for the parser,
//! currently only contains enough to parse expressions and return parse errors.
use token::{TokenType, Token};
use expression::{BinaryExpr, UnaryExpr, LiteralExpr};
use expression::Expr;
use std::fmt;
use statement::{Statement, Declaration, Assignment};

#[derive(Debug)]
/// The struct for a parse error, contains just enough information to show
/// the user what happend and where.
pub struct ParseError {
    token: Token,
    message: String,
}

impl ParseError {
    /// Creates a new parse error.
    pub fn new(token: Token, message: String) -> Self {
        ParseError {
            token: token,
            message: message,
        }
    }
}


impl fmt::Display for ParseError {
    /// Formats the error to a user readable format.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Error {} : at {:?}:{} line {}",
            self.message,
            self.token.get_type(),
            self.token.get_lexeme(),
            self.token.get_line()
        )
    }
}

/// The parser, contains the tokens and a cursor.
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Creates a new parser from the given tokens.
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens,
            current: 0,
        }
    }
    /// parses the given list of token
    ///
    /// If no error occurs, we return the list of the parsed expression.
    ///
    /// If an error occurs, we continue to parse, looking for other errors and return all of them
    /// When an error happens in an expression, we leave the expression to avoid cascading errors.
    ///
    /// But we start again with the new expressions.
    pub fn program(&mut self) -> Result<Vec<Statement>, Vec<ParseError>> {
        let mut fails = vec![];
        let mut expressions = vec![];
        while !self.is_at_end() {
            match self.declaration() {
                Ok(e) => expressions.push(e),
                Err(e) => fails.push(ParseError::new(self.previous(), e)),
            }
            if fails.len() > 5 {
                return Err(fails);
            }
        }

        if fails.is_empty() {
            Ok(expressions)
        } else {
            Err(fails)
        }
    }
    /// Matches a declaration or an assignment.
    pub fn declaration(&mut self) -> Result<Statement, String> {
        match self.peek().get_type() {
            &TokenType::IDENTIFIER => self.assignment(),
            _ => self.expr_statement(),
        }
    }
    /// Parses an assignment.
    /// should probably parse
    pub fn assignment(&mut self) -> Result<Statement, String> {
        let ident = self.advance();
        match self.match_nexts(&[TokenType::EQUAL]){
            true => {
                let res = Ok(Statement::Assignment(Assignment::new(ident, self.expression()?)));
                match self.match_nexts(&[TokenType::SEMICOLON]) {
                    true => res,
                    false => Err("Expected semicolon at the end of expression".to_string()),
                }
            },
            false => Err("expected Equals after string declaration".to_string()),
        }
    }
    /// Matches an expression statement, an expr ending with a semicolon.
    pub fn expr_statement(&mut self) -> Result<Statement, String>{
        let expr = self.expression()?;
        match self.match_nexts(&[TokenType::SEMICOLON]) {
            true => {
                self.advance();
                Ok(Statement::ExprStatement(expr))
            },
            false => Err("Expected semicolon at the end of expression".to_string()),
        }
    }
    /// Parses an expression, the lowest level of precedence is equality.
    pub fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }
    /// Parses an equality by searching fot comparisons.
    pub fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;
        while self.match_nexts(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let previous = self.previous();
            let right = self.comparison()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }
    /// parses a comparison.
    pub fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.addition()?;
        while self.match_nexts(
            &[
                TokenType::GreaterEqual,
                TokenType::LessEqual,
                TokenType::GREATER,
                TokenType::LESS,
            ],
        )
        {
            let previous = self.previous();
            let right = self.addition()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }
    /// Parses an adition.
    pub fn addition(&mut self) -> Result<Expr, String> {
        let mut expr = self.multiplication()?;
        while self.match_nexts(&[TokenType::MINUS, TokenType::PLUS]) {
            let previous = self.previous();
            let right = self.multiplication()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }

    /// Parses a multiplication.
    pub fn multiplication(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;
        while self.match_nexts(&[TokenType::STAR, TokenType::SLASH]) {
            let previous = self.previous();
            let right = self.unary()?;
            let new_expr = Expr::Binary(BinaryExpr::new(expr, previous.clone(), right));
            expr = new_expr;
        }
        Ok(expr)
    }

    /// Parses an unary expression.
    pub fn unary(&mut self) -> Result<Expr, String> {
        if self.match_nexts(&[TokenType::MINUS, TokenType::BANG]) {
            let previous = self.previous();
            println!("prev : {:?} ask unary", previous);
            let right = self.unary()?;
            return Ok(Expr::Unary(UnaryExpr::new(previous.clone(), right)));
        }
        return self.literal();
    }

    /// Parses a litteral expression.
    pub fn literal(&mut self) -> Result<Expr, String> {
        if self.match_nexts(&[TokenType::LeftParen]) {
            let expr = self.expression();
            match self.peek().is_type(&TokenType::RightParen) {
                true => {
                    self.advance();
                    expr
                }
                false => Err("Expected right parenthesis".to_string()),
            }
        } else {
            let token = self.advance();
            match token.get_type() {
                &TokenType::NUMBER => Ok(Expr::Literal(LiteralExpr::NUMBER(
                    token.get_lexeme().parse::<f64>().unwrap(),
                ))),
                &TokenType::STRING => Ok(Expr::Literal(
                    LiteralExpr::STRING(token.get_lexeme().to_string()),
                )),
                &TokenType::NIL => Ok(Expr::Literal(LiteralExpr::NUMBER(0.0))),
                &TokenType::FALSE => Ok(Expr::Literal(LiteralExpr::NUMBER(0.0))),
                &TokenType::TRUE => Ok(Expr::Literal(LiteralExpr::NUMBER(1.0))),
                _ => Err("Cant parse literal".to_string()),
            }
        }
    }
    /// Given a list of token types, matches one of them if possible and consume it
    /// If no matches were found, do nothing.
    pub fn match_nexts(&mut self, tokens: &[TokenType]) -> bool {
        for tp in tokens.iter() {
            if self.check(tp) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    /// Checks that the next token is of the given type.
    pub fn check(&mut self, token_type: &TokenType) -> bool {
        !(self.is_at_end() || !self.peek().is_type(token_type))

    }

    /// Peeks for the next token, without conduming it.
    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    /// Advance and consume the next token, returning it
    pub fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().clone()
    }

    /// Returns the previous token.
    pub fn previous(&self) -> Token {
        if self.current == 0 {
            self.tokens[0].clone()
        } else {
            self.tokens[self.current - 1].clone()
        }
    }

    /// Checks if we are at the end of the file.
    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
}
