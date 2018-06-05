//! Contains the code for the parser,
//! currently only contains enough to parse expressions and return parse errors.
use expression::{Expr, Operator};
use statement::{
    Assignment, Declaration, FunctionDecl, IfStatement, Statement,
    WhileStatement,
};
use types::{TypedVar, LisaaType};
use std::collections::HashMap;
use std::fmt;
use token::{Token, TokenType};

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
    pub fn program(&mut self) -> Result<HashMap<String, FunctionDecl>, Vec<ParseError>> {
        let mut fails = vec![];
        let mut functions = HashMap::new();
        while !self.is_at_end() {
            match self.function_decl() {
                Ok(e) => {
                    let name = e.name().to_string();
                    functions.insert(name, e);
                }
                Err(e) => fails.push(ParseError::new(self.previous(), e)),
            }
            if fails.len() != 0 {
                return Err(fails);
            }
        }

        if fails.is_empty() {
            Ok(functions)
        } else {
            Err(fails)
        }
    }
    /// A new function declaration.
    pub fn function_decl(&mut self) -> Result<FunctionDecl, String> {
        match self.peek().get_type() {
            &TokenType::FUN => {
                self.advance(); // skip the func keyword
                let name = match self.check(&TokenType::IDENTIFIER) {
                    true => self.advance().get_lexeme().to_string(),
                    false => return Err("Expected identifier after function".to_string()),
                };
                let type_parameters = self.parse_type_list()?;
                let arguments = self.func_args()?;
                let return_type = self.func_return_type()?;
                let scope = self.scope()?;
                Ok(FunctionDecl::new(name, type_parameters, arguments, scope, return_type))
            }
            _ => Err("error : expected function declaration there".to_string()),
        }
    }

    pub fn parse_type_list(&mut self, ) -> Result<Vec<LisaaType>, String>{
        let mut args = vec![];
        match self.peek().is_type(&TokenType::LESS){
            true => {
                self.advance();
                loop {
                    match self.peek().get_type().clone() {
                        TokenType::GREATER => break,
                        TokenType::IDENTIFIER => {
                            args.push(self.parse_type()?);
                            match self.peek().get_type() {
                                TokenType::GREATER => break,
                                TokenType::COMMA => {
                                    self.advance();
                                }
                                _ => Err("expected > or comma".to_string())?,
                            }
                        }
                        other => Err(format!("Expected > or type parameter got {:?}", other))?,
                    }
                }
                self.expect(TokenType::GREATER)?;
                Ok(args)
            }
            false => Ok(vec![]),
        }
    }

    /// Parses the return type of a function.
    pub fn func_return_type(&mut self) -> Result<LisaaType, String> {
        if self.match_nexts(&[TokenType::ARROW]) {
            return Ok(self.parse_type()?);
        }
        return Ok(LisaaType::Void);
    }


    /// Parses the declaration of arguments
    /// Should be refactored a little bit tho.
    pub fn func_args(&mut self) -> Result<Vec<TypedVar>, String> {
        let mut args = vec![];
        self.expect(TokenType::LeftParen)?;
        loop {
            match self.peek().get_type() {
                &TokenType::RightParen => break,
                &TokenType::IDENTIFIER => {
                    args.push(self.typed_identifier()?);
                    match self.peek().get_type() {
                        &TokenType::RightParen => break,
                        &TokenType::COMMA => {
                            self.advance();
                        }
                        _ => Err("expected right paren or comma".to_string())?,
                    }
                }
                _ => Err("Expected closing paren or identifier".to_string())?,
            }
        }
        self.advance();
        Ok(args)
    }

    pub fn typed_identifier(&mut self) -> Result<TypedVar, String> {
        Ok(TypedVar::new(
            self.parse_type()?,
            self.advance().get_lexeme().to_string(),
        ))
    }

    pub fn parse_type(&mut self) -> Result<LisaaType, String> {
        let ident = self.advance().get_lexeme().to_string();
        match ident.as_ref() {
            "slice" => {
                self.expect(TokenType::LESS)?;
                let inner = self.parse_type()?;
                self.expect(TokenType::GREATER)?;
                Ok(LisaaType::slice(inner))
            }
            "num" => Ok(LisaaType::Num),
            "char" => Ok(LisaaType::Char),
            i => Ok(LisaaType::Unresolved(i.to_string())),
        }
    }

    /// Expects a semicolon after the statement, else break it.
    pub fn expect_semicolon(&mut self, statement: Statement) -> Result<Statement, String> {
        match self.match_nexts(&[TokenType::SEMICOLON]) {
            true => Ok(statement),
            false => Err("Expected semicolon at the end of statement".to_string()),
        }
    }
    /// parses a statement and waits for a semicolon at the end.
    pub fn statement(&mut self) -> Result<Statement, String> {
        self.break_statement()
    }
    /// try to parse a break statement.
    pub fn break_statement(&mut self) -> Result<Statement, String> {
        match self.peek().is_type(&TokenType::BREAK) {
            true => {
                self.advance();
                self.expect_semicolon(Statement::BreakStatement)
            }
            false => self.if_condition(),
        }
    }
    /// parses an if condition.
    pub fn if_condition(&mut self) -> Result<Statement, String> {
        match self.peek().is_type(&TokenType::IF) {
            true => self.parse_if(),
            false => self.while_loop(),
        }
    }
    /// parses an if condition.
    pub fn while_loop(&mut self) -> Result<Statement, String> {
        match self.peek().is_type(&TokenType::WHILE) {
            true => self.parse_while(),
            false => self.scope(),
        }
    }

    /// Parses all the statements in a scope.
    ///
    /// scopes have implicit semicolons, it will be added if it does not exists.
    pub fn parse_if(&mut self) -> Result<Statement, String> {
        self.advance();
        let condition = self.expression()?;
        let next_statement = self.statement()?;
        // add implicit semicolon.
        Ok(Statement::IfStatement(IfStatement::new(
            condition,
            next_statement,
        )))
    }

    /// Parses all the statements in a scope.
    ///
    /// scopes have implicit semicolons, it will be added if it does not exists.
    pub fn parse_while(&mut self) -> Result<Statement, String> {
        self.advance();
        let condition = self.expression()?;
        let next_statement = self.statement()?;
        // add implicit semicolon.
        Ok(Statement::WhileStatement(WhileStatement::new(
            condition,
            next_statement,
        )))
    }

    /// Parses a scope.
    pub fn scope(&mut self) -> Result<Statement, String> {
        match self.peek().is_type(&TokenType::LeftCurlyBrace) {
            true => self.parse_scope(),
            false => self.declaration(),
        }
    }
    /// Parses all the statements in a scope.
    ///
    /// scopes have implicit semicolons, it will be added if it does not exists.
    pub fn parse_scope(&mut self) -> Result<Statement, String> {
        self.advance();
        let mut statements = vec![];
        while !self.peek().is_type(&TokenType::RightCurlyBrace) {
            statements.push(self.statement()?);
            if self.is_at_end() {
                return Err("Expected closing brace at the end of scope".to_string());
            }
        }
        self.advance();
        Ok(Statement::Scope(statements))
    }

    /// Matches a declaration or an assignment.
    /// When it arrives at an identifier it checks for the next element :
    /// If it is a < we have a type.
    /// If it is an identifier we also have a type.
    /// In the two cases we parse an assignment.
    /// Else we go to the assignment part.
    pub fn declaration(&mut self) -> Result<Statement, String> {
        let decl = match self.peek().get_type() {
            &TokenType::IDENTIFIER => {
                if self.peek_twice().get_type() == &TokenType::LESS
                    || self.peek_twice().get_type() == &TokenType::IDENTIFIER
                {
                    let tp = self.parse_type()?;
                    return self.parse_declaration(tp);
                }
                let expr = self.expression()?;
                self.assignment(expr)
            }
            _ => self.return_statement(),
        };
        self.expect_semicolon(decl?)
    }
    /// Parses a declaration
    /// When this function is called we know that we have a type and the next val is an identifier.
    pub fn parse_declaration(&mut self, val_type: LisaaType) -> Result<Statement, String> {
        let ident = self.expression()?;
        let ass = self.parse_assignment(ident.clone())?; // no problem in cloning a small string.
        let decl = Ok(Statement::Declaration(Declaration::new(
            val_type,
            ident.get_identifier()?.to_string(),
            ass,
        )));
        self.expect(TokenType::SEMICOLON)?;
        decl
    }

    /// Parses a return statement
    pub fn return_statement(&mut self) -> Result<Statement, String> {
        match self.peek().get_type() {
            &TokenType::RETURN => {
                self.advance(); // skip the return.
                let expr = self.expression()?; // get what is to return
                Ok(Statement::ReturnStatement(expr))
            }
            _ => self.expr_statement(),
        }
    }

    /// Parses an assignment.
    /// An identifier followed by an identifier is a declaration.
    /// Followed by a equal is an assignment.
    /// Followed by a semicolon it is an expression.
    pub fn assignment(&mut self, ex: Expr) -> Result<Statement, String> {
        match self.peek().get_type() {
            &TokenType::EQUAL => Ok(Statement::Assignment(self.parse_assignment(ex)?)),
            &TokenType::SEMICOLON => Ok(Statement::ExprStatement(ex)),
            _ => Err(
                "expected Equals or end of declaration after expression declaration".to_string(),
            ),
        }
    }

    pub fn parse_assignment(&mut self, ex: Expr) -> Result<Assignment, String> {
        self.advance();
        Ok(Assignment::new(ex, self.expression()?))
    }

    /// Matches an expression statement, an expr ending with a semicolon.
    pub fn expr_statement(&mut self) -> Result<Statement, String> {
        Ok(Statement::ExprStatement(self.expression()?))
    }
    /// Parses an expression, the lowest level of precedence are && and ||.
    pub fn expression(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;
        while self.match_nexts(&[TokenType::ANDAND, TokenType::OROR]) {
            let previous = self.previous();
            let right = self.equality()?;
            let new_expr = Expr::binary(
                expr,
                Operator::from_token(&previous)?,
                right,
                previous.get_line(),
            );
            expr = new_expr;
        }
        Ok(expr)
    }
    /// Parses an equality by searching fot comparisons.
    pub fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;
        while self.match_nexts(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let previous = self.previous();
            let right = self.comparison()?;
            let new_expr = Expr::binary(
                expr,
                Operator::from_token(&previous)?,
                right,
                previous.get_line(),
            );
            expr = new_expr;
        }
        Ok(expr)
    }
    /// parses a comparison.
    pub fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.addition()?;
        while self.match_nexts(&[
            TokenType::GreaterEqual,
            TokenType::LessEqual,
            TokenType::GREATER,
            TokenType::LESS,
        ]) {
            let previous = self.previous();
            let right = self.addition()?;
            let new_expr = Expr::binary(
                expr,
                Operator::from_token(&previous)?,
                right,
                previous.get_line(),
            );
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
            let new_expr = Expr::binary(
                expr,
                Operator::from_token(&previous)?,
                right,
                previous.get_line(),
            );
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
            let new_expr = Expr::binary(
                expr,
                Operator::from_token(&previous)?,
                right,
                previous.get_line(),
            );
            expr = new_expr;
        }
        Ok(expr)
    }

    /// Parses an unary expression.
    pub fn unary(&mut self) -> Result<Expr, String> {
        if self.match_nexts(&[TokenType::MINUS, TokenType::BANG]) {
            let previous = self.previous();
            let right = self.unary()?;
            return Ok(Expr::unary(
                Operator::from_token(&previous)?,
                right,
                previous.get_line(),
            ));
        }
        let lit = self.literal()?;
        return self.post_notation(lit);
    }

    pub fn post_notation(&mut self, lit : Expr) -> Result<Expr, String>{
        let mut expr = lit;
        loop {
            expr = match self.peek().get_type() {
                &TokenType::LeftParen => self.parse_function_call(expr)?,
                &TokenType::LeftBrace => self.parse_indexing(expr)?,
                &TokenType::DOT => self.parse_getattr(expr)?,
                _ => {break;},
            }
        }
        Ok(expr)
    }

    /// Parse the function call but it is ugly and should be modified.
    pub fn parse_function_call(&mut self, lit: Expr) -> Result<Expr, String> {
        let mut args = vec![];
        self.expect(TokenType::LeftParen)?;
        loop {
            match self.peek().get_type() {
                &TokenType::RightParen => break,
                _ => {
                    args.push(self.expression()?);
                    match self.peek().get_type() {
                        &TokenType::RightParen => break,
                        &TokenType::COMMA => {
                            self.advance();
                        }
                        _ => Err("expected right paren or comma".to_string())?,
                    }
                }
            }
        }
        self.advance();
        self.callable(args, lit)
    }

    /// Returns a call with the given arguments, the call must be a method or a fucntion.
    pub fn callable(&mut self, args : Vec<Expr>, lit : Expr) -> Result<Expr, String>{
        let line = lit.get_line();
        if let Ok(id) = lit.get_identifier() {
            return Ok(Expr::function_call(
                lit.get_identifier().map_err(|_| "function calls only allowed on identifier")?
                    .to_string(),
                args,
                line,
            ))
        }
        Ok(Expr::method_call(lit, args, line))
    }


    pub fn parse_indexing(&mut self, lit: Expr) -> Result<Expr, String> {
        self.expect(TokenType::LeftBrace)?;
        let index = self.expression()?;
        self.expect(TokenType::RightBrace)?;
        Ok(Expr::deref(Expr::indexing(
            lit,
            index,
            self.previous().get_line(),
        )))
    }

    pub fn parse_getattr(&mut self, lit : Expr) -> Result<Expr, String>{
        self.expect(TokenType::DOT)?;
        let next = self.advance();
        let name = Expr::identifier(next.get_lexeme().to_owned(), next.get_line());
        Ok(Expr::getattr(lit, name, next.get_line()))
    }

    pub fn expect(&mut self, token_type: TokenType) -> Result<(), String> {
        match self.match_nexts(&[token_type.clone()]) {
            true => Ok(()),
            false => Err(format!("Expected token {:?}", token_type)),
        }
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
                &TokenType::NUMBER => Ok(Expr::number(
                    token.get_lexeme().parse::<f64>().unwrap(),
                    token.get_line(),
                )),
                &TokenType::STRING => Ok(Expr::string(
                    token.get_lexeme().to_string(),
                    token.get_line(),
                )),
                &TokenType::NIL => Err("nil no longer supported".to_string()),
                &TokenType::FALSE => Ok(Expr::number(0.0, token.get_line())),
                &TokenType::TRUE => Ok(Expr::number(1.0, token.get_line())),
                &TokenType::IDENTIFIER => Ok(Expr::identifier(
                    token.get_lexeme().to_string(),
                    token.get_line(),
                )),
                &TokenType::CHAR => Ok(Expr::char(token.get_lexeme().chars().next().unwrap(), token.get_line())),
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
    pub fn peek_twice(&self) -> &Token {
        &self.tokens[self.current + 1]
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
