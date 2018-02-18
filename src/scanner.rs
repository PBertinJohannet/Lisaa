//! Scanner here
//!
use keywords::KEYWORDS;
use token::{TokenType, Token};

pub struct Scanner {
    tokens: Vec<Token>,
    source: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}
impl Scanner {
    pub fn new(source: &String) -> Self {
        Scanner {
            tokens: vec![],
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }
    pub fn tokens(&mut self) -> Result<Vec<Token>, String> {
        while !self.is_at_end() {
            self.start = self.current;
            let token = self.scan_token()?;
            if !token.ignore() {
                self.tokens.push(token);
            }
        }
        Ok(self.tokens.clone())
    }

    pub fn error(&self, message: String) -> Result<Token, String> {
        Err(format!("[line : {}] Error : {}", self.line, message))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<Token, String> {
        return match self.advance() {
            '(' => Ok(self.token(TokenType::LeftParen, "")),
            ')' => Ok(self.token(TokenType::RightParen, "")),
            '{' => Ok(self.token(TokenType::LeftBrace, "")),
            '}' => Ok(self.token(TokenType::RightBrace, "")),
            ',' => Ok(self.token(TokenType::COMMA, "")),
            '.' => Ok(self.token(TokenType::DOT, "")),
            '-' => Ok(self.token(TokenType::MINUS, "")),
            '+' => Ok(self.token(TokenType::PLUS, "")),
            ';' => Ok(self.token(TokenType::SEMICOLON, "")),
            '*' => Ok(self.token(TokenType::STAR, "")),
            '!' => Ok(match self.match_next('=') {
                true => self.token(TokenType::BangEqual, ""),
                _ => self.token(TokenType::BANG, ""),
            }),
            '=' => Ok(match self.match_next('=') {
                true => self.token(TokenType::EqualEqual, ""),
                _ => self.token(TokenType::EQUAL, ""),
            }),
            '<' => Ok(match self.match_next('=') {
                true => self.token(TokenType::LessEqual, ""),
                _ => self.token(TokenType::LESS, ""),
            }),
            '>' => Ok(match self.match_next('=') {
                true => self.token(TokenType::GreaterEqual, ""),
                _ => self.token(TokenType::GREATER, ""),
            }),
            '/' => {
                if self.match_next('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(self.token(TokenType::IGNORE, ""))
                } else {
                    Ok(self.token(TokenType::SLASH, ""))
                }
            }
            '\n' => {
                self.line += 1;
                Ok(self.token(TokenType::IGNORE, ""))
            }
            ' ' => Ok(self.token(TokenType::IGNORE, "")),
            '\r' => Ok(self.token(TokenType::IGNORE, "")),
            '\t' => Ok(self.token(TokenType::IGNORE, "")),
            '"' => self.string(),
            '0' ... '9' => self.number(),
            'a' ... 'z' => self.identifier(),
            'A' ... 'Z' => self.identifier(),
            c => Err(format!("unexpected token : {}", c)),
        };
    }

    fn identifier(&mut self) -> Result<Token, String> {
        while self.peek().is_alphanumeric() && !self.is_at_end() {
            self.advance();
        }
        let sub_string : String= self.source[self.start..self.current].into_iter().collect();
        match KEYWORDS.get::<str>(&sub_string) {
            Some(k) => Ok(self.token(k.clone(), "")),
            None => Ok(self.token(TokenType::IDENTIFIER, "")),
        }
    }

    fn string(&mut self) -> Result<Token, String> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            self.error("Unterminated string sequence".to_string())
        } else {
            let sub_string : String= self.source[self.start + 1..self.current - 1].into_iter().collect();
            self.advance();
            Ok(self.token(TokenType::STRING, &sub_string))
        }
    }

    fn number(&mut self) -> Result<Token, String> {
        while self.peek().is_numeric() && !self.is_at_end() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();
            while self.peek().is_numeric() && !self.is_at_end() {
                self.advance();
            }
        }
        let sub_string : String= self.source[self.start..self.current].into_iter().collect();
        Ok(self.token(TokenType::NUMBER, &sub_string))
    }


    fn peek_next(&mut self) -> char {
        if self.current+1 == self.source.len() {
            '\0'
        } else {
            self.source[self.current+1]
        }
    }

    fn peek(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn match_next(&mut self, expect: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.source[self.current] == expect {
            self.current += 1;
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn token(&mut self, token: TokenType, lexeme: &str) -> Token {
        Token::new(self.line, token, lexeme.to_string())
    }
}
