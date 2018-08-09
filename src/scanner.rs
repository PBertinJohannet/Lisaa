//! Scanner here
//!
use keywords::KEYWORDS;
use token::{Token, TokenType};
/// The scanner, will scan the tokens as expected.
/// Does not report the position of the tokens in the lines but will soon.
pub struct Scanner {
    tokens: Vec<Token>,
    source: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}
impl Scanner {
    /// Creates a new scanner for the given source code.
    pub fn new(source: String) -> Self {
        Scanner {
            tokens: vec![],
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    /// Returns the list of tokens contained in the code.
    pub fn tokens(&mut self) -> Result<Vec<Token>, String> {
        while !self.is_at_end() {
            self.start = self.current;
            let token = self.scan_token()?;
            if !token.ignore() {
                self.tokens.push(token);
            }
        }
        let _end = self.token(TokenType::EOF, "");
        //self.tokens.push(end);
        Ok(self.tokens.clone())
    }

    /// Creates an error with the given message at the current line.
    pub fn error(&self, message: String) -> Result<Token, String> {
        Err(format!("[line : {}] Error : {}", self.line, message))
    }

    /// Checks if we are at the end of the file.
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Scan the tokens, basically match everything known and returns an error if we can't
    fn scan_token(&mut self) -> Result<Token, String> {
        return match self.advance() {
            '[' => Ok(self.token(TokenType::LeftBrace, "")),
            ']' => Ok(self.token(TokenType::RightBrace, "")),
            '(' => Ok(self.token(TokenType::LeftParen, "")),
            ')' => Ok(self.token(TokenType::RightParen, "")),
            '{' => Ok(self.token(TokenType::LeftCurlyBrace, "")),
            '}' => Ok(self.token(TokenType::RightCurlyBrace, "")),
            ',' => Ok(self.token(TokenType::COMMA, "")),
            '.' => Ok(self.token(TokenType::DOT, "")),
            '+' => Ok(self.token(TokenType::PLUS, "")),
            '%' => Ok(self.token(TokenType::MOD, "")),
            ';' => Ok(self.token(TokenType::SEMICOLON, "")),
            ':' => Ok(match self.match_next(':') {
                true => self.token(TokenType::DOUBLECOLON, ""),
                _ => self.token(TokenType::COLON, ""),
            }),
            '-' => Ok(match self.match_next('>') {
                true => self.token(TokenType::ARROW, ""),
                _ => self.token(TokenType::MINUS, ""),
            }),
            '*' => Ok(self.token(TokenType::STAR, "")),
            '!' => Ok(match self.match_next('=') {
                true => self.token(TokenType::BangEqual, ""),
                _ => self.token(TokenType::BANG, ""),
            }),
            '=' => Ok(match self.match_next('=') {
                true => self.token(TokenType::EqualEqual, ""),
                _ => self.token(TokenType::EQUAL, ""),
            }),
            '&' => Ok(match self.match_next('&') {
                true => self.token(TokenType::ANDAND, ""),
                _ => self.token(TokenType::AND, ""),
            }),
            '|' => Ok(match self.match_next('|') {
                true => self.token(TokenType::OROR, ""),
                _ => self.token(TokenType::OR, ""),
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
            '\'' => self.scan_char(),
            '0'...'9' => self.number(),
            'a'...'z' => self.identifier(),
            'A'...'Z' => self.identifier(),
            c => Err(format!("unexpected token : {}", c)),
        };
    }

    /// Parses an identifier at the current position
    /// If it is a known keyword, register it as a keyword.
    fn identifier(&mut self) -> Result<Token, String> {
        while self.peek().is_alphanumeric() && !self.is_at_end() {
            self.advance();
        }
        let sub_string: String = self.source[self.start..self.current].into_iter().collect();
        match KEYWORDS.get::<str>(&sub_string) {
            Some(k) => Ok(self.token(k.clone(), &sub_string)),
            None => Ok(self.token(TokenType::IDENTIFIER, &sub_string)),
        }
    }

    fn scan_char(&mut self) -> Result<Token, String> {
        if self.is_at_end() {
            return self.error("Unterminated char declaration".to_string());
        }
        let val = self.advance();
        let mut fin = val;
        if val == '\\' {
            fin = match self.advance() {
                'n' => '\n',
                c => c,
            }
        }
        match self.match_next('\'') {
            false => self.error("char must be only 1 character long".to_string()),
            true => Ok(Token::char(self.line, fin)),
        }
    }

    /// Parses a string literal at the given position
    /// Checks for unterminated string.
    /// It allows multiline strings.
    fn string(&mut self) -> Result<Token, String> {
        let mut chars = vec![];
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            chars.push(match self.advance() {
                '\\' => match self.advance() {
                    'n' => Ok('\n'),
                    't' => Ok('\t'),
                    c => self
                        .error(format!("unknown escape character : {}", c))
                        .map(|_| 'i'),
                }?,
                c => c,
            });
        }
        if self.is_at_end() {
            self.error("Unterminated string sequence".to_string())
        } else {
            let sub_string: String = chars.iter().collect();
            self.advance();
            Ok(self.token(TokenType::STRING, &sub_string))
        }
    }

    /// Parses a number at the given position. (float or int)
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
        let sub_string: String = self.source[self.start..self.current].into_iter().collect();
        Ok(self.token(TokenType::NUMBER, &sub_string))
    }

    /// Peeks twice for next chars in the source but do not advance.
    fn peek_next(&mut self) -> char {
        if self.current + 1 == self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    /// Peeks once for next char in the source but do not advance.
    fn peek(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    /// Matches the given char, if it is a match, advance, else do nothing.
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

    /// Advance and consume a char, returning it.
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    /// Creates a new token at the given line and position.
    fn token(&mut self, token: TokenType, lexeme: &str) -> Token {
        Token::new(self.line, token, lexeme.to_string())
    }
}
