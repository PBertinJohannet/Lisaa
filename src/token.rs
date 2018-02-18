//! The tokens are defined here
//!

#[derive(Debug, PartialEq, Eq, Clone)]
/// All the different types of tokens.
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BangEqual,
    EQUAL,
    EqualEqual,
    GREATER,
    GreaterEqual,
    LESS,
    LessEqual,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    IGNORE,

    EOF,
}

#[derive(Debug, Clone)]
/// The structure of the token, contains the line, the type and the content.
pub struct Token {
    line: usize,
    token_type: TokenType,
    lexeme: String,
}

impl Token {
    /// Creates a new token at the given line with the given type.
    pub fn new(line: usize, token_type: TokenType, lexeme: String) -> Self {
        Token {
            line: line,
            token_type: token_type,
            lexeme: lexeme,
        }
    }

    /// Checks if the token should be ignored.
    pub fn ignore(&self) -> bool {
        self.token_type == TokenType::IGNORE
    }

    /// checks if the token is of the given type.
    pub fn is_type(&self, token_type : &TokenType) -> bool {
        &self.token_type == token_type
    }

    /// Returns the type of the token.
    pub fn get_type(&self) -> &TokenType {
        &self.token_type
    }

    /// Returns the lexeme of the token.
    pub fn get_lexeme(&self) -> &String {
        &self.lexeme
    }

    /// returns the line at wich the token arrived.
    pub fn get_line(&self) -> usize {
        self.line
    }
}
