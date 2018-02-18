
#[derive(Debug, PartialEq, Eq, Clone)]
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
pub struct Token {
    line: usize,
    token_type: TokenType,
    lexeme: String,
}

impl Token {
    pub fn new(line: usize, token_type: TokenType, lexeme: String) -> Self {
        Token {
            line: line,
            token_type: token_type,
            lexeme: lexeme,
        }
    }
    pub fn ignore(&self) -> bool {
        self.token_type == TokenType::IGNORE
    }
    pub fn is_type(&self, token_type : &TokenType) -> bool {
        &self.token_type == token_type
    }
    pub fn get_type(&self) -> &TokenType {
        &self.token_type
    }
    pub fn get_lexeme(&self) -> &String {
        &self.lexeme
    }
    pub fn get_line(&self) -> usize {
        self.line
    }
}
