//! The tokens are defined here
//!

#[derive(Debug, PartialEq, Eq, Clone)]
/// All the different types of tokens.
pub enum TokenType {
    // Single-character tokens.
    /// A left parenthesis.
    LeftParen,
    /// A right parenthesis.
    RightParen,
    /// A left brace.
    LeftBrace,
    /// A right brace.
    RightBrace,
    /// A comma.
    COMMA,
    /// A dot.
    DOT,
    /// A minus sign.
    MINUS,
    /// A plus sign.
    PLUS,
    /// A semicolon.
    SEMICOLON,
    /// A slash, used to divide.
    SLASH,
    /// A star "*".
    STAR,

    // One or two character tokens.
    /// A not "!".
    BANG,
    /// A not equals "!=".
    BangEqual,
    /// A single equal sign.
    EQUAL,
    /// A double equal sign.
    EqualEqual,
    /// A single greater than sign.
    GREATER,
    /// A greater than or equals sign ">=".
    GreaterEqual,
    /// A single less sign.
    LESS,
    /// A less than or equals sign "<=".
    LessEqual,

    // Literals.
    /// An identifier, variables, classes names etc...
    IDENTIFIER,
    /// A string literal.
    STRING,
    /// A number.
    NUMBER,

    /// The type of a num variable removed .
    //NUM,
    /// The type string before an assignment.
    StringDecl,

    // Keywords.
    /// A break to quit a scope.
    BREAK,
    /// The and keyword, used for comparison.
    AND,
    /// The class keyword, not used yet
    CLASS,
    /// The class keyword, not used yet
    ELSE,
    /// The class keyword, not used yet
    FALSE,
    /// The class keyword, not used yet
    FUN,
    /// The class keyword, not used yet.
    FOR,
    /// The class keyword, not used yet
    IF,
    /// The class keyword, not used yet
    NIL,
    /// The class keyword, not used yet
    OR,
    /// The class keyword, not used yet
    RETURN,
    /// The class keyword, not used yet
    SUPER,
    /// The class keyword, not used yet
    THIS,
    /// The class keyword, not used yet
    TRUE,
    /// The class keyword, not used yet
    VAR,
    /// The class keyword, not used yet
    WHILE,
    /// The ignore keyword, tells that this keyword may be ignored.
    IGNORE,

    /// The End of the File
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
    pub fn is_type(&self, token_type: &TokenType) -> bool {
        &self.token_type == token_type
    }

    /// Returns the type of the token.
    pub fn get_type(&self) -> &TokenType {
        &self.token_type
    }

    /// Changes the type of this token.
    pub fn set_type(&mut self, tp : TokenType) {
        self.token_type = tp;
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
