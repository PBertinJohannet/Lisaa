use token::TokenType;
use std::collections::HashMap;

lazy_static! {
    /// These are the langage's keywords.
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("and",    TokenType::AND);
        m.insert("class",  TokenType::CLASS);
        m.insert("else",   TokenType::ELSE);
        m.insert("false",  TokenType::FALSE);
        m.insert("for",    TokenType::FOR);
        m.insert("fun",    TokenType::FUN);
        m.insert("if",     TokenType::IF);
        m.insert("nil",    TokenType::NIL);
        m.insert("or",     TokenType::OR);
        m.insert("print",  TokenType::PRINT);
        m.insert("return", TokenType::RETURN);
        m.insert("super",  TokenType::SUPER);
        m.insert("this",   TokenType::THIS);
        m.insert("true",   TokenType::TRUE);
        m.insert("var",    TokenType::VAR);
        m.insert("while",  TokenType::WHILE);
        m
    };
}


