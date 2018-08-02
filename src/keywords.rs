//! Contains the association between the keywords and theyre tokens.
use std::collections::HashMap;
use token::TokenType;

lazy_static! {
    /// These are the langage's keywords.
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("break",    TokenType::BREAK);
        m.insert("and",    TokenType::AND);
        m.insert("Class",  TokenType::CLASS);
        m.insert("else",   TokenType::ELSE);
        m.insert("false",  TokenType::FALSE);
        m.insert("for",    TokenType::FOR);
        m.insert("fn",     TokenType::FUN);
        m.insert("import", TokenType::IMPORT);
        m.insert("method", TokenType::METHOD);
        m.insert("trait",  TokenType::TRAIT);
        m.insert("Self",   TokenType::BIGSELF);
        m.insert("of",     TokenType::OF);
        m.insert("if",     TokenType::IF);
        m.insert("nil",    TokenType::NIL);
        m.insert("or",     TokenType::OR);
        m.insert("return", TokenType::RETURN);
        m.insert("super",  TokenType::SUPER);
        m.insert("this",   TokenType::THIS);
        m.insert("true",   TokenType::TRUE);
        //m.insert("var",    TokenType::VAR);
        m.insert("while",  TokenType::WHILE);
        //m.insert("num",  TokenType::NUM);
        //m.insert("string",  TokenType::StringDecl);
        m
    };
}
