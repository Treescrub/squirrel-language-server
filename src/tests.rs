use crate::analysis::lexer::{Lexer, TokenType};

#[test]
fn hex_literal() {
    let mut lexer = Lexer::new("0x12345678");
    lexer.lex();

    assert_eq!(lexer.tokens.len(), 1);
    assert_eq!(lexer.tokens[0].token_type, TokenType::IntegerLiteral);
    assert_eq!(lexer.tokens[0].nvalue, Some(305419896));
}

#[test]
fn hex_literal_overflow() {
    let mut lexer = Lexer::new("0x123456789");
    lexer.lex();

    assert_eq!(lexer.tokens.len(), 1);
    assert_eq!(lexer.tokens[0].token_type, TokenType::Invalid);
}