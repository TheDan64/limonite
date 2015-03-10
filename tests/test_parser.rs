extern crate limonite;

use std::vec::IntoIter;

use limonite::syntax::lexer::Tokenizer;
use limonite::syntax::parser::Parser;
use limonite::syntax::core::tokens::Token;
use limonite::syntax::core::tokens::Token::{EOF, Identifier, Keyword, Numeric, Symbol};
use limonite::syntax::core::keywords::Keywords;
use limonite::syntax::core::symbols::Symbols;

struct MockLexer {
    tokens: IntoIter<Token>
}

impl MockLexer {
    fn new(v: Vec<Token>) -> MockLexer {
        MockLexer {
            tokens: v.into_iter()
        }
    }
}

impl Tokenizer for MockLexer {
    fn get_tok(&mut self) -> Token {
        let next = self.tokens.next();
        match next {
            Some(tok) => tok,
            None => EOF,
        }
    }

    fn get_error_pos(&self) -> (usize, usize, usize, usize) {
        (1, 1, 1, 1)
    }
}

#[test]
fn test_print() {
    let lexer = MockLexer::new(vec![
        Keyword(Keywords::Print),
        Symbol(Symbols::ParenOpen),
        Identifier("meow".to_string()),
        Symbol(Symbols::Comma),
        Identifier("meow".to_string()),
        Symbol(Symbols::ParenClose),
    ]);
    let mut parser = Parser::new(lexer);
    parser.parse();
}

#[test]
fn test_variable_int_declaration() {
    // let mLexer = MockLexer::new(vec![
        // Keyword(Keywords::Var),
        // Identifier("meow".to_string()),
        // Symbol(Symbols::Equals),
    // ]);
    // let mut parser = Parser::new(mLexer);

    // parser.parse();
}
