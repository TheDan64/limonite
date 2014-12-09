extern crate limonite;

use std::vec::MoveItems;

use limonite::syntax::lexer::Tokenizer;
use limonite::syntax::parser::Parser;
use limonite::syntax::core::tokens::Token;
use limonite::syntax::core::tokens::Token::{EOF, Identifier, Keyword, Numeric, Punctuation};
use limonite::syntax::core::keywords::Keywords::{Var};
use limonite::syntax::core::punctuation::Punctuations::{Equals};

struct MockLexer {
    tokens: MoveItems<Token>,
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
}

#[test]
fn test_variable_int_declaration() {
    let mLexer = MockLexer::new(vec![Keyword(Var), Identifier("meow".to_string()),
                                     Punctuation(Equals), Numeric("3".to_string(), None)]);
    let mut parser = Parser::new(mLexer);

    // parser.parse();
}
