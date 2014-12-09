extern crate limonite;

use std::vec::MoveItems;

use limonite::syntax::lexer::Tokenizer;
use limonite::syntax::parser::Parser;
use limonite::syntax::core::tokens::Token;
use limonite::syntax::core::keywords::Keywords;
use limonite::syntax::core::punctuation::Punctuations;

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
            None => Token::EOF,
        }
    }
}

#[test]
fn test_variable_int_declaration() {
    let mLexer = MockLexer::new(vec![Token::Keyword(Keywords::Var), Token::Identifier("meow".to_string()),
                                     Token::Punctuation(Punctuations::Equals), Token::Numeric("3".to_string(), None)]);
    let mut parser = Parser::new(mLexer);

    // parser.parse();
}
