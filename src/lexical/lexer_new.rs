use crate::lexical::token::Token;
use crate::span::Spanned;

use std::iter::{Iterator, Peekable};
use std::str::CharIndices;

pub struct Lexer<'s> {
    line_number: usize,
    column_number: usize,
    input: &'s str,
    iter: Peekable<CharIndices<'s>>,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        Lexer {
            line_number: 1,
            column_number: 1,
            input,
            iter: input.char_indices().peekable(),
        }
    }

    // Gets the next char and sets the position forward in the buffer
    fn consume_char(&mut self) -> Option<(usize, char)> {
        if let Some((index, chr)) = self.iter.next() {
            self.column_number += 1;

            if chr == '\n' {
                self.line_number += 1;
                self.column_number = 1;
            }

            return Some((index, chr));
        }

        None
    }

    fn next_char(&mut self) -> Option<char> {
        if let Some(result) = self.iter.peek() {
            let &(_, chr) = result;
            return Some(chr);
        }

        None
    }

    fn consume_while<F: FnMut(char) -> bool>(&mut self, mut test: F) -> &'s str {
        let mut start_idx = None;
        let mut end_idx = 0;

        loop {
            let next_char = match self.next_char() {
                Some(chr) => chr,
                None => break,
            };

            if !test(next_char) {
                break;
            }

            let (index, _) = self.consume_char().expect("Can never fail");

            if start_idx.is_none() {
                start_idx = Some(index);
            }

            end_idx = index;
        }

        if let Some(start_idx) = start_idx {
            &self.input[start_idx..=end_idx]
        } else {
            ""
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    // Parse the file where it left off and return the next token
    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

#[test]
fn test_consume_while() {
    let s = "Hello, World!";
    let mut lexer = Lexer::new(s);

    assert_eq!(lexer.consume_while(|c| c.is_alphanumeric()), "Hello");
    assert_eq!(lexer.consume_while(|c| !c.is_alphanumeric()), ", ");
    assert_eq!(lexer.consume_while(|c| c.is_alphanumeric()), "World");
    assert_eq!(lexer.consume_while(|c| !c.is_alphanumeric()), "!");
    assert_eq!(lexer.consume_while(|c| c.is_alphanumeric()), "");
}
