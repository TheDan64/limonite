use crate::interner::StrId;
use crate::lexical::keywords::Keyword;
use crate::lexical::token::{CommentKind, Token, TokenKind};
use crate::span::{Span, Spanned};

use std::iter::{Iterator, Peekable};
use std::str::CharIndices;

pub type TokenResult<'s> = Result<Token<'s>, LexerError>;

// REVIEW: CharIndices may not be sufficient for unicode with modifiers?
// For example, y̆ is y + \u{0306} modifier. I think the current approach
// would treat them separately and likely fail early on the modifier
// since it's not treated as the same "character".
pub struct Lexer<'s> {
    file_id: StrId,
    line_number: usize,
    column_number: usize,
    input: &'s str,
    iter: Peekable<CharIndices<'s>>,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str, file_id: StrId) -> Self {
        Lexer {
            file_id,
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

    pub fn consume_while<F: FnMut(char) -> bool>(&mut self, mut test: F) -> Spanned<&'s str> {
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
            let span = Span::new(self.file_id, start_idx, end_idx);

            Spanned::new(&self.input[start_idx..=end_idx], span)
        } else {
            Spanned::default_with_file_id(self.file_id)
        }
    }

    fn consume_comment(&mut self, start_idx: usize) -> TokenResult<'s> {
        let mut end_idx = start_idx;

        // Consume 2nd '>'
        self.consume_char();

        let comment_kind = match self.next_char() {
            // Multiline comments must end in <<< else error
            Some('>') => {
                unimplemented!("Multi-line comments");
            },
            // Single line comments eat up anything until newline or eof
            Some(_) => {
                let comment = self.consume_while(|ch| match ch {
                    '\n' => false,
                    _ => true,
                });

                end_idx = comment.span().end_idx;

                CommentKind::Single(comment)
            },
            // Single line comment w/ EOF at start should be valid:
            None => CommentKind::Single(Spanned::default()),
        };

        let span = Span::new(self.file_id, start_idx, end_idx);

        Ok(Spanned::new(TokenKind::Comment(comment_kind), span))
    }

    fn consume_tabs(&mut self) -> TokenResult<'s> {
        let mut count = 0;

        // Consume the newline token, count tabs
        let (start_idx, _) = self.consume_char().expect("Can't fail");
        let tabs = self.consume_while(|ch| match ch {
            '\t' => {
                count += 1;
                true
            },
            _ => false,
        });

        let end_idx = tabs.span().end_idx;
        let span = Span::new(self.file_id, start_idx, end_idx);

        Ok(Spanned::new(TokenKind::Indent(tabs, count), span))
    }

    // Identifiers: [a-zA-Z_][a-zA-z0-9_]*
    // Keywords are subsets of identifiers.
    fn consume_identifier(&mut self) -> TokenResult<'s> {
        // Lexer will only let you start with alpha or undescore,
        // so there is no need to check for numeric start
        let ident = self.consume_while(|ch| match ch {
            a if a.is_alphanumeric() => true,
            '_' => true,
            _ => false,
        });

        match ident.node() {
            "True" => return Ok(ident.map(|_| TokenKind::BoolLiteral(true))),
            "False" => return Ok(ident.map(|_| TokenKind::BoolLiteral(false))),
            _ => (),
        };

        if let Ok(keyword) = ident.node().parse::<Keyword>() {
            return Ok(ident.map(|_| TokenKind::Keyword(keyword)));
        }

        Ok(ident.map(|s| TokenKind::Identifier(s)))
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = TokenResult<'s>;

    // Parse the file where it left off and return the next token
    fn next(&mut self) -> Option<Self::Item> {
        let tok = match self.next_char() {
            // Find >> and >>> comments, otherwise > or >= symbols
            Some('>') => {
                let (start_idx, _) = self.consume_char().expect("Can't fail");

                match self.next_char() {
                    Some('>') => self.consume_comment(start_idx),
                    Some('=') => {
                        self.consume_char();
                        unimplemented!("self.symbols_token(\">=\");");
                    },
                    _ => unimplemented!("self.symbols_token(\">\")"),
                }
            },
            // Count tabs: \n\t*
            Some('\n') => self.consume_tabs(),
            // Find Keywords and Identifiers
            Some(a) if a.is_alphabetic() || a == '_' => self.consume_identifier(),
            None => return None,
            Some(chr) => unimplemented!("{}", chr),
        };

        Some(tok)
    }
}

#[derive(Debug)]
pub enum LexerError {}

#[test]
fn test_consume_while() {
    let s = "Hello, World!";
    let mut lexer = Lexer::new(s, StrId::DUMMY);

    assert_eq!(lexer.consume_while(|c| c.is_alphanumeric()).node(), "Hello");
    assert_eq!(lexer.consume_while(|c| !c.is_alphanumeric()).node(), ", ");
    assert_eq!(lexer.consume_while(|c| c.is_alphanumeric()).node(), "World");
    assert_eq!(lexer.consume_while(|c| !c.is_alphanumeric()).node(), "!");
    assert_eq!(lexer.consume_while(|c| c.is_alphanumeric()).node(), "");
}

#[test]
fn test_comment_hello_world() {
    let s = ">> This is a comment
print(\"Hello, world!\")\n";
    let mut lexer = Lexer::new(s, StrId::DUMMY);

    let tokens: Result<Vec<Token>, _> = lexer.into_iter().collect();
    let tokens = tokens.unwrap();

    assert_eq!(tokens[0].span().indexes(), (0, 20));
}
