use crate::interner::StrId;
use crate::lexical::keywords::Keyword;
use crate::lexical::symbols::Symbol;
use crate::lexical::token::{CommentKind, Token, TokenKind};
use crate::span::{Span, Spanned};

use std::iter::{Iterator, Peekable};
use std::str::CharIndices;

pub type TokenResult<'s> = Result<Token<'s>, LexerError>;

// REVIEW: CharIndices may not be sufficient for unicode with modifiers?
// For example, y̆ is y + \u{0306} modifier. I think the current approach
// would treat them separately and likely fail early on the modifier
// since it's not treated as the same "character".
/// A zero-copy Lexer/Tokenizer.
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

        let end_idx = if tabs.span().end_idx == 0 {
            start_idx
        } else {
            tabs.span().end_idx
        };
        let span = Span::new(self.file_id, start_idx, end_idx);

        Ok(Spanned::new(TokenKind::Indent(count), span))
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

    // Single and multi char symbols: *, -, +=, -=, ...
    fn symbols_token(&mut self) -> TokenResult<'s> {
        let mut bail = false;
        let mut count = 0;

        let symbol = self.consume_while(|ch| {
            if bail || count > 2 {
                return false;
            }

            count += 1;

            return match ch {
                '(' | ')' | '[' | ']' | '{' | '}' | '.' | ',' | ':' | '^' | '~' | '=' => {
                    bail = true;
                    true
                },
                '+' | '-' | '*' | '/' | '>' | '<' | '%' => true,
                _ => false
            }
        });

        Ok(symbol.map(|s| TokenKind::Symbol(s.parse::<Symbol>().expect("Can't fail"))))
    }

    // This is currently set up to accept multi line strings
    fn consume_string_literal(&mut self) -> TokenResult<'s> {
        let mut last_char_escaped = false;
        let mut unescaped_quotes = 0;
        let string = self.consume_while(|ch| {
            if unescaped_quotes == 2 {
                return false;
            }

            match ch {
                '\"' if !last_char_escaped => {
                    unescaped_quotes += 1;
                    last_char_escaped = false;
                    true
                },
                '\\' => {
                    last_char_escaped = true;
                    true
                },
                _ => {
                    last_char_escaped = false;
                    true
                }
            }
        });

        // Hit eof before last quote
        if &string.node()[string.node().len() - 1..] != "\"" {
            unimplemented!("EOF error: {}", string.node());
        }

        Ok(string.map(|s| TokenKind::StrLiteral(s)))
    }

    fn consume_whitespace(&mut self) {
        self.consume_while(|ch| match ch {
            '\n' | '\t' => false,
            w if w.is_whitespace() => true,
            _ => false,
        });
    }

    fn consume_numeric(&mut self) -> TokenResult<'s> {
        let mut leading_zero = self.next_char() == Some('0');
        let mut count = 0;
        let mut guarenteed_float = false;
        let mut guarenteed_int = false;

        let number = self.consume_while(|ch| {
            count += 1;

            match ch {
                '.' if !guarenteed_float && !guarenteed_int => {
                    guarenteed_float = true;
                    true
                },
                'x' | 'b' if leading_zero && !guarenteed_float => {
                    leading_zero = false;
                    guarenteed_int = true;
                    true
                },
                c if c.is_numeric() => {
                    if count > 1 {
                        leading_zero = false;
                    }

                    true
                }
                _ => false,
            }
        });
        let suffix = self.consume_while(|ch| match ch {
            c if c.is_alphanumeric() => true,
            '_' => true,
            _ => false,
        });
        // If the suffix is empty, use None instead
        let opt_suffix = if suffix.span().start_idx < suffix.span().end_idx {
            Some(suffix)
        } else {
            None
        };

        let end_idx = if suffix.span().end_idx == 0 {
            number.span().end_idx
        } else {
            suffix.span().end_idx
        };
        let outer_span = Span::new(self.file_id, number.span().start_idx, end_idx);

        Ok(Spanned::new(TokenKind::Numeric(number, opt_suffix), outer_span))
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = TokenResult<'s>;

    // Parse the file where it left off and return the next token
    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();

        let tok = match self.next_char() {
            // Find >> and >>> comments, otherwise > or >= symbols
            Some('>') => {
                let (start_idx, _) = self.consume_char().expect("Can't fail");

                match self.next_char() {
                    Some('>') => self.consume_comment(start_idx),
                    Some('=') => {
                        self.symbols_token()
                    },
                    _ => self.symbols_token(),
                }
            },
            // Find single-char symbols
            Some('(') | Some(')') |
            Some('[') | Some(']') |
            Some('{') | Some('}') |
            Some('.') |
            Some(',') |
            Some(':') |
            Some('^') |
            Some('~') |
            Some('+') |
            Some('*') |
            Some('/') |
            Some('%') |
            Some('-') |
            Some('<') |
            Some('=') => {
                self.symbols_token()
            },
            // Find string literals, "String"
            Some('\"') => self.consume_string_literal(),
            // Count tabs: \n\t*
            Some('\n') => self.consume_tabs(),
            // Find Keywords and Identifiers
            Some(a) if a.is_alphabetic() || a == '_' => self.consume_identifier(),
            // Find ints, floats, hex, and bin numeric values
            Some(n) if n.is_digit(10) => self.consume_numeric(),
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
    let lexer = Lexer::new(s, StrId::DUMMY);

    let tokens: Result<Vec<Token>, _> = lexer.into_iter().collect();
    let tokens = tokens.unwrap();

    assert_eq!(&s[tokens[0].span()], ">> This is a comment");
    assert_eq!(
        tokens[0].node(),
        TokenKind::Comment(CommentKind::Single(Spanned::new(" This is a comment", Span::new(StrId::DUMMY, 2, 19)))),
    );

    assert_eq!(&s[tokens[1].span()], "\n");
    assert_eq!(tokens[1].node(), TokenKind::Indent(0));

    assert_eq!(&s[tokens[2].span()], "print");
    assert_eq!(tokens[2].node(), TokenKind::Identifier("print"));

    assert_eq!(&s[tokens[3].span()], "(");
    assert_eq!(tokens[3].node(), TokenKind::Symbol(Symbol::ParenOpen));

    assert_eq!(&s[tokens[4].span()], "\"Hello, world!\"");
    assert_eq!(tokens[4].node(), TokenKind::StrLiteral("\"Hello, world!\""));

    assert_eq!(&s[tokens[5].span()], ")");
    assert_eq!(tokens[5].node(), TokenKind::Symbol(Symbol::ParenClose));
}

#[test]
fn test_non_ascii() {
    // FIXME: y̆ after Å will fail
    let s = "abÅcd";
    let lexer = Lexer::new(s, StrId::DUMMY);
    let tokens: Result<Vec<Token>, _> = lexer.into_iter().collect();
    let tokens = tokens.unwrap();

    assert_eq!(&s[tokens[0].span()], "abÅcd");
    assert_eq!(tokens[0].node(), TokenKind::Identifier("abÅcd"));
}


#[test]
fn test_vars_if_while() {
    let s = "var x = 1 + 2
if x <= 3,
\tprint(\"x <= 3\")
";
    let lexer = Lexer::new(s, StrId::DUMMY);
    let tokens: Result<Vec<Token>, _> = lexer.into_iter().collect();
    let tokens = tokens.unwrap();

    assert_eq!(&s[tokens[0].span()], "var");
    assert_eq!(tokens[0].node(), TokenKind::Keyword(Keyword::Var));

    assert_eq!(&s[tokens[1].span()], "x");
    assert_eq!(tokens[1].node(), TokenKind::Identifier("x"));

    assert_eq!(&s[tokens[2].span()], "=");
    assert_eq!(tokens[2].node(), TokenKind::Symbol(Symbol::Equals));

    let inner_span = Span::new(StrId::DUMMY, 8, 8);

    assert_eq!(&s[inner_span], "1");
    assert_eq!(&s[tokens[3].span()], "1");
    assert_eq!(tokens[3].node(), TokenKind::Numeric(Spanned::new("1", inner_span), None));

    assert_eq!(&s[tokens[4].span()], "+");
    assert_eq!(tokens[4].node(), TokenKind::Symbol(Symbol::Plus));

    let inner_span = Span::new(StrId::DUMMY, 12, 12);

    assert_eq!(&s[inner_span], "2");
    assert_eq!(&s[tokens[5].span()], "2");
    assert_eq!(tokens[5].node(), TokenKind::Numeric(Spanned::new("2", inner_span), None));

    assert_eq!(&s[tokens[6].span()], "\n");
    assert_eq!(tokens[6].node(), TokenKind::Indent(0));

    assert_eq!(&s[tokens[7].span()], "if");
    assert_eq!(tokens[7].node(), TokenKind::Keyword(Keyword::If));

    assert_eq!(&s[tokens[8].span()], "x");
    assert_eq!(tokens[8].node(), TokenKind::Identifier("x"));

    assert_eq!(&s[tokens[9].span()], "<=");
    assert_eq!(tokens[9].node(), TokenKind::Symbol(Symbol::LessThanEqual));

    let inner_span = Span::new(StrId::DUMMY, 22, 22);

    assert_eq!(&s[inner_span], "3");
    assert_eq!(&s[tokens[10].span()], "3");
    assert_eq!(tokens[10].node(), TokenKind::Numeric(Spanned::new("3", inner_span), None));

    assert_eq!(&s[tokens[11].span()], ",");
    assert_eq!(tokens[11].node(), TokenKind::Symbol(Symbol::Comma));

    assert_eq!(&s[tokens[12].span()], "\n\t");
    assert_eq!(tokens[12].node(), TokenKind::Indent(1));

    assert_eq!(&s[tokens[13].span()], "print");
    assert_eq!(tokens[13].node(), TokenKind::Identifier("print"));

    assert_eq!(&s[tokens[14].span()], "(");
    assert_eq!(tokens[14].node(), TokenKind::Symbol(Symbol::ParenOpen));

    assert_eq!(&s[tokens[15].span()], "\"x <= 3\"");
    assert_eq!(tokens[15].node(), TokenKind::StrLiteral("\"x <= 3\""));

    assert_eq!(&s[tokens[16].span()], ")");
    assert_eq!(tokens[16].node(), TokenKind::Symbol(Symbol::ParenClose));

    assert_eq!(&s[tokens[17].span()], "\n");
    assert_eq!(tokens[17].node(), TokenKind::Indent(0));
}
