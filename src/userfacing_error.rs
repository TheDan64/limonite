use crate::lexical::LexerError;
use crate::span::Span;
use crate::syntax::{ParserError, ParserErrorKind};

use std::fmt::{Display, Formatter, Result as FmtResult};
use std::io::Write;
use std::str;

// TODO: Use termion or something for this
const RED_FG: &str = "\x1B[38;5;1m";
const GREEN_FG: &str = "\x1B[38;5;2m";
const YELLOW_FG: &str = "\x1B[38;5;3m";
const BLUE_FG: &str = "\x1B[38;5;4m";
const MAGENTA_FG: &str = "\x1B[38;5;5m";
const LIGHT_BLUE_FG: &str = "\x1B[38;5;6m";
const DARK_GRAY_FG: &str = "\x1B[38;5;8m";
const LIGHT_RED_FG: &str = "\x1B[38;5;9m";
const WHITE_FG: &str = "\x1B[38;5;15m";
const CLEAR: &str = "\x1B[0m";

#[derive(Debug)]
pub struct UserfacingError<'intern, 's> {
    pub file_name: &'intern str,
    pub file_str: &'s str,
    pub error: ParserError<'s>,
}

impl<'s> UserfacingError<'_, 's> {
    // Returns the full source line and a Span adjusted to the substr.
    fn get_line(&self, span: Span) -> (&'s str, u32, Span) {
        let file_bytes = self.file_str.as_bytes();
        let Span { mut start_idx, mut end_idx, file_id } = span;
        let mut line_no = 1;

        while end_idx < file_bytes.len() && file_bytes[end_idx + 1] != '\n' as u8 {
            end_idx += 1;
        }

        while start_idx > 0 && file_bytes[start_idx - 1] != '\n' as u8 {
            start_idx -= 1;
        }

        // TODO: Maybe we can cache line info/location so that the entire source isn't scoured
        // multiple times.
        for ch in self.file_str[..=start_idx].chars() {
            if ch == '\n' {
                line_no += 1;
            }
        }

        let adjusted_span = Span::new(file_id, span.start_idx - start_idx, end_idx - start_idx);

        (&self.file_str[start_idx..=end_idx], line_no, adjusted_span)
    }
}

impl Display for UserfacingError<'_, '_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let (err_msg, span) = match self.error.kind {
            ParserErrorKind::LexerError(le) => match le {
                LexerError::IncompleteNumeric(num) => {
                    if num.node() == "0b" {
                        ("Found an incomplete binary literal", num.span())
                    } else {
                        ("Found an incomplete hex literal", num.span())
                    }
                },
                LexerError::InvalidNumeric(num, suffix) => {
                    if num.node() == "0b" {
                        ("Found an invalid binary literal", suffix.span())
                    } else {
                        ("Found an invalid hex literal", suffix.span())
                    }
                },
                _ => todo!(),
            },
            ParserErrorKind::UnexpectedToken(tok) => todo!(),
        };

        let (line, line_no, adjusted_span) = self.get_line(span);
        let mut line_no_str = [0u8; 10];

        write!(&mut line_no_str as &mut [u8], "{}", line_no).expect("u32 to always fit in 10 bytes");

        let line_no_str = str::from_utf8(&line_no_str[..bytes_written(&line_no_str)]).unwrap();
        let n = line_no_str.len() + 1;

        f.write_str(RED_FG)?;
        f.write_str("⚑ error")?;
        f.write_str(WHITE_FG)?;
        f.write_str(": ")?;
        f.write_str(err_msg)?;
        f.write_str(BLUE_FG)?;

        write_nl_n_spaces(f, n)?;

        f.write_str("┏━━▶ ")?;
        f.write_str(CLEAR)?;
        f.write_str(self.file_name)?; // TODO: "file_name:line:column"?
        f.write_str(BLUE_FG)?;
        f.write_str("\n")?;
        f.write_str(line_no_str)?;
        f.write_str(" ┃")?;

        write_nl_n_spaces(f, n)?;

        f.write_str("┗━━▶ ")?;
        f.write_str(CLEAR)?;
        f.write_str(line)?;
        f.write_str(RED_FG)?;

        write_nl_n_spaces(f, n + adjusted_span.start_idx + 5)?;
        write_n_chars(f, adjusted_span.width(), "^")?;

        f.write_str(CLEAR)?;
        f.write_str("\n")
    }
}

fn bytes_written(bytes: &[u8]) -> usize {
    bytes.iter()
        .position(|&byte| byte == 0)
        .unwrap_or(bytes.len())
}

#[inline]
fn write_n_chars(f: &mut Formatter, n: usize, s: &str) -> FmtResult {
    for _ in 0..n {
        f.write_str(s)?;
    }

    Ok(())
}

#[inline]
fn write_nl_n_spaces(f: &mut Formatter, n: usize) -> FmtResult {
    f.write_str("\n")?;

    write_n_chars(f, n, " ")
}
