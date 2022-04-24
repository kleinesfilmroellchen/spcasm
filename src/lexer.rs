//! Lexing.

use std::iter::Peekable;
use std::sync::Arc;

use miette::SourceOffset;

use crate::error::{AssemblyCode, AssemblyError};
use crate::parser::Parse;
use crate::{Register, Token};

/// Lex the given assembly into a list of tokens.
/// # Errors
/// Errors are returned for any syntactical error at the token level, e.g. invalid number literals.
#[allow(clippy::missing_panics_doc)]
pub fn lex(program: &str, name: String) -> Result<Vec<Token>, AssemblyError> {
	let source_code = Arc::new(AssemblyCode { name, text: program.to_owned() });
	let mut chars = program.chars().peekable();
	let mut index = 0;
	let mut tokens = Vec::new();

	while let Some(chr) = chars.next() {
		// \r is treated as white space and ignored.
		if chr == '\n' {
			tokens.push(Token::Newline(index.into()));
			index += 1;
			continue;
		} else if chr.is_whitespace() {
			index += 1;
			continue;
		}
		match chr {
			'A' ..= 'Z' | 'a' ..= 'z' | '_' => {
				let start_index = index;
				let identifier = next_identifier(&mut chars, chr);
				index += identifier.len();
				let identifier_span =  (start_index, identifier.len()).into();
				tokens.push(Register::parse(&identifier.to_lowercase(), identifier_span, source_code.clone())
								.map_or_else(|_| Token::Identifier(identifier, identifier_span),
									|value| Token::Register(value, identifier_span)));
			},
			'#' | ',' | '+' | '(' | ')' | ':' => {
				tokens.push(parse_single_char_tokens(chr, index.into()));
				index += 1;
			},
			'$' => {
				let (number, size) = next_hex_number(&mut chars, index, source_code.clone())?;
				tokens.push(Token::Number(number, (index, size).into()));
				index += size+1;
			}
			'%' => {
				let (number, size) = next_bin_number(&mut chars, index, source_code.clone())?;
				tokens.push(Token::Number(number, (index, size).into()));
				index += size+1;
			}
			';' =>
				if cfg!(test) && let Some(chr) = chars.peek() && chr == &'=' {
					chars.next();
					index += 1;
					let mut comment_contents = String::new();
					while let Some(chr) = chars.peek() && chr != &'\n' {
						comment_contents.push(chars.next().unwrap());
					}
					// This cfg() is technically unnecessary, but the above check doesn't happen at compile time so
					// Rust wouldn't find the conditionally-compiled token type.
					#[cfg(test)]
					tokens.push(Token::TestComment(comment_contents
						.split_whitespace()
						.map(|byte_string| u8::from_str_radix(byte_string, 16))
						.try_collect::<Vec<u8>>()
						.map_err(|parse_error| AssemblyError::InvalidTestComment {
							basis: Some(parse_error),
							src: source_code.clone(),
							location: (index, comment_contents.len()).into()
						})?, (index, comment_contents.len()).into()));
					index += comment_contents.len();
				} else {
					index += 1;
					while let Some(chr) = chars.peek() && chr != &'\n' {
						chars.next();
						index += 1;
					}
				},
			_ => return Err(AssemblyError::UnexpectedCharacter {
				chr,
				location: index.into(),
				src: source_code
			}),
		}
	}

	Ok(tokens)
}

fn next_identifier(chars: &mut Peekable<std::str::Chars>, start: char) -> String {
	let mut identifier = String::default();
	identifier.push(start);
	while let Some(chr) = chars.peek() && (chr.is_alphanumeric() || chr.is_ascii_digit() || chr == &'_') {
		identifier.push(chars.next().unwrap());
	}
	identifier
}

fn next_hex_number(
	chars: &mut Peekable<std::str::Chars>,
	start_index: usize,
	source_code: Arc<AssemblyCode>,
) -> Result<(i64, usize), AssemblyError> {
	let mut number_chars = String::default();
	while let Some(chr) = chars.peek() && chr.is_ascii_hexdigit() {
		number_chars.push(chars.next().unwrap());
	}
	i64::from_str_radix(&number_chars, 16)
		.map_err(|error| AssemblyError::InvalidNumber {
			error,
			location: (start_index, number_chars.len()).into(),
			src: source_code,
		})
		.map(|value| (value, number_chars.len()))
}

fn next_bin_number(
	chars: &mut Peekable<std::str::Chars>,
	start_index: usize,
	source_code: Arc<AssemblyCode>,
) -> Result<(i64, usize), AssemblyError> {
	let mut number_chars = String::default();
	while let Some(chr) = chars.peek() && ['0', '1'].contains(chr) {
		number_chars.push(chars.next().unwrap());
	}
	i64::from_str_radix(&number_chars, 2)
		.map_err(|error| AssemblyError::InvalidNumber {
			error,
			location: (start_index, number_chars.len()).into(),
			src: source_code,
		})
		.map(|value| (value, number_chars.len()))
}

fn parse_single_char_tokens(chr: char, location: SourceOffset) -> Token {
	match chr {
		'+' => Token::Plus(location),
		',' => Token::Comma(location),
		'#' => Token::Hash(location),
		'(' => Token::OpenParenthesis(location),
		')' => Token::CloseParenthesis(location),
		':' => Token::Colon(location),
		_ => unreachable!(),
	}
}
