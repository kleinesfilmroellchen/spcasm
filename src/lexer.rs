//! Lexing.
#![allow(clippy::use_self)]

use std::fmt::Display;
use std::iter::Peekable;
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};
use serde::Serialize;
use serde_variant::to_variant_name;
use spcasm_derive::Parse;

use crate::error::{AssemblyCode, AssemblyError};
use crate::parser::Parse;

/// Assembly language tokens.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
	/// Identifier, i.e. a label.
	Identifier(String, SourceSpan),
	/// Register name (this can never be used as an identifier).
	Register(Register, SourceSpan),
	/// Literal number which was already parsed.
	Number(i64, SourceSpan),
	/// '#'
	Hash(SourceOffset),
	/// ','
	Comma(SourceOffset),
	/// '+'
	Plus(SourceOffset),
	/// '('
	OpenParenthesis(SourceOffset),
	/// ')'
	CloseParenthesis(SourceOffset),
	/// ':'
	Colon(SourceOffset),
	/// ASCII newline (\n).
	Newline(SourceOffset),
	/// Comments used for testing purposes.
	#[cfg(test)]
	TestComment(Vec<u8>, SourceSpan),
}

impl Token {
	/// Returns this token if it matches the given type. For all tokens that contain data, this data is ignored.
	///
	/// # Errors
	/// If the token doesn't match, an "Expected XYZ" error string is returned.
	#[allow(clippy::needless_pass_by_value)]
	pub fn expect(&self, type_: Self, src: Arc<AssemblyCode>) -> Result<&Self, AssemblyError> {
		match self {
			Self::Identifier(..) => match type_ {
				Self::Identifier(..) => Ok(self),
				_ => Err(()),
			},
			Self::Number(..) => match type_ {
				Self::Number(..) => Ok(self),
				_ => Err(()),
			},
			Self::Register(..) => match type_ {
				Self::Register(..) => Ok(self),
				_ => Err(()),
			},
			Self::Colon(..) => match type_ {
				Self::Colon(..) => Ok(self),
				_ => Err(()),
			},
			Self::OpenParenthesis(..) => match type_ {
				Self::OpenParenthesis(..) => Ok(self),
				_ => Err(()),
			},
			Self::CloseParenthesis(..) => match type_ {
				Self::CloseParenthesis(..) => Ok(self),
				_ => Err(()),
			},
			Self::Hash(..) => match type_ {
				Self::Hash(..) => Ok(self),
				_ => Err(()),
			},
			Self::Plus(..) => match type_ {
				Self::Plus(..) => Ok(self),
				_ => Err(()),
			},
			Self::Newline(..) => match type_ {
				Self::Newline(..) => Ok(self),
				_ => Err(()),
			},
			Self::Comma(..) => match type_ {
				Self::Comma(..) => Ok(self),
				_ => Err(()),
			},
			#[cfg(test)]
			Self::TestComment(..) => match type_ {
				Self::TestComment(..) => Ok(self),
				_ => Err(()),
			},
		}
		.map_err(|_| AssemblyError::ExpectedToken {
			expected: self.clone(),
			actual: type_,
			location: self.source_span(),
			src,
		})
	}

	/// Returns the source span where this token is located in the file.
	#[must_use]
	pub fn source_span(&self) -> SourceSpan {
		match self {
			Self::Hash(location)
			| Self::CloseParenthesis(location)
			| Self::Colon(location)
			| Self::Comma(location)
			| Self::Newline(location)
			| Self::OpenParenthesis(location)
			| Self::Plus(location) => (*location, SourceOffset::from(1)).into(),
			Self::Identifier(_, location) | Self::Number(_, location) | Self::Register(_, location) => *location,
			#[cfg(test)]
			Self::TestComment(_, location) => *location,
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", match self {
			Self::Identifier(..) => "identifier",
			Self::Register(..) => "register name",
			Self::Number(..) => "number",
			Self::Hash(..) => "hash",
			Self::Comma(..) => "comma",
			Self::Plus(..) => "'+'",
			Self::OpenParenthesis(..) => "'('",
			Self::CloseParenthesis(..) => "')'",
			Self::Newline(..) => "new line",
			Self::Colon(..) => "':'",
			#[cfg(test)]
			Self::TestComment(..) => "test comment (';=')",
		})
	}
}

/// Registers.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Parse, Serialize)]
pub enum Register {
	/// Accumulator.
	A,
	/// First index register.
	X,
	/// Second index register.
	Y,
	/// Stack pointer.
	SP,
	/// Program status word (instruction pointer).
	PSW,
	/// Combined 16-bit register from Y and A.
	YA,
}

impl Display for Register {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", to_variant_name(self).unwrap())
	}
}

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
