//! Lexing.

use std::iter::Peekable;
use std::num::NonZeroU64;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};
use miette::SourceOffset;

use super::{Parse, Token};
use crate::cli::Frontend;
use crate::directive::DirectiveSymbol;
use crate::error::AssemblyError;
use crate::sema::instruction::{MemoryAddress, Mnemonic};
use crate::sema::Register;
use crate::AssemblyCode;

macro_rules! start_of_identifier {
	() => { 'A' ..= 'Z' | 'a' ..= 'z' | '_' | '@' };
}

macro_rules! is_identifier {
	($chr:expr) => {
		($chr.is_alphanumeric() || $chr.is_ascii_digit() || ['_', '-', '@'].contains($chr))
	};
}

/// Lex the given assembly into a list of tokens.
/// # Errors
/// Errors are returned for any syntactical error at the token level, e.g. invalid number literals.
#[allow(clippy::missing_panics_doc, clippy::too_many_lines)]
pub fn lex(source_code: Arc<AssemblyCode>, options: &dyn Frontend) -> Result<Vec<Token>, Box<AssemblyError>> {
	let mut chars = source_code.text.chars().peekable();
	let code_length = source_code.text.len();
	let mut index = 0usize;
	let mut tokens = Vec::new();

	while let Some(chr) = chars.next() {
		// \r is treated as white space and ignored.
		if chr == '\n' {
			tokens.push(Token::Newline(index.min(code_length - 1).into()));
			index += 1;
			continue;
		} else if chr.is_whitespace() {
			index += 1;
			// A ':' surrounded by whitespace serves as a pseudo-line separator within a line for code organization.
			if chars.peek().is_some_and(|chr| chr == &':') {
				chars.next();
				index += 1;
				if chars.peek().is_some_and(|c| c.is_whitespace()) {
					chars.next();
					index += 1;
					tokens.push(Token::Newline((index - 2).into()));
				} else {
					tokens.push(Token::Colon((index - 1).into()));
				}
			}
			continue;
		}
		match chr {
			'"' => {
				let start_index = index;
				index += 1;
				let text = next_string(&mut chars, source_code.clone(), &mut index)?.into_iter().map(|chr| chr as u8).collect();
				let text_span = (start_index, index - start_index).into();
				tokens.push(Token::String(text, text_span));
			},
			'\'' => {
				let start_index = index;
				index += 1;
				let chr = match chars.next() {
					Some('\'') => Err(AssemblyError::UnexpectedCharacter { chr: '\'', location: (index, 1).into(), src: source_code.clone() }.into()),
					Some('\\') => {
						index += 1;
						next_escape_sequence(&mut chars, source_code.clone(), &mut index)
					},
					Some(chr) => {
						index += 1;
						Ok(chr)
					},
					None => Err(AssemblyError::UnexpectedEndOfTokens {
						expected: vec!["\"".into()],
						location: (source_code.text.len() - 1, 0).into(),
						src:      source_code.clone(),
					}.into()),
				}?;
				let end = chars.next();
				if let Some(end) = end && end != '\'' {
					return Err(AssemblyError::UnexpectedCharacter { chr: end, location: (index, 1).into(), src: source_code.clone() }.into());
				} else if end.is_none() {
					return Err(AssemblyError::UnexpectedEndOfTokens { expected: vec!["'".into()], location: (index, 1).into(), src: source_code.clone() }.into());
				}
				index += 1;
				let end_index = index;
				tokens.push(Token::Number(chr as MemoryAddress, source_code.text[start_index..=end_index].into(), (start_index, end_index - start_index).into()));
			},
			start_of_identifier!() => {
				tokens.push(parse_identifier_like(&mut chars, chr, &mut index, &source_code)?);
			},
			'0'..='9' => {
				let (number, size) = next_number(&mut chars, Some(chr), false, 10, index, &source_code, options)?;
				tokens.push(number);
				index += size;
			},
			'.' if let Some(letter_after_dot @ ('b' | 'w')) = chars.peek().copied() => {
				chars.next();
				// This is a local label, not an addressing mode suffix!
				if chars.peek().is_some_and(|chr| is_identifier!(chr)) {
					tokens.push(parse_single_char_tokens('.', index.into()));
					index += 1;
					tokens.push(parse_identifier_like(&mut chars, letter_after_dot, &mut index, &source_code)?);
				} else {
					index += 2;
					tokens.push((if letter_after_dot == 'b' {Token::ExplicitDirectPage} else {Token::ExplicitNoDirectPage})
						((index - 2, 2).into()));
				}
			},
			'*' if chars.peek().is_some_and(|chr| chr == &'*') => {
				chars.next();
				index += 2;
				tokens.push(Token::DoubleStar((index - 2, 2).into()));
			},
			'<' if chars.peek().is_some_and(|chr| chr == &'<') => {
				chars.next();
				index += 2;
				tokens.push(Token::DoubleOpenAngleBracket((index - 2, 2).into()));
			},
			'>' if chars.peek().is_some_and(|chr|chr == &'>') => {
				chars.next();
				index += 2;
				tokens.push(Token::DoubleCloseAngleBracket((index - 2, 2).into()));
			},
			'!' if matches!(chars.peek(), Some(start_of_identifier!())) => {
				let chr = chars.next().unwrap();
				let start_index = index;
				let identifier = next_identifier(&mut chars, chr);
				index += identifier.len() + 1;
				let identifier_span = (start_index, identifier.len() + 1).into();
				tokens.push(Token::Identifier(identifier, identifier_span));
			},
			'!' if chars.peek().is_some_and(|chr|chr == &'=') => {
				chars.next();
				index += 2;
				tokens.push(Token::ExclamationEquals((index - 2, 2).into()));
			},
			'=' if chars.peek().is_some_and(|chr|chr == &'=') => {
				chars.next();
				index += 2;
				tokens.push(Token::DoubleEquals((index - 2, 2).into()));
			},
			'<' if chars.peek().is_some_and(|chr|chr == &'=') => {
				chars.next();
				index += 2;
				tokens.push(Token::OpenAngleBracketEquals((index - 2, 2).into()));
			},
			'>' if chars.peek().is_some_and(|chr|chr == &'=') => {
				chars.next();
				index += 2;
				tokens.push(Token::CloseAngleBracketEquals((index - 2, 2).into()));
			},
			'-' => {
				let start = index;
				index += 1;
				while chars.peek() == Some(&'-') {
					index += 1;
					chars.next();
				}
				let end = index;
				// Multi-dash: reference!
				if end - start > 1 {
					tokens.push(Token::RelativeLabelMinus(NonZeroU64::new((end-start).try_into().unwrap()).unwrap(), (start, end-start).into()));
				} else {
					tokens.push(parse_single_char_tokens(chr, start.into()));
				}
			},
			'#' | ',' | '+' | '^' | '|' | '~' | '&' | '*' | '(' | ')' | '[' | ']' | ':' | '.' | '/' | '!' | '=' | '<' | '>' => {
				tokens.push(parse_single_char_tokens(chr, index.into()));
				index += 1;
			},
			'$' => {
				let (number, size) = next_number(&mut chars, None, true, 16, index, &source_code, options)?;
				tokens.push(number);
				index += size+1;
			},
			'%' => {
				let can_be_binary = chars.peek().map(|chr| ['0', '1'].contains(chr));
				let (token, increment) = if can_be_binary.is_some_and(|v| v) {
					next_number(&mut chars, None, true, 2, index, &source_code, options)
				} else {
					Ok((Token::Percent(index.into()), 0))
				}?;
				tokens.push(token);
				index += increment + 1;
			},
			';' =>
				if cfg!(test) && let Some(chr) = chars.peek() && chr == &'=' {
					chars.next();
					#[allow(unused)]
					let start_index = index;
					index += 2;
					#[cfg(test)]
					let mut comment_contents = String::new();
					// Either stop at a newline or another regular comment.
					while let Some(chr) = chars.peek() && chr != &'\n' && chr != &';' {
						#[cfg(test)]
						comment_contents.push(chars.next().unwrap());
						index += 1;
					}
					// This cfg() is technically unnecessary, but the above check doesn't happen at compile time so
					// Rust wouldn't find the conditionally-compiled token type.
					#[cfg(test)]
					tokens.push(Token::TestComment(comment_contents
						.split_whitespace()
						.map(|byte_string| u8::from_str_radix(byte_string, 16))
						.try_collect::<Vec<u8>>()
						.map_err(|parse_error| AssemblyError::InvalidTestComment {
							basis: parse_error,
							src: source_code.clone(),
							location: (start_index, comment_contents.len()).into()
						})?, (start_index, comment_contents.len()).into()));
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
			}.into()),
		}
	}

	Ok(tokens)
}

fn next_identifier(chars: &mut Peekable<std::str::Chars>, start: char) -> SharedStr {
	let mut identifier = String::new();
	identifier.push(start);
	while let Some(chr) = chars.peek() && (chr.is_alphanumeric() || chr.is_ascii_digit() || ['_', '-', '@' ].contains(chr)) {
		identifier.push(chars.next().unwrap());
	}
	identifier.into()
}

fn parse_identifier_like(
	chars: &mut Peekable<std::str::Chars>,
	start: char,
	index: &mut usize,
	source_code: &Arc<AssemblyCode>,
) -> Result<Token, AssemblyError> {
	let start_index = *index;
	let identifier = next_identifier(chars, start);
	*index += identifier.len();
	let identifier_span = (start_index, identifier.len()).into();

	Token::parse_special_identifier(&identifier.to_lowercase(), identifier_span, source_code.clone())
		.map(|value| Token::SpecialIdentifier(value, identifier_span))
		.or_else(|_| {
			Register::parse(&identifier.to_lowercase(), identifier_span, source_code.clone())
				.map(|value| Token::Register(value, identifier_span))
		})
		.or_else(|_| {
			DirectiveSymbol::parse(&identifier.to_lowercase(), identifier_span, source_code.clone())
				.map(|value| Token::Directive(value, identifier_span))
		})
		.or_else(|_| {
			Mnemonic::parse(&identifier.to_lowercase(), identifier_span, source_code.clone())
				.map(|mnemonic| Token::Mnemonic(mnemonic, identifier_span))
		})
		.or_else::<AssemblyError, _>(|_| Ok(Token::Identifier(identifier, identifier_span)))
}

/// Parse the next number from the character stream `chars`. Since the first character may have already been extracted
/// from the stream, it can be provided separately. If this number is known to be a number (mostly due to radix
/// prefixes), `must_be_number` is set.
///
/// # Returns
/// The number, parsed as either a Number token or an Identifier token (if parsing the number failed), as well as the
/// length of underlying text is returned. An error is only returned if fallback parsing of numbers as identifiers is
/// disabled according to the `options` or with `must_be_number`.
fn next_number(
	chars: &mut Peekable<std::str::Chars>,
	first_char: Option<char>,
	must_be_number: bool,
	radix: u8,
	start_index: usize,
	source_code: &Arc<AssemblyCode>,
	options: &dyn Frontend,
) -> Result<(Token, usize), Box<AssemblyError>> {
	let mut number_chars: String = first_char.map_or_else(std::string::String::default, std::string::String::from);
	while let Some(chr) = chars.peek() && chr.is_alphanumeric() {
		number_chars.push(chars.next().unwrap());
	}
	let number_chars = number_chars.into_shared_str();
	let location = (start_index, number_chars.len() + usize::from(radix != 10)).into();
	i64::from_str_radix(&number_chars, radix.into())
		.map_or_else(
			|error| {
				if must_be_number {
					Err(AssemblyError::InvalidNumber { error, location, src: source_code.clone() }.into())
				} else {
					options.report_diagnostic(AssemblyError::NumberIdentifier {
						text: number_chars.clone(),
						error,
						location: (start_index, number_chars.len()).into(),
						src: source_code.clone(),
					});
					Ok(Token::Identifier(number_chars.clone(), location))
				}
			},
			|number| {
				Ok(Token::Number(
					number,
					source_code.text[start_index .. start_index + number_chars.len() + usize::from(radix != 10)].into(),
					location,
				))
			},
		)
		.map(|token| (token, number_chars.len()))
}

fn next_string(
	chars: &mut Peekable<std::str::Chars>,
	source_code: Arc<AssemblyCode>,
	start_index: &mut usize,
) -> Result<Vec<char>, Box<AssemblyError>> {
	let mut text = Vec::new();
	while let Some(chr) = chars.next() {
		*start_index += 1;
		match chr {
			'"' => return Ok(text),
			'\\' => text.push(next_escape_sequence(chars, source_code.clone(), start_index)?),
			_ => text.push(chr),
		}
	}
	Err(AssemblyError::UnexpectedEndOfTokens {
		expected: vec!["\"".into()],
		location: (source_code.text.len() - 1, 0).into(),
		src:      source_code,
	}
	.into())
}

/// Used for parsing escape sequences both in string and character literals; for this reason allow escaping ' and " even
/// though you don't need to escape ' in a string (and vice-versa).
fn next_escape_sequence(
	chars: &mut Peekable<std::str::Chars>,
	source_code: Arc<AssemblyCode>,
	start_index: &mut usize,
) -> Result<char, Box<AssemblyError>> {
	if let Some(chr) = chars.next() {
		*start_index += 1;
		// TODO: Do we want to support unicode literals, and with which format? The plain format of many languages
		// (\u1234) or the Rust format (\u{1234})?
		match chr {
			// The usual suspects...
			'"' | '\'' | '\\' => Ok(chr),
			'n' => Ok('\n'),
			'r' => Ok('\r'),
			't' => Ok('\t'),
			'0' => Ok('\0'),
			// Byte literals
			'x' => {
				let first_number = chars
					.next()
					.ok_or(AssemblyError::UnexpectedEndOfTokens {
						expected: vec!["first byte literal digit".into()],
						location: (source_code.text.len() - 1, 0).into(),
						src:      source_code.clone(),
					})?
					.to_ascii_lowercase();
				let second_number = chars
					.next()
					.ok_or(AssemblyError::UnexpectedEndOfTokens {
						expected: vec!["second byte literal digit".into()],
						location: (source_code.text.len() - 1, 0).into(),
						src:      source_code.clone(),
					})?
					.to_ascii_lowercase();
				if !first_number.is_ascii_hexdigit() || !second_number.is_ascii_hexdigit() {
					return Err(AssemblyError::InvalidNumber {
						// HACK: We can't create an invalid digit error manually, so let's hijack a stdlib parser to do
						// it for us.
						error:    u32::from_str_radix("HACK", 2).unwrap_err(),
						src:      source_code,
						location: (*start_index + 1, 2).into(),
					}
					.into());
				}
				let value =
					char::from_u32(u32::from_str_radix(&format!("{}{}", first_number, second_number), 16).unwrap())
						.unwrap();
				*start_index += 2;
				Ok(value)
			},
			_ => Err(AssemblyError::ExpectedTokens {
				expected: vec!["'".into(), "\"".into(), "t".into(), "n".into(), "0".into(), "r".into(), "x".into()],
				actual:   Token::String(vec![chr as u8], (*start_index, 0).into()),
				location: (*start_index, 0).into(),
				src:      source_code,
			}
			.into()),
		}
	} else {
		Err(AssemblyError::UnexpectedEndOfTokens {
			expected: vec!["escape sequence".into()],
			location: (source_code.text.len() - 1, 0).into(),
			src:      source_code.clone(),
		}
		.into())
	}
}

fn parse_single_char_tokens(chr: char, location: SourceOffset) -> Token {
	match chr {
		'+' => Token::Plus(location),
		'-' => Token::Minus(location),
		'*' => Token::Star(location),
		',' => Token::Comma(location),
		'#' => Token::Hash(location),
		'(' => Token::OpenParenthesis(location),
		')' => Token::CloseParenthesis(location),
		'[' => Token::OpenIndexingParenthesis(location),
		']' => Token::CloseIndexingParenthesis(location),
		'<' => Token::OpenAngleBracket(location),
		'>' => Token::CloseAngleBracket(location),
		'%' => Token::Percent(location),
		'&' => Token::Ampersand(location),
		'|' => Token::Pipe(location),
		'~' => Token::Tilde(location),
		'^' => Token::Caret(location),
		':' => Token::Colon(location),
		'/' | '!' => Token::Slash(location),
		'.' => Token::Period(location),
		'=' => Token::Equals(location),
		_ => unreachable!(),
	}
}
