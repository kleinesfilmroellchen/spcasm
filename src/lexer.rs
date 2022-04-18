//! Lexing.

use std::iter::Peekable;

/// Assembly language tokens.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
	/// Identifier, i.e. a label.
	Identifier(String),
	/// Register name (this can never be used as an identifier).
	Register(Register),
	/// Literal number which was already parsed.
	Number(i64),
	/// '#'
	Hash,
	/// ','
	Comma,
	/// '+'
	Plus,
	/// '('
	OpenParenthesis,
	/// ')'
	CloseParenthesis,
	/// ':'
	Colon,
	/// ASCII newline (\n).
	Newline,
	/// Comments used for testing purposes.
	#[cfg(test)]
	TestComment(Vec<u8>),
}

impl Token {
	/// Returns this token if it matches the given type. For all tokens that contain data, this data is ignored.
	///
	/// # Errors
	/// If the token doesn't match, an "Expected XYZ" error string is returned.
	#[allow(clippy::needless_pass_by_value)]
	pub fn expect(&self, type_: Self) -> Result<&Self, String> {
		match self {
			Self::Identifier(_) => match type_ {
				Self::Identifier(_) => Ok(self),
				_ => Err("Expected identifier".to_owned()),
			},
			Self::Number(_) => match type_ {
				Self::Number(_) => Ok(self),
				_ => Err("Expected number".to_owned()),
			},
			Self::Register(_) => match type_ {
				Self::Register(_) => Ok(self),
				_ => Err("Expected register name".to_owned()),
			},
			_ =>
				if &type_ == self {
					Ok(self)
				} else {
					Err(format!("Expected {:?}", type_))
				},
		}
	}
}

/// Registers.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Register {
	/// Accumulator.
	A,
	/// First index register.
	X,
	/// Second index register.
	Y,
	/// Stack pointer.
	SP,
	/// Program status word (instruction pointer)
	PSW,
}

/// Lex the given assembly into a list of tokens.
/// # Errors
/// Errors are returned for any syntactical error at the token level, e.g. invalid number literals.
#[allow(clippy::missing_panics_doc)]
pub fn lex(program: &str) -> Result<Vec<Token>, String> {
	let mut chars = program.chars().peekable();
	let mut tokens = Vec::new();

	while let Some(chr) = chars.next() {
		// \r is treated as white space and ignored.
		if chr == '\n' {
			tokens.push(Token::Newline);
		}
		if chr.is_whitespace() {
			continue;
		}
		match chr {
			'A' ..= 'Z' | 'a' ..= 'z' | '_' => {
				let identifier = next_identifier(&mut chars, chr);
				tokens.push(if identifier == "A" {
					Token::Register(Register::A)
				} else if identifier == "X" {
					Token::Register(Register::X)
				} else if identifier == "Y" {
					Token::Register(Register::Y)
				} else if identifier == "SP" {
					Token::Register(Register::SP)
				} else if identifier == "PSW" {
					Token::Register(Register::PSW)
				} else {
					Token::Identifier(identifier)
				});
			},
			'#' | ',' | '+' | '(' | ')' | ':' => tokens.push(parse_single_char_tokens(chr)),
			'$' => tokens.push(Token::Number(next_hex_number(&mut chars)?)),
			'%' => tokens.push(Token::Number(next_bin_number(&mut chars)?)),
			';' =>
				if cfg!(test) && let Some(chr) = chars.peek() && chr == &'=' {
					chars.next();
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
						.map_err(|parse_error| format!("Test comment is invalid: {:?}", parse_error))?));
				} else {
					while let Some(chr) = chars.peek() && chr != &'\n' {
						chars.next();
					}
				},
			_ => return Err(format!("Unexpected character {}", chr)),
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

fn next_hex_number(chars: &mut Peekable<std::str::Chars>) -> Result<i64, String> {
	let mut number_chars = String::default();
	while let Some(chr) = chars.peek() && chr.is_ascii_hexdigit() {
		number_chars.push(chars.next().unwrap());
	}
	i64::from_str_radix(&number_chars, 16).map_err(|_| "Not a valid hex number".to_owned())
}

fn next_bin_number(chars: &mut Peekable<std::str::Chars>) -> Result<i64, String> {
	let mut number_chars = String::default();
	while let Some(chr) = chars.peek() && ['0', '1'].contains(chr) {
		number_chars.push(chars.next().unwrap());
	}
	i64::from_str_radix(&number_chars, 2).map_err(|_| "Not a valid binary number".to_owned())
}

fn parse_single_char_tokens(chr: char) -> Token {
	match chr {
		'+' => Token::Plus,
		',' => Token::Comma,
		'#' => Token::Hash,
		'(' => Token::OpenParenthesis,
		')' => Token::CloseParenthesis,
		':' => Token::Colon,
		_ => unreachable!(),
	}
}
