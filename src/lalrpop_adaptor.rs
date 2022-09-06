//! LALRPOP adaptor code.

use std::vec::IntoIter;

use crate::error::AssemblyError;
use crate::{Register, Token};

/// An API adaptor that allows us to pass the Vec<Token> we lexed into LALRPOP.
pub struct LalrpopAdaptor(IntoIter<Token>);

impl From<Vec<Token>> for LalrpopAdaptor {
	fn from(vec: Vec<Token>) -> Self {
		Self(vec.into_iter())
	}
}

impl Iterator for LalrpopAdaptor {
	type Item = Result<(usize, Token, usize), AssemblyError>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0.next().map(|token| {
			Ok((token.source_span().offset(), token.clone(), token.source_span().offset() + token.source_span().len()))
		})
	}
}

/// Because LALRPOP's grammar needs to be LR(1), it cannot disambiguate the use of parentheses for expressions and
/// indexing addressing modes. Therefore, we do this in a preprocessing step which is (somewhat) LL(2).
pub fn disambiguate_indexing_parenthesis(tokens: Vec<Token>) -> Vec<Token> {
	let mut tokens = tokens.into_iter();
	let mut result = Vec::new();
	// Only set when we are immediately expecting an addressing mode. This is either after a mnemonic or a comma on an
	// instruction line.
	let mut expecting_indexing_addressing_mode = false;
	// We're currently on a line that has a mnemonic; important for determining what purpose commas have.
	let mut in_mnemonic_line = false;
	while let Some(next_token) = tokens.next() {
		match next_token {
			Token::Mnemonic(..) => {
				in_mnemonic_line = true;
				expecting_indexing_addressing_mode = true;
			},
			Token::Newline(..) => {
				expecting_indexing_addressing_mode = false;
				in_mnemonic_line = false;
			},
			Token::Comma(..) if in_mnemonic_line => expecting_indexing_addressing_mode = true,
			// This is an indexing parenthesis.
			Token::OpenParenthesis(..) if expecting_indexing_addressing_mode => {
				let mut inner_tokens = Vec::new();
				let mut depth = 1usize;
				let mut hit_newline = false;
				while depth > 0 && let Some(next_inner_token) = tokens.next() {
					inner_tokens.push(next_inner_token.clone());

					match next_inner_token {
						Token::CloseParenthesis(..) => depth -= 1,
						Token::OpenParenthesis(..) => depth += 1,
						Token::Newline(..) => {
							hit_newline = true;
							break;
						},
						_ => (),
					}
				}
				// We hit a newline without a matching closing brace; the actual parser will reject this soon enough.
				// Let's just bail.
				if hit_newline {
					result.push(next_token);
					result.append(&mut inner_tokens);
					continue;
				}

				// Normal case, the last token is a closing parenthesis.
				// Remember that we are just replacing the outermost parenthesis, the inner ones were just consumed by
				// us and will not be replaced.
				result.push(Token::OpenIndexingParenthesis(next_token.source_span().offset().into()));
				// If this is false, we had `( )` in the stream, so the inner tokens are just the closing parenthesis.
				if inner_tokens.len() > 1 {
					result.extend_from_slice(&inner_tokens[.. inner_tokens.len() - 1]);
				}
				result
					.push(Token::CloseIndexingParenthesis(inner_tokens.last().unwrap().source_span().offset().into()));

				// Don't push the parenthesis token.
				continue;
			},
			// After all of these tokens we can be sure we can't have an indexing addressing mode, so parentheses can
			// stay. For example: '#' signals immediate, '/' signals negated bit indexing, '+', '-' signal the
			// unambiguous start of a number, '.'/identifier signal labels.
			_ if expecting_indexing_addressing_mode => expecting_indexing_addressing_mode = false,
			_ => (),
		}
		result.push(next_token);
	}
	result.push(Token::Newline(0.into()));

	result.iter().fold(Vec::new(), |mut tokens, token| {
		if let Some(plus @ Token::Plus(..)) = tokens.last().cloned() && let Token::Register(register @(Register::X | Register::Y), ..) = token {
			tokens.pop();
			tokens.push(Token::PlusRegister(*register, 
				(plus.source_span().offset(), plus.source_span().offset() + plus.source_span().len() - token.source_span().offset()).into()));
		} else {
			tokens.push(token.clone());
		}
		tokens
	})
}
