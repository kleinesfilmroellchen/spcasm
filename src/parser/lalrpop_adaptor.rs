//! LALRPOP adaptor code.

use std::num::NonZeroU64;
use std::vec::IntoIter;

#[allow(unused)]
use smartstring::alias::String;

use super::{source_range, Token};
use crate::error::AssemblyError;
use crate::sema::reference::RelativeReferenceDirection;
use crate::sema::Register;

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

/// Because LALRPOP's grammar needs to be LR(1) but SPC700 assembly by default is not, we do a preprocessing step which
/// is (somewhat) LL(2). Things we do here:
/// - insert final newline to allow simpler newline-related grammar rules
/// - disambiguate parenthesis used for expressions and for addressing
/// - combine '+X' and '+Y' into one token
/// - transform simple '-' into a "range dash" for range specifications
#[allow(clippy::too_many_lines)]
pub fn preprocess_token_stream(tokens: Vec<Token>) -> Vec<Token> {
	let mut tokens = tokens.into_iter();
	let mut result = Vec::new();
	// Only set when we are immediately expecting an addressing mode. This is either after a mnemonic or a comma on an
	// instruction line.
	let mut expecting_indexing_addressing_mode = false;
	// We're currently on a line that has a mnemonic; important for determining what purpose commas have.
	let mut in_mnemonic_line = false;
	let mut last_offset = 0;
	while let Some(next_token) = tokens.next() {
		last_offset = next_token.source_span().offset() + next_token.source_span().len() - 1;
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
			// Protect dashes from getting transformed into range dashes inside parenthesis.
			Token::OpenParenthesis(..) if !expecting_indexing_addressing_mode && !in_mnemonic_line => {
				result.push(next_token);
				let mut depth = 1usize;
				while depth > 0 && let Some(next_inner_token) = tokens.next() {
					match &next_inner_token {
						Token::CloseParenthesis(..) => depth -= 1,
						Token::OpenParenthesis(..) => depth += 1,
						Token::Newline(..) => depth = 0,
						_ => (),
					}
					result.push(next_inner_token);
				}
				continue;
			},
			Token::Minus(offset) if !in_mnemonic_line => {
				result.push(Token::RangeMinus(offset));
				continue;
			},
			Token::Plus(offset) | Token::Minus(offset) => {
				expecting_indexing_addressing_mode = false;
				let direction = match next_token {
					Token::Plus(_) => RelativeReferenceDirection::Forward,
					Token::Minus(_) => RelativeReferenceDirection::Backward,
					_ => unreachable!(),
				};
				let mut collected_tokens = vec![next_token.clone()];
				let mut final_token = None;
				let was_uniform = loop {
					if let Some(further_token) = tokens.next() {
						if further_token == next_token {
							// It can still be a relative + / - token...
							collected_tokens.push(further_token);
						} else if matches!(further_token, Token::TestComment(..) | Token::Newline(_)) {
							// We really did find a relative + / - token.
							final_token = Some(further_token);
							in_mnemonic_line = false;
							break true;
						} else {
							// Didn't find a relative + / - token.
							final_token = Some(further_token);
							break false;
						}
					} else {
						break true;
					}
				};
				if was_uniform || collected_tokens.len() > 1 {
					let plus_amount = NonZeroU64::new(u64::try_from(collected_tokens.len()).unwrap()).unwrap();
					let source_span =
						source_range(offset.into(), collected_tokens.last().unwrap().source_span().into());
					result.push(match direction {
						RelativeReferenceDirection::Backward => Token::RelativeLabelMinus(plus_amount, source_span),
						RelativeReferenceDirection::Forward => Token::RelativeLabelPlus(plus_amount, source_span),
					});
				} else {
					result.append(&mut collected_tokens);
				}
				if let Some(final_token) = final_token {
					result.push(final_token);
				}
				continue;
			},
			// After all of these tokens we can be sure we can't have an indexing addressing mode, so parentheses can
			// stay. For example: '#' signals immediate, '/' signals negated bit indexing, '+', '-' signal the
			// unambiguous start of a number, '.'/identifier signal references.
			_ if expecting_indexing_addressing_mode => expecting_indexing_addressing_mode = false,
			_ => (),
		}
		result.push(next_token);
	}
	result.push(Token::Newline(last_offset.into()));

	result.iter().fold(Vec::new(), |mut tokens, token| {
		if let Some(plus @ Token::Plus(..)) = tokens.last().cloned() && let Token::Register(register @(Register::X | Register::Y), ..) = token {
			tokens.pop();
			tokens.push(Token::PlusRegister(*register,
				source_range(plus.source_span().into(), token.source_span().into())));
		} else {
			tokens.push(match token {
				Token::Register(register, location) => Token::Register(register.coerce_alternate_registers(), *location),
				Token::Mnemonic(mnemonic, location) => Token::Mnemonic(mnemonic.coerce_alternate_mnemonics(), *location),
				others => others.clone(),
			});
		}
		tokens
	})
}
