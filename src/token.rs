//! The Token struct.

use std::fmt::Display;
use std::mem::MaybeUninit;
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};
use serde_variant::to_variant_name;

use crate::error::{AssemblyCode, AssemblyError, TokenOrString};
use crate::instruction::Mnemonic;
use crate::mcro::MacroSymbol;
use crate::Register;

/// Assembly language tokens.
#[derive(Debug, Clone)]
pub enum Token {
	/// Mnemonic, the start of an instruction.
	Mnemonic(Mnemonic, SourceSpan),
	/// Identifier, i.e. a label.
	Identifier(String, SourceSpan),
	/// Register name (this can never be used as an identifier).
	Register(Register, SourceSpan),
	/// Start of a macro.
	Macro(MacroSymbol, SourceSpan),
	/// Literal number which was already parsed.
	Number(i64, SourceSpan),
	/// '#'
	Hash(SourceOffset),
	/// ','
	Comma(SourceOffset),
	/// '+'
	Plus(SourceOffset),
	/// '/'
	Slash(SourceOffset),
	/// '('
	OpenParenthesis(SourceOffset),
	/// ')'
	CloseParenthesis(SourceOffset),
	/// ':'
	Colon(SourceOffset),
	/// '.'
	Period(SourceOffset),
	/// '='
	Equals(SourceOffset),
	/// ASCII newline (\n).
	Newline(SourceOffset),
	/// Comments used for testing purposes.
	#[cfg(test)]
	TestComment(Vec<u8>, SourceSpan),
}

impl Eq for Token {}
impl PartialEq for Token {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Identifier(name, ..), Self::Identifier(other_name, ..)) => name == other_name,
			(Self::Macro(name, ..), Self::Macro(other_name, ..)) => name == other_name,
			(Self::Number(value, ..), Self::Number(other_value, ..)) => value == other_value,
			(Self::Register(register, ..), Self::Register(other_register, ..)) => register == other_register,
			(Self::Mnemonic(mnemonic, ..), Self::Mnemonic(other_mnemonic, ..)) => mnemonic == other_mnemonic,
			(Self::Colon(..), Self::Colon(..))
			| (Self::OpenParenthesis(..), Self::OpenParenthesis(..))
			| (Self::CloseParenthesis(..), Self::CloseParenthesis(..))
			| (Self::Hash(..), Self::Hash(..))
			| (Self::Plus(..), Self::Plus(..))
			| (Self::Slash(..), Self::Slash(..))
			| (Self::Newline(..), Self::Newline(..))
			| (Self::Comma(..), Self::Comma(..))
			| (Self::Period(..), Self::Period(..)) => true,
			#[cfg(test)]
			(Self::TestComment(contents, ..), Self::TestComment(other_contents, ..)) => contents == other_contents,
			_ => false,
		}
	}
}

impl Token {
	/// Returns this token if it matches the given type. For all tokens that contain data, this data is ignored.
	///
	/// # Errors
	/// If the token doesn't match, an "Expected XYZ" error string is returned.
	pub fn expect(self, type_: &Self, src: Arc<AssemblyCode>) -> Result<Self, AssemblyError> {
		if self.equals_type(type_) {
			Ok(self)
		} else {
			Err(AssemblyError::ExpectedToken {
				expected: self.clone(),
				actual: type_.clone(),
				location: self.source_span(),
				src,
			})
		}
	}

	/// Checks whether the two tokens equal in type. For the most part, this ignores token-internal data, especially
	/// source spans. It does however consider register and macro enum types, as those are finite unlike identifier
	/// strings.
	#[must_use]
	pub fn equals_type(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Identifier(..), Self::Identifier(..))
			| (Self::Macro(..), Self::Macro(..))
			| (Self::Number(..), Self::Number(..))
			| (Self::Colon(..), Self::Colon(..))
			| (Self::OpenParenthesis(..), Self::OpenParenthesis(..))
			| (Self::CloseParenthesis(..), Self::CloseParenthesis(..))
			| (Self::Hash(..), Self::Hash(..))
			| (Self::Plus(..), Self::Plus(..))
			| (Self::Slash(..), Self::Slash(..))
			| (Self::Newline(..), Self::Newline(..))
			| (Self::Comma(..), Self::Comma(..))
			| (Self::Period(..), Self::Period(..)) => true,
			(Self::Register(first, ..), Self::Register(second, ..)) => first == second,
			(Self::Mnemonic(first, ..), Self::Mnemonic(second, ..)) => first == second,
			#[cfg(test)]
			(Self::TestComment(..), Self::TestComment(..)) => true,
			_ => false,
		}
	}

	/// Returns the source span where this token is located in the file.
	#[must_use]
	pub fn source_span(&self) -> SourceSpan {
		match self {
			Self::Hash(location)
			| Self::CloseParenthesis(location)
			| Self::Colon(location)
			| Self::Comma(location)
			| Self::Equals(location)
			| Self::Newline(location)
			| Self::OpenParenthesis(location)
			| Self::Period(location)
			| Self::Slash(location)
			| Self::Plus(location) => (*location, SourceOffset::from(1)).into(),
			Self::Identifier(_, location)
			| Self::Number(_, location)
			| Self::Register(_, location)
			| Self::Mnemonic(_, location)
			| Self::Macro(_, location) => *location,
			#[cfg(test)]
			Self::TestComment(_, location) => *location,
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		if let Self::Mnemonic(mnemonic, ..) = self {
			return write!(f, "{}", mnemonic);
		};
		write!(f, "{}", match self {
			Self::Identifier(..) => "identifier",
			Self::Register(..) => "register name",
			Self::Macro(..) => "macro",
			Self::Number(..) => "number",
			Self::Hash(..) => "hash",
			Self::Comma(..) => "comma",
			Self::Period(..) => "'.'",
			Self::Plus(..) => "'+'",
			Self::Equals(..) => "'='",
			Self::OpenParenthesis(..) => "'('",
			Self::CloseParenthesis(..) => "')'",
			Self::Slash(..) => "'/'",
			Self::Newline(..) => "new line",
			Self::Colon(..) => "':'",
			#[cfg(test)]
			Self::TestComment(..) => "test comment (';=')",
			_ => unreachable!(),
		})
	}
}
#[derive(Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct TokenStream<'a> {
	pub tokens: &'a [Token],
	pub index:  usize,
	source:     Arc<AssemblyCode>,
}

impl<'a> TokenStream<'a> {
	pub fn new<'local>(tokens: &'a [Token], source: &'local Arc<AssemblyCode>) -> Self {
		Self { tokens, index: 0, source: source.clone() }
	}

	pub const fn len(&self) -> usize {
		self.tokens.len()
	}

	pub const fn remaining(&self) -> usize {
		self.len() - self.index
	}

	pub const fn is_end(&self) -> bool {
		self.index >= self.len()
	}

	pub fn next(&mut self) -> Result<Token, AssemblyError> {
		if self.is_end() {
			Err(AssemblyError::UnexpectedEndOfTokens {
				expected: TokenOrString::String("a token".to_string()),
				location: self.tokens.last().map_or((0, 1).into(), Token::source_span),
				src:      self.source.clone(),
			})
		} else {
			self.index += 1;
			Ok(self.tokens[self.index - 1].clone())
		}
	}

	pub fn expect(&mut self, expected: &Token) -> Result<Token, AssemblyError> {
		self.next()?.expect(expected, self.source.clone())
	}

	/// Passes the next token into the function for checking.
	pub fn expect_and_map(
		&mut self,
		predicate: impl FnOnce(Token) -> Result<Token, AssemblyError>,
	) -> Result<Token, AssemblyError> {
		predicate(self.next()?)
	}

	pub fn lookahead<const Amount: usize>(&self) -> Result<[Token; Amount], AssemblyError> {
		let token_span =
			self.tokens.get(self.index .. self.index + Amount).ok_or_else(|| AssemblyError::UnexpectedEndOfTokens {
				expected: TokenOrString::String(format!("{} tokens", Amount)),
				location: self.tokens.last().map_or((0, 1).into(), Token::source_span),
				src:      self.source.clone(),
			})?;
		// HACK: This avoids initializing the array with dummy tokens, which should be faster.
		let mut token_array = MaybeUninit::uninit_array();
		for (i, token) in token_array.iter_mut().enumerate() {
			token.write(token_span[i].clone());
		}
		Ok(unsafe { MaybeUninit::array_assume_init(token_array) })
	}

	pub fn end(&self) -> Result<&Token, AssemblyError> {
		self.tokens.last().ok_or_else(|| AssemblyError::UnexpectedEndOfTokens {
			expected: TokenOrString::String("a token".to_string()),
			location: (0, 1).into(),
			src:      self.source.clone(),
		})
	}

	pub fn backtrack(&mut self, amount: usize) {
		self.index = self.index.checked_sub(amount).expect("backtrack beyond token stream beginning");
	}

	/// Move the token stream to this index.
	///
	/// **Use this API with great caution.**
	pub fn move_to(&mut self, index: usize) {
		self.index = index.min(self.tokens.len() - 1);
	}

	/// Create a substream that is almost identical, but all the previous visited tokens are cut off.
	pub fn make_substream(&self) -> Self {
		Self { tokens: &self.tokens[self.index ..], index: 0, source: self.source.clone() }
	}

	/// Limit the stream to the first token of the given type. If this token is never found, the stream size is
	/// unchanged.
	pub fn limit_to_first<'local>(&mut self, type_: &'local Token) -> &Self {
		let mut current_index = self.index;
		while let Some(next_token) = self.tokens.get(current_index ..= current_index) {
			if next_token[0].equals_type(type_) {
				break;
			}
			current_index += 1;
		}
		self.tokens = &self.tokens[.. current_index];

		self
	}

	pub fn advance_past_other(&mut self, other_stream: &Self) -> Result<&Self, AssemblyError> {
		let other_token = &other_stream.lookahead::<1>()?[0];
		while let Ok(next_token) = self.next() {
			if next_token == *other_token {
				break;
			}
		}

		Ok(self)
	}

	/// Advance this stream so that it is placed at the other stream's end. This
	/// assumes that the streams start at the same place in the same physical
	/// token slice, but have a different end point; the first stream was
	/// carefully resized and the second stream should now start where the first
	/// stream ends.
	pub fn advance_to_others_end(&mut self, other_stream: &Self) -> Result<&Self, AssemblyError> {
		for _ in 0 .. other_stream.remaining() {
			self.next()?;
		}
		Ok(self)
	}

	pub fn iter(&self) -> impl Iterator<Item = &Token> {
		self.tokens.iter()
	}
}
