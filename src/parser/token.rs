//! The Token struct.

use std::fmt::Display;
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};

use crate::error::{AssemblyCode, AssemblyError};
use crate::mcro::MacroSymbol;
use crate::parser::instruction::Mnemonic;
use crate::parser::Register;

/// Assembly language tokens.
#[derive(Debug, Clone)]
pub enum Token {
	/// Mnemonic, the start of an instruction.
	Mnemonic(Mnemonic, SourceSpan),
	/// Identifier, i.e. a label.
	Identifier(String, SourceSpan),
	/// Register name (this can never be used as an identifier).
	Register(Register, SourceSpan),
	/// + Register name; necessary for LALRPOP.
	PlusRegister(Register, SourceSpan),
	/// Start of a macro.
	Macro(MacroSymbol, SourceSpan),
	/// Literal number which was already parsed.
	Number(i64, SourceSpan),
	/// Text string delimited by "".
	String(Vec<u8>, SourceSpan),
	/// '#'
	Hash(SourceOffset),
	/// ','
	Comma(SourceOffset),
	/// '+'
	Plus(SourceOffset),
	/// '-'
	Minus(SourceOffset),
	/// '-' but used in range expressions like for 'incbin'
	RangeMinus(SourceOffset),
	/// '*'
	Star(SourceOffset),
	/// '/'
	Slash(SourceOffset),
	/// '('
	OpenParenthesis(SourceOffset),
	/// ')'
	CloseParenthesis(SourceOffset),
	/// '(' but used for indexing addressing mode (necessary for LALRPOP)
	OpenIndexingParenthesis(SourceOffset),
	/// ')' but used for indexing addressing mode (necessary for LALRPOP)
	CloseIndexingParenthesis(SourceOffset),
	/// ':'
	Colon(SourceOffset),
	/// '.'
	Period(SourceOffset),
	/// '='
	Equals(SourceOffset),
	/// ASCII newline (\n).
	Newline(SourceOffset),
	/// Comments used for testing purposes.
	// #[cfg(test)]
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
			(Self::PlusRegister(register, ..), Self::PlusRegister(other_register, ..)) => register == other_register,
			(Self::Mnemonic(mnemonic, ..), Self::Mnemonic(other_mnemonic, ..)) => mnemonic == other_mnemonic,
			(Self::String(text, ..), Self::String(other_text, ..)) => text == other_text,
			(Self::Colon(..), Self::Colon(..))
			| (Self::OpenParenthesis(..), Self::OpenParenthesis(..))
			| (Self::OpenIndexingParenthesis(..), Self::OpenIndexingParenthesis(..))
			| (Self::CloseParenthesis(..), Self::CloseParenthesis(..))
			| (Self::CloseIndexingParenthesis(..), Self::CloseIndexingParenthesis(..))
			| (Self::Hash(..), Self::Hash(..))
			| (Self::Plus(..), Self::Plus(..))
			| (Self::Equals(..), Self::Equals(..))
			| (Self::Minus(..), Self::Minus(..))
			| (Self::RangeMinus(..), Self::RangeMinus(..))
			| (Self::Slash(..), Self::Slash(..))
			| (Self::Star(..), Self::Star(..))
			| (Self::Newline(..), Self::Newline(..))
			| (Self::Comma(..), Self::Comma(..))
			| (Self::Period(..), Self::Period(..)) => true,
			#[cfg(test)]
			(Self::TestComment(contents, ..), Self::TestComment(other_contents, ..)) => contents == other_contents,
			_ => false,
		}
	}
}

#[allow(clippy::match_same_arms)]
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
			| (Self::OpenIndexingParenthesis(..), Self::OpenIndexingParenthesis(..))
			| (Self::CloseIndexingParenthesis(..), Self::CloseIndexingParenthesis(..))
			| (Self::Hash(..), Self::Hash(..))
			| (Self::Plus(..), Self::Plus(..))
			| (Self::Star(..), Self::Star(..))
			| (Self::String(..), Self::String(..))
			| (Self::Minus(..), Self::Minus(..))
			| (Self::RangeMinus(..), Self::RangeMinus(..))
			| (Self::Slash(..), Self::Slash(..))
			| (Self::Newline(..), Self::Newline(..))
			| (Self::Comma(..), Self::Comma(..))
			| (Self::Period(..), Self::Period(..)) => true,
			(Self::Register(first, ..), Self::Register(second, ..)) => first == second,
			(Self::PlusRegister(first, ..), Self::PlusRegister(second, ..)) => first == second,
			(Self::Mnemonic(first, ..), Self::Mnemonic(second, ..)) => first == second,
			// #[cfg(test)]
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
			| Self::CloseIndexingParenthesis(location)
			| Self::Colon(location)
			| Self::Comma(location)
			| Self::Equals(location)
			| Self::Star(location)
			| Self::Minus(location)
			| Self::RangeMinus(location)
			| Self::Newline(location)
			| Self::OpenParenthesis(location)
			| Self::OpenIndexingParenthesis(location)
			| Self::Period(location)
			| Self::Slash(location)
			| Self::Plus(location) => (*location, SourceOffset::from(1)).into(),
			Self::Identifier(_, location)
			| Self::Number(_, location)
			| Self::Register(_, location)
			| Self::String(_, location)
			| Self::PlusRegister(_, location)
			| Self::Mnemonic(_, location)
			| Self::Macro(_, location) => *location,
			// #[cfg(test)]
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
			Self::String(..) => "string",
			Self::Register(..) => "register name",
			Self::PlusRegister(..) => "'+' register name",
			Self::Macro(..) => "macro",
			Self::Number(..) => "number",
			Self::Hash(..) => "hash",
			Self::Comma(..) => "comma",
			Self::Period(..) => "'.'",
			Self::Plus(..) => "'+'",
			Self::Minus(..) | Self::RangeMinus(..) => "'-'",
			Self::Star(..) => "'*'",
			Self::Equals(..) => "'='",
			Self::CloseParenthesis(..) | Self::CloseIndexingParenthesis(..) => "')'",
			Self::OpenIndexingParenthesis(..) | Self::OpenParenthesis(..) => "'('",
			Self::Slash(..) => "'/'",
			Self::Newline(..) => "new line",
			Self::Colon(..) => "':'",
			// #[cfg(test)]
			Self::TestComment(..) => "test comment (';=')",
			Self::Mnemonic(..) => unreachable!(),
		})
	}
}
