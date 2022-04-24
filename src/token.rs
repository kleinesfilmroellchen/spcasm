//! The Token struct.

use std::fmt::Display;
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};

use crate::error::{AssemblyCode, AssemblyError};
use crate::Register;

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
	/// '.'
	Period(SourceOffset),
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
			Self::Period(..) => match type_ {
				Self::Period(..) => Ok(self),
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
			| Self::Period(location)
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
			Self::Period(..) => "'.'",
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
