//! The Token struct.

use std::fmt::Display;

use miette::{SourceOffset, SourceSpan};
#[allow(unused)]
use smartstring::alias::String;

use crate::directive::DirectiveSymbol;
use crate::parser::instruction::Mnemonic;
use crate::parser::Register;
use crate::{AssemblyCode, AssemblyError};

/// Assembly language tokens.
#[derive(Debug, Clone)]
pub enum Token {
	/// Mnemonic, the start of an instruction.
	Mnemonic(Mnemonic, SourceSpan),
	/// Identifier, i.e. a reference.
	Identifier(String, SourceSpan),
	/// A "special" identifier used in some directives as a keyword.
	SpecialIdentifier(&'static str, SourceSpan),
	/// Register name (this can never be used as an identifier).
	Register(Register, SourceSpan),
	/// + Register name; necessary for LALRPOP.
	PlusRegister(Register, SourceSpan),
	/// Start of a directive.
	Directive(DirectiveSymbol, SourceSpan),
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
	/// '**'
	DoubleStar(SourceSpan),
	/// '/'
	Slash(SourceOffset),
	/// '|'
	Pipe(SourceOffset),
	/// '&'
	Ampersand(SourceOffset),
	/// '~'
	Tilde(SourceOffset),
	/// '^'
	Caret(SourceOffset),
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
	/// '<'
	OpenAngleBracket(SourceOffset),
	/// '<<'
	DoubleOpenAngleBracket(SourceSpan),
	/// '>'
	CloseAngleBracket(SourceOffset),
	/// '>>'
	DoubleCloseAngleBracket(SourceSpan),
	/// '%'
	Percent(SourceOffset),
	/// '.b'
	ExplicitDirectPage(SourceSpan),
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
			(Self::Directive(name, ..), Self::Directive(other_name, ..)) => name == other_name,
			(Self::Number(value, ..), Self::Number(other_value, ..)) => value == other_value,
			(Self::Register(register, ..), Self::Register(other_register, ..)) => register == other_register,
			(Self::PlusRegister(register, ..), Self::PlusRegister(other_register, ..)) => register == other_register,
			(Self::Mnemonic(mnemonic, ..), Self::Mnemonic(other_mnemonic, ..)) => mnemonic == other_mnemonic,
			(Self::SpecialIdentifier(text, ..), Self::SpecialIdentifier(other_text, ..)) => text == other_text,
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
			| (Self::Ampersand(..), Self::Ampersand(..))
			| (Self::Caret(..), Self::Caret(..))
			| (Self::CloseAngleBracket(..), Self::CloseAngleBracket(..))
			| (Self::OpenAngleBracket(..), Self::OpenAngleBracket(..))
			| (Self::DoubleOpenAngleBracket(..), Self::DoubleOpenAngleBracket(..))
			| (Self::DoubleCloseAngleBracket(..), Self::DoubleCloseAngleBracket(..))
			| (Self::Percent(..), Self::Percent(..))
			| (Self::Pipe(..), Self::Pipe(..))
			| (Self::Tilde(..), Self::Tilde(..))
			| (Self::DoubleStar(..), Self::DoubleStar(..))
			| (Self::RangeMinus(..), Self::RangeMinus(..))
			| (Self::Slash(..), Self::Slash(..))
			| (Self::Star(..), Self::Star(..))
			| (Self::Newline(..), Self::Newline(..))
			| (Self::ExplicitDirectPage(..), Self::ExplicitDirectPage(..))
			| (Self::Comma(..), Self::Comma(..))
			| (Self::Period(..), Self::Period(..)) => true,
			#[cfg(test)]
			(Self::TestComment(contents, ..), Self::TestComment(other_contents, ..)) => contents == other_contents,
			_ => false,
		}
	}
}

#[allow(clippy::match_same_arms, clippy::missing_const_for_fn)]
impl Token {
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
			| Self::OpenAngleBracket(location)
			| Self::CloseAngleBracket(location)
			| Self::Percent(location)
			| Self::Slash(location)
			| Self::Pipe(location)
			| Self::Tilde(location)
			| Self::Caret(location)
			| Self::Ampersand(location)
			| Self::Plus(location) => (*location, SourceOffset::from(1)).into(),
			Self::Identifier(_, location)
			| Self::ExplicitDirectPage(location)
			| Self::Number(_, location)
			| Self::Register(_, location)
			| Self::String(_, location)
			| Self::PlusRegister(_, location)
			| Self::DoubleStar(location)
			| Self::DoubleOpenAngleBracket(location)
			| Self::DoubleCloseAngleBracket(location)
			| Self::Mnemonic(_, location)
			| Self::SpecialIdentifier(_, location)
			| Self::Directive(_, location) => *location,
			// #[cfg(test)]
			Self::TestComment(_, location) => *location,
		}
	}

	/// Parse a special identifier, mainly validating that the identifier given is one of the special identifiers.
	pub fn parse_special_identifier(
		identifier: &str,
		span: SourceSpan,
		src: std::sync::Arc<AssemblyCode>,
	) -> Result<&'static str, AssemblyError> {
		match identifier {
			"offset" => Ok("offset"),
			"align" => Ok("align"),
			_ => Err(AssemblyError::ExpectedToken {
				expected: String::from("identifier").into(),
				actual: Token::Identifier(identifier.into(), span),
				location: span,
				src,
			}),
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
			Self::SpecialIdentifier(name, ..) => name,
			Self::String(..) => "string",
			Self::Register(..) => "register name",
			Self::PlusRegister(..) => "'+' register name",
			Self::Directive(..) => "directive",
			Self::Number(..) => "number",
			Self::Hash(..) => "hash",
			Self::Comma(..) => "comma",
			Self::Period(..) => "'.'",
			Self::ExplicitDirectPage(..) => "'.b'",
			Self::Plus(..) => "'+'",
			Self::Minus(..) | Self::RangeMinus(..) => "'-'",
			Self::Star(..) => "'*'",
			Self::Tilde(..) => "'~'",
			Self::DoubleStar(..) => "'**'",
			Self::OpenAngleBracket(..) => "'<'",
			Self::CloseAngleBracket(..) => "'>'",
			Self::DoubleOpenAngleBracket(..) => "'<<'",
			Self::DoubleCloseAngleBracket(..) => "'>>'",
			Self::Percent(..) => "'%'",
			Self::Equals(..) => "'='",
			Self::CloseParenthesis(..) | Self::CloseIndexingParenthesis(..) => "')'",
			Self::OpenIndexingParenthesis(..) | Self::OpenParenthesis(..) => "'('",
			Self::Slash(..) => "'/'",
			Self::Pipe(..) => "'|'",
			Self::Ampersand(..) => "'&'",
			Self::Caret(..) => "'^'",
			Self::Newline(..) => "new line",
			Self::Colon(..) => "':'",
			// #[cfg(test)]
			Self::TestComment(..) => "test comment (';=')",
			Self::Mnemonic(..) => unreachable!(),
		})
	}
}
