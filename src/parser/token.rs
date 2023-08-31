//! The Token struct.

use std::fmt::Display;
use std::num::NonZeroU64;

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};
use miette::{SourceOffset, SourceSpan};

use crate::directive::DirectiveSymbol;
use crate::sema::instruction::Mnemonic;
use crate::sema::Register;
use crate::{AssemblyCode, AssemblyError};

/// Assembly language tokens.
#[derive(Debug, Clone)]
pub enum Token {
	/// Mnemonic, the start of an instruction.
	Mnemonic(Mnemonic, SourceSpan),
	/// Identifier, i.e. a reference.
	Identifier(SharedStr, SourceSpan),
	/// A "special" identifier used in some directives as a keyword.
	SpecialIdentifier(&'static str, SourceSpan),
	/// Register name (this can never be used as an identifier).
	Register(Register, SourceSpan),
	/// + Register name; necessary for LALRPOP.
	PlusRegister(Register, SourceSpan),
	/// Start of a directive.
	Directive(DirectiveSymbol, SourceSpan),
	/// Literal number which was already parsed.
	Number(i64, SharedStr, SourceSpan),
	/// Text string delimited by "".
	String(Vec<u8>, SourceSpan),
	/// '#'
	Hash(SourceOffset),
	/// ','
	Comma(SourceOffset),
	/// '+'
	Plus(SourceOffset),
	/// Multiple '+' (or just one), when it's clear that they are used as a relative label.
	RelativeLabelPlus(NonZeroU64, SourceSpan),
	/// '-'
	Minus(SourceOffset),
	/// Multiple '-' (or just one), when it's clear that they are used as a relative label.
	RelativeLabelMinus(NonZeroU64, SourceSpan),
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
	/// '.w'
	ExplicitNoDirectPage(SourceSpan),
	/// '='
	Equals(SourceOffset),
	/// '>='
	OpenAngleBracketEquals(SourceSpan),
	/// '<='
	CloseAngleBracketEquals(SourceSpan),
	/// '!='
	ExclamationEquals(SourceSpan),
	/// '=='
	DoubleEquals(SourceSpan),
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
			| (Self::CloseAngleBracketEquals(..), Self::CloseAngleBracketEquals(..))
			| (Self::OpenAngleBracketEquals(..), Self::OpenAngleBracketEquals(..))
			| (Self::ExclamationEquals(..), Self::ExclamationEquals(..))
			| (Self::DoubleEquals(..), Self::DoubleEquals(..))
			| (Self::RelativeLabelMinus(..), Self::RelativeLabelMinus(..))
			| (Self::RelativeLabelPlus(..), Self::RelativeLabelPlus(..))
			| (Self::Percent(..), Self::Percent(..))
			| (Self::Pipe(..), Self::Pipe(..))
			| (Self::Tilde(..), Self::Tilde(..))
			| (Self::DoubleStar(..), Self::DoubleStar(..))
			| (Self::RangeMinus(..), Self::RangeMinus(..))
			| (Self::Slash(..), Self::Slash(..))
			| (Self::Star(..), Self::Star(..))
			| (Self::Newline(..), Self::Newline(..))
			| (Self::ExplicitDirectPage(..), Self::ExplicitDirectPage(..))
			| (Self::ExplicitNoDirectPage(..), Self::ExplicitNoDirectPage(..))
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
	#[inline]
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
			| Self::ExplicitNoDirectPage(location)
			| Self::Number(_, _, location)
			| Self::Register(_, location)
			| Self::String(_, location)
			| Self::PlusRegister(_, location)
			| Self::DoubleStar(location)
			| Self::DoubleOpenAngleBracket(location)
			| Self::DoubleCloseAngleBracket(location)
			| Self::OpenAngleBracketEquals(location)
			| Self::CloseAngleBracketEquals(location)
			| Self::DoubleEquals(location)
			| Self::ExclamationEquals(location)
			| Self::RelativeLabelMinus(_, location)
			| Self::RelativeLabelPlus(_, location)
			| Self::Mnemonic(_, location)
			| Self::SpecialIdentifier(_, location)
			| Self::Directive(_, location) => *location,
			Self::TestComment(_, location) => *location,
		}
	}

	/// Parse a special identifier, mainly validating that the identifier given is one of the special identifiers.
	///
	/// # Errors
	/// If the identifier is not a special identifier.
	pub fn parse_special_identifier(
		identifier: &str,
		span: SourceSpan,
		src: std::sync::Arc<AssemblyCode>,
	) -> Result<&'static str, Box<AssemblyError>> {
		match identifier {
			"offset" => Ok("offset"),
			"align" => Ok("align"),
			"startpos" => Ok("startpos"),
			_ => Err(AssemblyError::ExpectedToken {
				expected: shared_str!("identifier"),
				actual: Self::Identifier(identifier.into(), span),
				location: span,
				src,
			}
			.into()),
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		if let Self::Mnemonic(mnemonic, ..) = self {
			return write!(f, "{}", mnemonic);
		};
		write!(f, "{}", match self {
			Self::Identifier(..) => "identifier".to_string(),
			Self::SpecialIdentifier(name, ..) => (*name).to_string(),
			Self::String(..) => "string".to_string(),
			Self::Register(..) => "register name".to_string(),
			Self::PlusRegister(..) => "'+' register name".to_string(),
			Self::Directive(..) => "directive".to_string(),
			Self::Number(..) => "number".to_string(),
			Self::Hash(..) => "hash".to_string(),
			Self::Comma(..) => "comma".to_string(),
			Self::Period(..) => "'.'".to_string(),
			Self::ExplicitDirectPage(..) => "'.b'".to_string(),
			Self::ExplicitNoDirectPage(..) => "'.w'".to_string(),
			Self::Plus(..) => "'+'".to_string(),
			Self::RelativeLabelPlus(count, _) =>
				format!("'{}'", "+".repeat(usize::try_from(u64::from(*count)).unwrap_or(usize::MAX))),
			Self::RelativeLabelMinus(count, _) =>
				format!("'{}'", "-".repeat(usize::try_from(u64::from(*count)).unwrap_or(usize::MAX))),
			Self::Minus(..) | Self::RangeMinus(..) => "'-'".to_string(),
			Self::Star(..) => "'*'".to_string(),
			Self::Tilde(..) => "'~'".to_string(),
			Self::DoubleStar(..) => "'**'".to_string(),
			Self::OpenAngleBracket(..) => "'<'".to_string(),
			Self::CloseAngleBracket(..) => "'>'".to_string(),
			Self::DoubleOpenAngleBracket(..) => "'<<'".to_string(),
			Self::DoubleCloseAngleBracket(..) => "'>>'".to_string(),
			Self::OpenAngleBracketEquals(..) => "'>='".to_string(),
			Self::CloseAngleBracketEquals(..) => "'<='".to_string(),
			Self::ExclamationEquals(..) => "'!='".to_string(),
			Self::DoubleEquals(..) => "'=='".to_string(),
			Self::Percent(..) => "'%'".to_string(),
			Self::Equals(..) => "'='".to_string(),
			Self::CloseParenthesis(..) | Self::CloseIndexingParenthesis(..) => "')'".to_string(),
			Self::OpenIndexingParenthesis(..) | Self::OpenParenthesis(..) => "'('".to_string(),
			Self::Slash(..) => "'/'".to_string(),
			Self::Pipe(..) => "'|'".to_string(),
			Self::Ampersand(..) => "'&'".to_string(),
			Self::Caret(..) => "'^'".to_string(),
			Self::Newline(..) => "new line".to_string(),
			Self::Colon(..) => "':'".to_string(),
			Self::TestComment(..) => "test comment (';=')".to_string(),
			Self::Mnemonic(..) => unreachable!(),
		})
	}
}
