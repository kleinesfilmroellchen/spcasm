use std::fmt::Display;
use std::num::ParseIntError;
use std::sync::Arc;

use miette::{Diagnostic, MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use thiserror::Error;

use crate::instruction::{AddressingMode, MemoryAddress, Mnemonic};
use crate::Token;

/// The source code for an assembly error.
#[derive(Debug, Clone)]
pub struct AssemblyCode {
	pub text: String,
	pub name: String,
}

impl SourceCode for AssemblyCode {
	fn read_span<'a>(
		&'a self,
		span: &SourceSpan,
		context_lines_before: usize,
		context_lines_after: usize,
	) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
		let result = self.text.read_span(span, context_lines_before, context_lines_after)?;
		let retval = Box::new(MietteSpanContents::new_named(
			self.name.clone(),
			result.data(),
			*result.span(),
			result.line(),
			result.column(),
			result.line_count(),
		));
		Ok(retval)
	}
}

/// All types of errors that the assembler can report to the user.
#[derive(Error, Debug, Diagnostic)]
#[allow(clippy::module_name_repetitions)]
pub enum AssemblyError {
	//#region Semantic errors: detected while parsing or assembling
	#[error("Invalid addressing mode `{mode}` as {} operand for `{mnemonic}`", if *.is_first_operand { "first" } else { "second" })]
	#[diagnostic(
		code(spcasm::invalid_addressing_mode),
		help("The instruction `{mnemonic}` accepts the modes {} here", .legal_modes.iter().map(|mode| format!("{} ", mode)).collect::<String>().trim()),
	)]
	InvalidAddressingMode {
		mode:             AddressingMode,
		is_first_operand: bool,
		mnemonic:         Mnemonic,
		legal_modes:      Vec<AddressingMode>,
		#[source_code]
		src:              Arc<AssemblyCode>,
		#[label("For this instruction")]
		location:         SourceSpan,
	},

	#[error("Section at {section_start:04x} starts after the end of the previous one, which is {section_end:04x}")]
	#[diagnostic(code(spcasm::syntax::expected_token))]
	SectionMismatch {
		section_start: MemoryAddress,
		section_end:   MemoryAddress,
		#[label("Unexpected")]
		location:      SourceSpan,
		#[source_code]
		src:           Arc<AssemblyCode>,
	},

	#[error("Invalid addressing mode combination: `{first_mode}` with `{second_mode}` for `{mnemonic}`")]
	#[diagnostic(code(spcasm::invalid_addressing_mode))]
	InvalidAddressingModeCombination {
		first_mode:  AddressingMode,
		second_mode: AddressingMode,
		mnemonic:    Mnemonic,
		#[source_code]
		src:         Arc<AssemblyCode>,
		#[label("Addressing mode invalid")]
		location:    SourceSpan,
	},

	#[error("Two operands are not allowed for `{mnemonic}`")]
	#[diagnostic(code(spcasm::two_operands_not_allowed), help("Remove the second operand"))]
	TwoOperandsNotAllowed {
		mnemonic: Mnemonic,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Only takes 1 operand")]
		location: SourceSpan,
	},

	#[error("`{mnemonic}` doesn't take any operands")]
	#[diagnostic(code(spcasm::operand_not_allowed), help("Remove the operands of this instruction"))]
	OperandNotAllowed {
		mnemonic: Mnemonic,
		#[label("Takes 0 operands")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("`{constant}` is not valid for {typename}")]
	#[diagnostic(code(spcasm::invalid_constant), help("Remove the operands of this instruction"))]
	InvalidConstant {
		constant: String,
		typename: String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Takes 0 operands")]
		location: SourceSpan,
	},

	#[cfg(test)]
	#[error("Test assembly doesn't have an expected output comment")]
	#[diagnostic(code(spcasm::missing_test_result))]
	MissingTestResult {
		#[label("Must have a ';=' comment")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	//#endregion
	//#region Syntax errors: detected in the lexer and mainly the parser
	#[error("Expected {expected}")]
	#[diagnostic(code(spcasm::syntax::expected_token))]
	ExpectedToken {
		expected: Token,
		actual:   Token,
		#[label("This {actual} is invalid here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Invalid number")]
	#[diagnostic(code(spcasm::syntax::expected_token))]
	InvalidNumber {
		error:    ParseIntError,
		#[label("{error}")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Expected {expected}")]
	#[diagnostic(code(spcasm::syntax::expected_token))]
	UnexpectedEndOfTokens {
		expected: TokenOrString,
		#[label("There should be a token here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Unexpected character {chr}")]
	#[diagnostic(code(spcasm::syntax::expected_token))]
	UnexpectedCharacter {
		chr:      char,
		#[label("Unexpected")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Single '#' is not a valid addressing mode")]
	#[diagnostic(code(spcasm::syntax::single_hash_invalid), help("Add a number to make this an immediate operand"))]
	SingleHashInvalid {
		#[label("This is not an addressing mode")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Invalid token `{token}` for indexing")]
	#[diagnostic(code(spcasm::syntax::invalid_indexing_token), help("Use `X` or `Y` for indexing"))]
	InvalidIndexingToken {
		token:    Token,
		#[label("This token is not valid for indexing")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},
	#[cfg(test)]
	#[error("Test comment has invalid format")]
	#[diagnostic(
		code(spcasm::syntax::invalid_test_comment),
		help(
			"Test comments consist of a series of space-delimited bytes, given as hexadecimal, for example `;= 0F AA B8` \
			 for three bytes"
		)
	)]
	InvalidTestComment {
		#[label("This ';=' comment is invalid: {}", basis.clone().or_else(|| "".parse::<u8>().err()).unwrap())]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
		basis:    Option<ParseIntError>,
	},
	//#endregion
}

#[derive(Clone, Debug)]
pub enum TokenOrString {
	Token(Token),
	String(String),
}

impl Display for TokenOrString {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", match self {
			Self::Token(token) => format!("{}", token),
			Self::String(string) => string.clone(),
		})
	}
}

impl From<String> for TokenOrString {
	fn from(string: String) -> Self {
		Self::String(string)
	}
}

impl From<&str> for TokenOrString {
	fn from(string: &str) -> Self {
		Self::String(string.to_owned())
	}
}

impl From<Token> for TokenOrString {
	fn from(string: Token) -> Self {
		Self::Token(string)
	}
}
