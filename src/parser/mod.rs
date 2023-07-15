//! Parser infrastructure & communication with LALRPOP.

pub(crate) mod lalrpop_adaptor;
mod lexer;
mod token;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};
pub use lexer::*;
use miette::{SourceOffset, SourceSpan};
pub use token::Token;

use crate::directive::DirectiveValue;
use crate::sema::AssemblyTimeValue;
use crate::{AssemblyCode, AssemblyError};

lalrpop_mod!(
	#[allow(missing_docs, unused, clippy::all, clippy::pedantic, clippy::nursery)]
	asm,
	"/parser/asm.rs"
);

#[allow(clippy::module_name_repetitions)]
pub use asm::ProgramParser;

/// Anything that can be primitively parsed from a string into an enum variant.
/// This trait is intended to be derived with the macro from [`spcasm_derive`].
pub trait Parse
where
	Self: Sized,
{
	/// Parse this enum from the string representation.
	/// # Errors
	/// If the string doesn't correspond with any enum variant.
	fn parse(value: &str, location: SourceSpan, src: Arc<AssemblyCode>) -> Result<Self, Box<AssemblyError>>;

	/// Returns whether this string corresponds with an enum variant; i.e. parsing would succeed.
	fn is_valid(value: &str) -> bool;
}

/// A simple union type for source spans and (zero-width) source offsets.
///
/// Used by LALRPOP action code to create source spans more easily.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum SpanOrOffset {
	///
	Span(SourceSpan),
	///
	Offset(SourceOffset),
}

impl Default for SpanOrOffset {
	fn default() -> Self {
		Self::Offset(0.into())
	}
}

impl From<SourceOffset> for SpanOrOffset {
	fn from(offset: SourceOffset) -> Self {
		Self::Offset(offset)
	}
}

impl From<SourceSpan> for SpanOrOffset {
	fn from(span: SourceSpan) -> Self {
		Self::Span(span)
	}
}

impl From<&SourceSpan> for SpanOrOffset {
	fn from(span: &SourceSpan) -> Self {
		Self::Span(*span)
	}
}

#[allow(clippy::from_over_into)]
impl Into<SourceSpan> for SpanOrOffset {
	fn into(self) -> SourceSpan {
		match self {
			Self::Span(span) => span,
			Self::Offset(offset) => (offset, 0.into()).into(),
		}
	}
}

/// Creates a new source span from the given start and end source spans.
///
/// This is used by LALRPOP action code for constructing larger syntactic
/// elements that span multiple tokens or sub-elements.
#[must_use]
pub fn source_range(start: SpanOrOffset, end: SpanOrOffset) -> SourceSpan {
	let start: SourceSpan = start.into();
	let end: SourceSpan = end.into();
	(start.offset(), (end.offset() + end.len()).saturating_sub(start.offset())).into()
}

/// Creates the direct page addressing mode if the number is a legal direct page address.
///
/// This function is both generic over the value being passed (it must be convertible into a number) and the return type
/// of the handler functions. Typically, you want to use Result types with fallible handlers and the
/// [`crate::sema::AddressingMode`] type with non-fallible handlers, but the function is agnostic to that.
pub fn try_make_direct_page_addressing_mode<T, ReturnType>(
	value: T,
	dp_mode: impl FnOnce(T) -> ReturnType,
	non_dp_mode: impl FnOnce(T) -> ReturnType,
) -> ReturnType
where
	T: Into<AssemblyTimeValue> + Clone,
{
	let number: AssemblyTimeValue = value.clone().into().try_resolve();
	match number {
		AssemblyTimeValue::Literal(literal, ..) if literal <= 0xFF => dp_mode(value),
		_ => non_dp_mode(value),
	}
}

/// Apply the given list of options to a BRR directive, and report errors if necessary. This function is called from
/// parser generator action code.
///
/// # Errors
/// An invalid option was provided.
#[allow(clippy::result_large_err)] // Used by LALRPOP
pub fn apply_brr_options(
	directive_location: SourceSpan,
	source_code: &Arc<AssemblyCode>,
	mut value: DirectiveValue,
	options: Vec<(SharedStr, SourceSpan)>,
) -> Result<DirectiveValue, AssemblyError> {
	match &mut value {
		DirectiveValue::Brr { auto_trim, directory, .. } => {
			for (option, option_location) in options {
				match &*option {
					"nodirectory" => *directory = false,
					"autotrim" => *auto_trim = true,
					_ =>
						return Err(AssemblyError::InvalidDirectiveOption {
							directive_location,
							option_location,
							option: option.clone(),
							directive: "brr".into(),
							valid_options: vec![shared_str!("nodirectory"), shared_str!("autotrim")],
							src: source_code.clone(),
						}),
				}
			}
			Ok(value)
		},
		_ => unreachable!(),
	}
}

/// Apply the given list of options to a sample table directive, and report errors if necessary. This function is called
/// from parser generator action code.
///
/// # Errors
/// An invalid option was provided.
#[allow(clippy::result_large_err)] // Used by LALRPOP
pub fn apply_sample_table_options(
	directive_location: SourceSpan,
	source_code: &Arc<AssemblyCode>,
	mut value: DirectiveValue,
	options: Vec<(SharedStr, SourceSpan)>,
) -> Result<DirectiveValue, AssemblyError> {
	match &mut value {
		DirectiveValue::SampleTable { auto_align } => {
			for (option, option_location) in options {
				match &*option {
					"noalign" => *auto_align = false,
					_ =>
						return Err(AssemblyError::InvalidDirectiveOption {
							directive_location,
							option_location,
							option: option.clone(),
							directive: "sampletable".to_owned().into(),
							valid_options: vec!["noalign".to_owned().into()],
							src: source_code.clone(),
						}),
				}
			}
			Ok(value)
		},
		_ => unreachable!(),
	}
}
