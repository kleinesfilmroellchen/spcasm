//! Assembly directives and macros.
#![allow(clippy::module_name_repetitions)]
use std::fmt::Display;
use std::sync::Arc;

use miette::SourceSpan;
use spcasm_derive::Parse;

use crate::error::AssemblyError;
use crate::instruction::{MemoryAddress, Number};
use crate::label::GlobalLabel;
use crate::parser::Environment;
use crate::token::TokenStream;

/// An assembly macro.
#[derive(Clone, Debug)]
pub struct Macro {
	/// Actual data of the macro.
	pub value:       MacroValue,
	pub(crate) span: SourceSpan,
}

/// Macro symbols, used in lexing.
#[derive(Debug, Clone, Copy, Parse, Eq, PartialEq)]
pub enum MacroSymbol {
	Org,
}

impl Display for MacroSymbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::Org => "org",
		})
	}
}

/// An assembly macro's value and relevant data.
#[derive(Clone, Debug)]
pub enum MacroValue {
	/// org <memory address>
	Org(MemoryAddress),
}

impl Macro {
	/// Parse a macro from a macro symbol and a line of text containing parameters for the macro.
	/// # Errors
	/// Any parser error.
	pub fn parse_macro(
		environment: &mut Environment,
		symbol: MacroSymbol,
		span: SourceSpan,
		mut remaining_line: TokenStream,
		current_global_label: Option<Arc<GlobalLabel>>,
	) -> Result<Self, AssemblyError> {
		Ok(Self {
			value: match symbol {
				MacroSymbol::Org => {
					let initial_span = remaining_line.lookahead(1)?[0].source_span();
					let number = environment.parse_number(&mut remaining_line, current_global_label)?.try_resolve();
					match number {
						Number::Literal(value) => MacroValue::Org(value),
						_ => {
							return Err(AssemblyError::LabelsInMacroArgument {
								// TODO: not long enough for complex expressions
								argument_location: initial_span,
								location:          span,
								r#macro:           symbol,
								src:               environment.source_code.clone(),
							});
						},
					}
				},
				_ => unimplemented!(),
			},
			span,
		})
	}
}
