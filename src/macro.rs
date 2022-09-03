//! Assembly directives and macros.
#![allow(clippy::module_name_repetitions)]
use std::fmt::Display;
use std::sync::Arc;

use miette::SourceSpan;
use spcasm_derive::Parse;

use crate::error::AssemblyError;
use crate::instruction::{MemoryAddress, Number};
use crate::label::{GlobalLabel, Label};
use crate::parser::Environment;
use crate::token::TokenStream;
use crate::Token;

/// An assembly macro.
#[derive(Clone, Debug)]
pub struct Macro {
	/// Actual data of the macro.
	pub value:       MacroValue,
	pub(crate) span: SourceSpan,
	/// Label at the start of the macro. Some macros ignore this.
	pub label:       Option<Label>,
}

/// Macro symbols, used in lexing.
#[derive(Debug, Clone, Copy, Parse, Eq, PartialEq)]
pub enum MacroSymbol {
	Org,
	Db,
	Byte,
	Dw,
	Word,
	Dl,
	Dd,
	Ascii,
	Asciiz,
}

impl Display for MacroSymbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::Org => "org",
			Self::Db | Self::Byte => "db",
			Self::Dw | Self::Word => "dw",
			Self::Dl => "dl",
			Self::Dd => "dd",
			Self::Ascii => "ascii",
			Self::Asciiz => "asciiz",
		})
	}
}

/// An assembly macro's value and relevant data.
#[derive(Clone, Debug)]
pub enum MacroValue {
	/// org <memory address>
	Org(MemoryAddress),
	/// Various table macros, such as byte/db, word/dw, dl, dd, ascii(z), ...
	/// dw <16-bit word>
	Table {
		/// The entries of the table. For simple macros like "dw $0A", this only has one entry.
		values:     Vec<Number>,
		/// How many bytes each entry occupies; depends on the specific macro used.
		entry_size: u8,
	},
}

impl Macro {
	/// Parse a macro from a macro symbol and a line of text containing parameters for the macro.
	/// # Errors
	/// Any parser error.
	/// # Panics
	pub fn parse_macro(
		environment: &mut Environment,
		symbol: MacroSymbol,
		span: SourceSpan,
		mut remaining_line: TokenStream,
		label_for_macro: Option<Label>,
		current_global_label: Option<Arc<GlobalLabel>>,
	) -> Result<Self, AssemblyError> {
		Ok(Self {
			value: match symbol {
				MacroSymbol::Org => {
					let initial_span = remaining_line.lookahead::<1>()?[0].source_span();
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
				symbol @ (MacroSymbol::Db
				| MacroSymbol::Byte
				| MacroSymbol::Dw
				| MacroSymbol::Word
				| MacroSymbol::Dl
				| MacroSymbol::Dd) => {
					let mut values = Vec::new();
					loop {
						match environment.parse_number(&mut remaining_line, current_global_label.clone()) {
							Ok(number) => values.push(number),
							Err(AssemblyError::UnexpectedEndOfTokens { .. }) => break,
							Err(err) => return Err(err),
						}
						// No comma means we need to stop parsing values.
						if remaining_line.expect(&Token::Comma(0.into())).is_err() {
							break;
						}
					}
					MacroValue::Table {
						values,
						entry_size: match symbol {
							MacroSymbol::Byte | MacroSymbol::Db => 1,
							MacroSymbol::Dw | MacroSymbol::Word => 2,
							MacroSymbol::Dl => 3,
							MacroSymbol::Dd => 4,
							_ => unreachable!(),
						},
					}
				},
				MacroSymbol::Ascii => todo!(),
				MacroSymbol::Asciiz => todo!(),
			},
			span,
			label: label_for_macro,
		})
	}
}
