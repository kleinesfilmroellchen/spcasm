//! AST of the entire assembly program.
#![allow(clippy::module_name_repetitions, clippy::large_enum_variant)]
use miette::SourceSpan;

use super::instruction::Instruction;
use super::label::Label;
use super::Macro;
use crate::parser::source_range;

/// A program element of an assenbled program. A list of program elements makes an assembled program itself.
#[derive(Clone, Debug)]
pub enum ProgramElement {
	/// An assembly directive or macro that doesn't necessarily correspond to assembled data directly.
	Macro(Macro),
	/// A processor instruction that corresponds to some assembled data.
	Instruction(Instruction),
}

impl ProgramElement {
	/// Obtains a reference to the source span of this program element.
	#[must_use]
	pub const fn span(&self) -> &SourceSpan {
		match self {
			Self::Macro(Macro { span, .. }) | Self::Instruction(Instruction { span, .. }) => span,
		}
	}

	/// Extends the source span for this program element to reach until the new end span.
	#[must_use]
	pub fn extend_span(mut self, end: SourceSpan) -> Self {
		match &mut self {
			Self::Macro(Macro { span, .. }) | Self::Instruction(Instruction { span, .. }) =>
				*span = source_range((*span).into(), end.into()),
		}
		self
	}

	/// Set the label on this program element, returning itself.
	#[allow(clippy::missing_const_for_fn)] // false positive
	#[must_use]
	pub fn set_label(self, label: Option<Label>) -> Self {
		match self {
			Self::Macro(mut r#macro) => {
				r#macro.label = label;
				Self::Macro(r#macro)
			},
			Self::Instruction(mut instruction) => {
				instruction.label = label;
				Self::Instruction(instruction)
			},
		}
	}
}
