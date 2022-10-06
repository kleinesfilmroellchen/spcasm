//! AST of the entire assembly program.
#![allow(clippy::module_name_repetitions, clippy::large_enum_variant)]
use std::cell::RefCell;
use std::sync::Arc;

use miette::SourceSpan;

use super::instruction::{Instruction, Number};
use super::label::{Label, MacroParent, MacroParentReplacable};
use super::Macro;
use crate::parser::source_range;
use crate::{AssemblyCode, AssemblyError};

/// A program element of an assenbled program. A list of program elements makes an assembled program itself.
#[derive(Clone, Debug)]
pub enum ProgramElement {
	/// An assembly directive or macro that doesn't necessarily correspond to assembled data directly.
	Macro(Macro),
	/// A processor instruction that corresponds to some assembled data.
	Instruction(Instruction),
	/// Include directive that copy-pastes another file's assembly into this one.
	IncludeSource {
		/// The file that is included as source code.
		file:  String,
		/// Source code location of the include directive.
		span:  SourceSpan,
		/// Label before the include directive. This label will be used for the first instruction in the included file.
		label: Option<Label>,
	},
	/// Calling a user-defined macro, e.g. `%my_macro(3, 4, 5)`
	UserDefinedMacroCall {
		/// Name of the macro that is being called.
		macro_name: String,
		/// The arguments to the macro; currently only numbers are supported.
		arguments:  Vec<Number>,
		/// Location in source code of the macro call.
		span:       SourceSpan,
		/// Label before the macro call. This label will be used for the first instruction in the macro.
		label:      Option<Label>,
	},
}

impl ProgramElement {
	/// Obtains a reference to the source span of this program element.
	#[must_use]
	pub const fn span(&self) -> &SourceSpan {
		match self {
			Self::Macro(Macro { span, .. })
			| Self::Instruction(Instruction { span, .. })
			| Self::UserDefinedMacroCall { span, .. }
			| Self::IncludeSource { span, .. } => span,
		}
	}

	/// Extends the source span for this program element to reach until the new end span.
	#[must_use]
	pub fn extend_span(mut self, end: SourceSpan) -> Self {
		match &mut self {
			Self::Macro(Macro { span, .. })
			| Self::Instruction(Instruction { span, .. })
			| Self::UserDefinedMacroCall { span, .. }
			| Self::IncludeSource { span, .. } => *span = source_range((*span).into(), end.into()),
		}
		self
	}

	/// Set the label on this program element, returning itself.
	#[allow(clippy::missing_const_for_fn)] // false positive
	#[must_use]
	pub fn set_label(self, label: Option<Label>) -> Self {
		match self {
			Self::Macro(mut r#macro) => {
				r#macro.label = r#macro.label.or(label);
				Self::Macro(r#macro)
			},
			Self::Instruction(mut instruction) => {
				instruction.label = instruction.label.or(label);
				Self::Instruction(instruction)
			},
			Self::IncludeSource { file, span, label: original_label } =>
				Self::IncludeSource { file, span, label: original_label.or(label) },
			Self::UserDefinedMacroCall { span, arguments, macro_name, label: original_label } =>
				Self::UserDefinedMacroCall { span, arguments, macro_name, label: original_label.or(label) },
		}
	}
}

impl MacroParentReplacable for ProgramElement {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Macro(r#macro) => r#macro.replace_macro_parent(replacement_parent, source_code),
			Self::Instruction(instruction) => instruction.replace_macro_parent(replacement_parent, source_code),
			Self::UserDefinedMacroCall { arguments, .. } => {
				for argument in arguments {
					argument.replace_macro_parent(replacement_parent.clone(), source_code)?;
				}
				Ok(())
			},
			Self::IncludeSource { file, span, label } => Ok(()),
		}
	}
}
