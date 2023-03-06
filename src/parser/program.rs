//! AST of the entire assembly program.
#![allow(clippy::module_name_repetitions, clippy::large_enum_variant)]
use std::cell::RefCell;
use std::sync::Arc;

use miette::SourceSpan;
#[allow(unused)]
use smartstring::alias::String;

use super::instruction::Instruction;
use super::reference::{merge_local_into_parent, MacroParent, MacroParentReplacable, Reference};
use super::{AssemblyTimeValue, Directive};
use crate::parser::source_range;
use crate::{AssemblyCode, AssemblyError};

/// A program element of an assenbled program. A list of program elements makes an assembled program itself.
#[derive(Clone, Debug)]
pub enum ProgramElement {
	/// An assembly directive that doesn't necessarily correspond to assembled data directly.
	Directive(Directive),
	/// A processor instruction that corresponds to some assembled data.
	Instruction(Instruction),
	/// Include directive that copy-pastes another file's assembly into this one.
	IncludeSource {
		/// The file that is included as source code.
		file:  String,
		/// Source code location of the include directive.
		span:  SourceSpan,
		/// Label before the include directive. This label will be used for the first instruction in the included file.
		label: Option<Reference>,
	},
	/// Calling a user-defined macro, e.g. `%my_macro(3, 4, 5)`
	UserDefinedMacroCall {
		/// Name of the macro that is being called.
		macro_name: String,
		/// The arguments to the macro; currently only numbers are supported.
		arguments:  Vec<AssemblyTimeValue>,
		/// Location in source code of the macro call.
		span:       SourceSpan,
		/// Label before the macro call. This label will be used for the first instruction in the macro.
		label:      Option<Reference>,
	},
}

impl ProgramElement {
	/// Obtains a reference to the source span of this program element.
	#[must_use]
	pub const fn span(&self) -> &SourceSpan {
		match self {
			Self::Directive(Directive { span, .. })
			| Self::Instruction(Instruction { span, .. })
			| Self::UserDefinedMacroCall { span, .. }
			| Self::IncludeSource { span, .. } => span,
		}
	}

	/// Extends the source span for this program element to reach until the new end span.
	#[must_use]
	pub fn extend_span(mut self, end: SourceSpan) -> Self {
		match &mut self {
			Self::Directive(Directive { span, .. })
			| Self::Instruction(Instruction { span, .. })
			| Self::UserDefinedMacroCall { span, .. }
			| Self::IncludeSource { span, .. } => *span = source_range((*span).into(), end.into()),
		}
		self
	}

	/// Set the labels on this program element, returning itself.
	#[allow(clippy::missing_panics_doc)]
	#[must_use]
	pub fn set_labels(self, mut labels: Vec<Reference>, source_code: &Arc<AssemblyCode>) -> Self {
		// Preserve the label hierarchy correctly.
		let mut current_global = None;
		let mut last_label = if labels.is_empty() { None } else { Some(labels.remove(labels.len() - 1)) };
		for mut label in labels {
			// The unwrap cannot fail since that would mean that we never entered the loop.
			label.set_location(AssemblyTimeValue::Reference(last_label.clone().unwrap()));
			match label {
				Reference::Global(ref global) => current_global = Some(global.clone()),
				Reference::Local(ref local) =>
					if current_global.is_some() {
						merge_local_into_parent(local.clone(), current_global.clone(), source_code).unwrap();
					},
				_ => (),
			}
		}
		if let (Some(global), Some(Reference::Local(ref mut local))) = (current_global, &mut last_label) {
			*local = merge_local_into_parent(local.clone(), Some(global), source_code).unwrap();
		}

		match self {
			Self::Directive(mut directive) => {
				directive.label = directive.label.or(last_label);
				Self::Directive(directive)
			},
			Self::Instruction(mut instruction) => {
				instruction.label = instruction.label.or(last_label);
				Self::Instruction(instruction)
			},
			Self::IncludeSource { file, span, label: original_label } =>
				Self::IncludeSource { file, span, label: original_label.or(last_label) },
			Self::UserDefinedMacroCall { span, arguments, macro_name, label: original_label } =>
				Self::UserDefinedMacroCall { span, arguments, macro_name, label: original_label.or(last_label) },
		}
	}

	/// Returns the assembled size of this program element. Note that some program elements return a size of 0 as they
	/// should be gone by the end of the assembly process, and others return a large size intentionally because their
	/// size is not known yet and they should prevent any low-address optimizations.
	#[must_use]
	pub fn assembled_size(&self) -> usize {
		match self {
			Self::Directive(directive) => directive.value.assembled_size(),
			Self::Instruction(instruction) => instruction.assembled_size() as usize,
			Self::IncludeSource { .. } | Self::UserDefinedMacroCall { .. } => 0,
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
			Self::Directive(directive) => directive.replace_macro_parent(replacement_parent, source_code),
			Self::Instruction(instruction) => instruction.replace_macro_parent(replacement_parent, source_code),
			Self::UserDefinedMacroCall { arguments, .. } => {
				for argument in arguments {
					argument.replace_macro_parent(replacement_parent.clone(), source_code)?;
				}
				Ok(())
			},
			Self::IncludeSource { .. } => Ok(()),
		}
	}
}
