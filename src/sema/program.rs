//! AST of the entire assembly program.
#![allow(clippy::module_name_repetitions, clippy::large_enum_variant)]
use std::sync::Arc;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use miette::SourceSpan;
use parking_lot::RwLock;

use super::AssemblyTimeValue;
use super::instruction::Instruction;
use super::reference::{MacroParent, Reference, ReferenceResolvable};
use crate::parser::source_range;
use crate::{AssemblyCode, AssemblyError, Directive, span_to_string};

/// A program element of an assembled program. A list of program elements makes an assembled program itself.
#[derive(Clone, Debug)]
pub enum ProgramElement {
	/// A label for the current location in memory.
	/// Later on, these are used to assign specific values to these references.
	Label(Reference),
	/// An assembly directive that doesn't necessarily correspond to assembled data directly.
	Directive(Directive),
	/// A processor instruction that corresponds to some assembled data.
	Instruction(Instruction),
	/// Include directive that copy-pastes another file's assembly into this one.
	IncludeSource {
		/// The file that is included as source code.
		file: SharedStr,
		/// Source code location of the include directive.
		span: SourceSpan,
	},
	/// Calling a user-defined macro, e.g. `%my_macro(3, 4, 5)`
	UserDefinedMacroCall {
		/// Name of the macro that is being called.
		macro_name: SharedStr,
		/// The arguments to the macro; currently only numbers are supported.
		arguments:  Vec<AssemblyTimeValue>,
		/// Location in source code of the macro call.
		span:       SourceSpan,
	},
}

impl ProgramElement {
	/// Obtains a reference to the source span of this program element.
	#[must_use]
	pub fn span(&self) -> SourceSpan {
		match self {
			Self::Directive(Directive { span, .. })
			| Self::Instruction(Instruction { span, .. })
			| Self::UserDefinedMacroCall { span, .. }
			| Self::IncludeSource { span, .. } => *span,
			Self::Label(reference) => reference.source_span(),
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
			// Label source spans never need to be adjusted afterwards.
			Self::Label(_) => (),
		}
		self
	}

	/// Returns the assembled size of this program element. Note that some program elements return a size of 0 as they
	/// should be gone by the end of the assembly process, and others return a large size intentionally because their
	/// size is not known yet and they should prevent any low-address optimizations.
	#[must_use]
	pub fn assembled_size(&self) -> usize {
		match self {
			Self::Directive(directive) => directive.value.assembled_size(),
			Self::Instruction(instruction) => instruction.assembled_size() as usize,
			Self::IncludeSource { .. } | Self::UserDefinedMacroCall { .. } | Self::Label(_) => 0,
		}
	}
}

impl ReferenceResolvable for ProgramElement {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RwLock<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Directive(directive) => directive.replace_macro_parent(replacement_parent, source_code),
			Self::Label(reference) => reference.replace_macro_parent(replacement_parent, source_code),
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

	fn resolve_relative_labels(
		&mut self,
		direction: super::reference::RelativeReferenceDirection,
		relative_labels: &std::collections::HashMap<std::num::NonZeroU64, Arc<RwLock<super::reference::Label>>>,
	) {
		match self {
			Self::Directive(directive) => directive.resolve_relative_labels(direction, relative_labels),
			Self::Label(reference) => reference.resolve_relative_labels(direction, relative_labels),
			Self::Instruction(instruction) => instruction.resolve_relative_labels(direction, relative_labels),
			Self::UserDefinedMacroCall { arguments, .. } =>
				for argument in arguments {
					argument.resolve_relative_labels(direction, relative_labels);
				},
			Self::IncludeSource { .. } => (),
		}
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<super::reference::Label>>]) {
		match self {
			Self::Directive(directive) => directive.resolve_pseudo_labels(global_labels),
			Self::Label(reference) => reference.resolve_pseudo_labels(global_labels),
			Self::Instruction(instruction) => instruction.resolve_pseudo_labels(global_labels),
			Self::UserDefinedMacroCall { arguments, .. } =>
				for argument in arguments {
					argument.resolve_pseudo_labels(global_labels);
				},
			Self::IncludeSource { .. } => (),
		}
	}

	fn set_current_label(
		&mut self,
		current_label: Option<&Arc<RwLock<super::reference::Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Label(label) =>
				label.set_current_label_with_kind(current_label, super::LabelUsageKind::AsDefinition, source_code),
			Self::Directive(directive) => directive.set_current_label(current_label, source_code),
			Self::Instruction(instruction) => instruction.set_current_label(current_label, source_code),
			Self::IncludeSource { .. } => Ok(()),
			Self::UserDefinedMacroCall { arguments, .. } =>
				try {
					for argument in arguments {
						argument.set_current_label(current_label, source_code)?;
					}
				},
		}
	}

	fn resolve_repeatcount(&mut self, repetition: super::instruction::MemoryAddress) {
		match self {
			Self::Label(label) => label.resolve_repeatcount(repetition),
			Self::Directive(directive) => directive.resolve_repeatcount(repetition),
			Self::Instruction(instruction) => instruction.resolve_repeatcount(repetition),
			Self::IncludeSource { .. } => {},
			Self::UserDefinedMacroCall { arguments, .. } =>
				for argument in arguments {
					argument.resolve_repeatcount(repetition);
				},
		}
	}
}

impl std::fmt::Display for ProgramElement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Label(label) => write!(
				f,
				"{} [label] {}{}",
				span_to_string(label.source_span()),
				label,
				label.location().map_or_else(String::new, |value| format!(" = {value}"),)
			),
			Self::Directive(directive) => write!(f, "{directive}"),
			Self::Instruction(instruction) => write!(f, "{instruction}"),
			Self::IncludeSource { file, span } => write!(f, "{} include \"{}\"", span_to_string(*span), file),
			Self::UserDefinedMacroCall { macro_name, arguments, span } => write!(
				f,
				"{} [macro call] %{} ( {} )",
				span_to_string(*span),
				macro_name,
				arguments.iter().map(AssemblyTimeValue::to_string).intersperse(", ".into()).collect::<String>(),
			),
		}
	}
}
