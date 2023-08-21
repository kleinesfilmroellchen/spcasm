//! Assembly directives and user-defined macros
#![allow(clippy::module_name_repetitions)]

use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::num::NonZeroU64;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{shared_str, FlexStr, IntoSharedStr, SharedStr, ToSharedStr};
use miette::SourceSpan;
use num_derive::ToPrimitive;
use num_traits::{FromPrimitive, ToPrimitive};
use parking_lot::RwLock;
use spcasm_derive::Parse;

use crate::parser::source_range;
use crate::sema::instruction::MemoryAddress;
use crate::sema::reference::{Label, MacroParent, Reference, ReferenceResolvable};
use crate::sema::value::{Size, SizedAssemblyTimeValue};
use crate::sema::{self, AssemblyTimeValue, ProgramElement};
use crate::{byte_vec_to_string, span_to_string, AssemblyCode, AssemblyError, Segments};

/// An assembly directive, often confusingly referred to as a "macro". spcasm uses the term "macro" to specifically mean
/// user-defined macros, and "directive" to mean builtin commands (i.e. directives) to the assembler.
#[derive(Clone, Debug)]
pub struct Directive {
	/// Actual data of the directive.
	pub value:          DirectiveValue,
	pub(crate) span:    SourceSpan,
	/// Label at the start of the directive. Some directives ignore this.
	pub expected_value: Option<Vec<u8>>,
}

impl Directive {
	/// Perform the segments operations of this directive, if this directive does any segment operations.
	/// # Errors
	/// If the segments are mishandled, for example an empty segment stack.
	#[allow(clippy::missing_panics_doc)]
	pub fn perform_segment_operations_if_necessary<Contained>(
		&mut self,
		segments: &mut Segments<Contained>,
		source_code: Arc<AssemblyCode>,
		current_label: Option<&Reference>,
	) -> Result<(), Box<AssemblyError>> {
		match &mut self.value {
			DirectiveValue::PopSection => segments.pop_segment(),
			DirectiveValue::PushSection => segments.push_segment(),
			DirectiveValue::Org(address) => {
				segments.new_segment(*address);
				Ok(())
			},
			DirectiveValue::Brr { directory: true, .. } => {
				segments.sample_table.add_sample(AssemblyTimeValue::Reference(
					current_label
						.cloned()
						.expect("all BRR samples in directories should have a label *automatically added*"),
					self.span,
				));
				Ok(())
			},
			DirectiveValue::SetDirectiveParameters(parameters) => {
				for (parameter, value) in parameters {
					match parameter {
						DirectiveParameter::FillValue =>
							segments.directive_parameters.fill_value.get_or_insert_default().value = value.clone(),
						DirectiveParameter::PadValue =>
							segments.directive_parameters.pad_value.get_or_insert_default().value = value.clone(),
						DirectiveParameter::FillSize =>
							segments.directive_parameters.fill_value.get_or_insert_default().size =
								Size::from_i64(value.try_value(self.span, &source_code)?).unwrap(),
						DirectiveParameter::PadSize =>
							segments.directive_parameters.pad_value.get_or_insert_default().size =
								Size::from_i64(value.try_value(self.span, &source_code)?).unwrap(),
					}
				}
				Ok(())
			},
			DirectiveValue::Fill { value, operation, .. } =>
				try {
					*value = if operation.is_fill() {
						&segments.directive_parameters.fill_value
					} else {
						&segments.directive_parameters.pad_value
					}
					.clone();
				},
			// For zero-sized strings, we would lose the preceding labels if we didn't remove the string here.
			DirectiveValue::String { text, has_null_terminator: false } if text.is_empty() =>
				try {
					self.value = DirectiveValue::Placeholder;
				},
			DirectiveValue::AssignReference {
				reference: ref reference @ (Reference::MacroArgument { .. } | Reference::MacroGlobal { .. }),
				..
			} =>
				return Err(AssemblyError::AssigningToReference {
					kind:     reference.into(),
					name:     reference.to_string().into(),
					src:      source_code,
					location: self.span,
				}
				.into()),
			_ => Ok(()),
		}
		.map_err(|()| AssemblyError::NoSegmentOnStack { location: self.span, src: source_code }.into())
	}
}

impl Default for Directive {
	fn default() -> Self {
		// We use the table directive with no entries as default as that will do nothing.
		Self { value: DirectiveValue::Placeholder, span: (0, 0).into(), expected_value: None }
	}
}

impl ReferenceResolvable for Directive {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RwLock<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<crate::AssemblyError>> {
		self.value.replace_macro_parent(replacement_parent, source_code)
	}

	fn resolve_relative_labels(
		&mut self,
		direction: sema::reference::RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RwLock<Label>>>,
	) {
		self.value.resolve_relative_labels(direction, relative_labels);
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		self.value.resolve_pseudo_labels(global_labels);
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.value.set_current_label(current_label, source_code)
	}
}

impl std::fmt::Display for Directive {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {} {}", span_to_string(self.span), self.value, byte_vec_to_string(&self.expected_value),)
	}
}

/// Directive symbols, used in lexing.
#[derive(Debug, Clone, Copy, Parse, Eq, PartialEq)]
pub enum DirectiveSymbol {
	Org,
	Db,
	Byte,
	Dw,
	Word,
	Dl,
	Dd,
	Ascii,
	Asciiz,
	Brr,
	SampleTable,
	Incbin,
	Include,
	Incsrc,
	EndAsm,
	Pushpc,
	Pullpc,
	Arch,
	Macro,
	EndMacro,
	If,
	Else,
	ElseIf,
	EndIf,
	Math,
	Fill,
	FillByte,
	FillWord,
	FillLong,
	FillDWord,
	Pad,
	PadByte,
	PadWord,
	PadLong,
	PadDWord,
	Namespace,
}

impl Display for DirectiveSymbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::Org => "org",
			Self::Db | Self::Byte => "db",
			Self::Dw | Self::Word => "dw",
			Self::Dl => "dl",
			Self::Dd => "dd",
			Self::Ascii => "ascii",
			Self::Asciiz => "asciiz",
			Self::Brr => "brr",
			Self::SampleTable => "sampletable",
			Self::Incbin => "incbin",
			Self::Include | Self::Incsrc => "include",
			Self::EndAsm => "endasm",
			Self::Pushpc => "pushpc",
			Self::Pullpc => "pullpc",
			Self::Arch => "arch",
			Self::Macro => "macro",
			Self::EndMacro => "endmacro",
			Self::If => "if",
			Self::Else => "else",
			Self::ElseIf => "elseif",
			Self::EndIf => "endif",
			Self::Math => "math",
			Self::Fill => "fill",
			Self::FillByte => "fillbyte",
			Self::FillWord => "fillword",
			Self::FillLong => "filllong",
			Self::FillDWord => "filldword",
			Self::Pad => "pad",
			Self::PadByte => "padbyte",
			Self::PadWord => "padword",
			Self::PadLong => "padlong",
			Self::PadDWord => "paddword",
			Self::Namespace => "namespace",
		})
	}
}

/// An assembly directive's value and relevant data.
#[derive(Clone, Debug)]
pub enum DirectiveValue {
	/// A placeholder value, this is mostly auto-generated.
	Placeholder,
	/// `org <memory address>`
	Org(MemoryAddress),
	/// Various table directives, such as byte/db, word/dw, dl, dd, ...
	Table {
		/// The entries of the table. For simple directives like "dw $0A", this only has one entry.
		values: Vec<SizedAssemblyTimeValue>,
	},
	/// `brr <file name>`
	Brr {
		/// Path to the WAV source file.
		file:      SharedStr,
		/// The range of samples to include.
		range:     Option<SourceSpan>,
		/// Whether to automatically trim silence at the beginning and end of the sample (after cutting the range)
		auto_trim: bool,
		/// Whether to add the sample to the sample directory (not currently implemented)
		directory: bool,
	},
	/// `sampletable`
	SampleTable {
		/// Whether to automatically align the sample table or throw an error.
		auto_align: bool,
	},
	/// `ascii(z) <string>`
	String { text: Vec<u8>, has_null_terminator: bool },
	/// `<reference> = <value>`
	AssignReference { reference: Reference, value: AssemblyTimeValue },
	/// `incbin <file name>`
	Include { file: SharedStr, range: Option<SourceSpan> },
	/// `endasm`
	End,
	/// `pushpc`
	PushSection,
	/// `pullpc`
	PopSection,
	/// `macro`
	UserDefinedMacro { name: SharedStr, arguments: Arc<RwLock<MacroParent>>, body: Vec<ProgramElement> },
	/// A variety of global parameters are changed with this directive.
	SetDirectiveParameters(HashMap<DirectiveParameter, AssemblyTimeValue>),
	/// `fill`, `pad`
	Fill {
		/// The exact fill operation to be performed.
		operation: FillOperation,
		/// The operation parameter which decides how much to fill. This is interpreted in a variety of ways depending
		/// on the fill operation.
		parameter: AssemblyTimeValue,
		/// The value to fill with, populated from a global directive parameter.
		value:     Option<SizedAssemblyTimeValue>,
	},
	/// `if`
	Conditional {
		/// The condition that decides which of the two blocks is assembled.
		condition:   AssemblyTimeValue,
		/// The block that is assembled if the condition is truthy.
		true_block:  Vec<ProgramElement>,
		/// The block that is assembled if the condition is falsy.
		false_block: Vec<ProgramElement>,
	},
	/// `namespace`
	StartNamespace { name: SharedStr },
	/// `namespace off`
	EndNamespace,
}

/// Expands to a pattern that matches all symbolic directives.
macro_rules! symbolic_directives {
	() => {
		$crate::directive::DirectiveValue::Org(_)
			| $crate::directive::DirectiveValue::Placeholder
			| $crate::directive::DirectiveValue::PushSection
			| $crate::directive::DirectiveValue::SetDirectiveParameters { .. }
			| $crate::directive::DirectiveValue::PopSection
			| $crate::directive::DirectiveValue::End
			| $crate::directive::DirectiveValue::StartNamespace { .. }
			| $crate::directive::DirectiveValue::EndNamespace
			| $crate::directive::DirectiveValue::AssignReference { .. }
			| $crate::directive::DirectiveValue::UserDefinedMacro { .. }
	};
}

pub(crate) use symbolic_directives;

impl DirectiveValue {
	/// A large assembled size constant which effectively disables all optimizations.
	const large_assembled_size: usize = u16::MAX as usize;

	/// Returns the final size of this directive once assembled. Note that:
	/// - some directives are purely symbolic and will not (directly) impact the size of the assembly, at least not
	///   here, and
	/// - the size of some directives is not known at this point, so a large placeholder is used instead. Because the
	///   purpose of this function is to estimate memory locations for coercing references into direct page mode, the
	///   large value will force later references into normal addressing, which is always correct.
	#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
	pub fn assembled_size(&self) -> usize {
		match self {
			// Symbolic operations take no space.
			Self::End
			| Self::PushSection
			| Self::PopSection
			| Self::UserDefinedMacro { .. }
			| Self::AssignReference { .. }
			| Self::Placeholder
			| Self::SetDirectiveParameters { .. }
			| Self::StartNamespace { .. }
			| Self::EndNamespace
			| Self::Org(..) => 0,
			Self::Table { values } =>
				values.len() * values.first().and_then(|value| value.size.to_u8()).unwrap_or(0) as usize,
			Self::String { text, has_null_terminator } => text.len() + (usize::from(*has_null_terminator)),
			Self::Fill { operation: FillOperation::Amount, parameter, .. } => parameter
				.value_using_resolver(&|_| None)
				.unwrap_or_else(|| (Self::large_assembled_size).try_into().unwrap())
				as usize,
			Self::Conditional { condition, true_block, false_block } =>
				if condition.is_truthy() {
					true_block.iter().map(ProgramElement::assembled_size).sum()
				} else if condition.is_falsy() {
					false_block.iter().map(ProgramElement::assembled_size).sum()
				} else {
					true_block
						.iter()
						.map(ProgramElement::assembled_size)
						.sum::<usize>()
						.max(false_block.iter().map(ProgramElement::assembled_size).sum())
				},
			// Use a large assembled size as a signal that we don't know at this point. This will force any later
			// reference out of the direct page, which will always yield correct behavior.
			Self::Include { .. } | Self::Brr { .. } | Self::SampleTable { .. } | Self::Fill { .. } =>
				Self::large_assembled_size,
		}
	}

	/// Whether the directive needs to be in the segmented AST (false) or not (true).
	pub const fn is_symbolic(&self) -> bool {
		matches!(self, symbolic_directives!())
	}
}

impl Display for DirectiveValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.pad(&match self {
			Self::Placeholder => "[placeholder]".to_owned(),
			Self::Org(value) => format!("org {:04X}", value),
			Self::Table { values } => format!(
				"table {}",
				values.iter().map(ToString::to_string).intersperse(", ".to_string()).collect::<String>()
			),
			Self::Brr { file, range, auto_trim, directory } => format!(
				"brr \"{}\" {}{}{}",
				file,
				range.map(span_to_string).unwrap_or_default(),
				if *auto_trim { " autotrim" } else { " no autotrim" },
				if *directory { " directory" } else { " nodirectory" }
			),
			Self::SampleTable { auto_align } =>
				format!("sampletable{}", if *auto_align { " autoalign" } else { " no autoalign" }),
			Self::String { text, has_null_terminator } => format!(
				"ascii \"{:?}{}\"",
				text.iter().map(|byte| *byte as char).collect::<String>(),
				if *has_null_terminator { "\\0" } else { "" }
			),
			Self::AssignReference { reference, value } => format!("[reference] {} = {:04X}", reference, value),
			Self::Include { file, range } =>
				format!("include \"{}\"{}", file, range.map(span_to_string).unwrap_or_default()),
			Self::End => "endasm".to_string(),
			Self::PushSection => "push".to_string(),
			Self::PopSection => "pop".to_string(),
			Self::EndNamespace => "namespace off".to_string(),
			Self::StartNamespace { name } => format!("namespace {}", name),
			Self::UserDefinedMacro { name, arguments, body } => format!(
				"macro {} ({})\n    {}",
				name,
				arguments.read(),
				body.iter()
					.map(ProgramElement::to_string)
					.intersperse("\n".into())
					.collect::<String>()
					.replace('\n', "\n    ")
			),
			Self::SetDirectiveParameters(parameters) => format!(
				"set parameters: {}",
				parameters
					.iter()
					.map(|(parameter, value)| format!("{parameter} = {value}"))
					.intersperse(", ".into())
					.collect::<String>()
			),
			Self::Fill { operation, parameter, value } => format!(
				"fill {} {} {}",
				match operation {
					FillOperation::Amount => "amount".to_owned(),
					FillOperation::ToAddress => "to address".to_owned(),
					FillOperation::ToAlignment { offset } => format!(
						"to alignment{}",
						offset.as_ref().map(|offset| format!(" (offset {:04X})", offset)).unwrap_or_default()
					),
				},
				parameter,
				value.as_ref().map_or_else(|| "[fill value unknown]".to_owned(), SizedAssemblyTimeValue::to_string)
			),
			Self::Conditional { condition, true_block, false_block } => format!(
				"if {:04X}\n{}\nelse\n{}",
				condition,
				true_block
					.iter()
					.map(ProgramElement::to_string)
					.intersperse("\n".into())
					.collect::<String>()
					.replace('\n', "\n    "),
				false_block
					.iter()
					.map(ProgramElement::to_string)
					.intersperse("\n".into())
					.collect::<String>()
					.replace('\n', "\n    "),
			),
		})
	}
}

impl ReferenceResolvable for DirectiveValue {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RwLock<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Table { values, .. } =>
				try {
					for value in values {
						value.value.replace_macro_parent(replacement_parent.clone(), source_code)?;
					}
				},
			Self::Conditional { true_block, false_block, .. } =>
				try {
					for element in true_block.iter_mut().chain(false_block.iter_mut()) {
						element.replace_macro_parent(replacement_parent.clone(), source_code)?;
					}
				},
			Self::String { .. }
			| Self::Include { .. }
			| Self::End
			| Self::PushSection
			| Self::Placeholder
			| Self::Brr { .. }
			| Self::SampleTable { .. }
			| Self::SetDirectiveParameters { .. }
			| Self::Fill { .. }
			| Self::PopSection
			| Self::StartNamespace { .. }
			| Self::EndNamespace
			| Self::Org(_) => Ok(()),
			Self::AssignReference { value, .. } => value.replace_macro_parent(replacement_parent, source_code),
			Self::UserDefinedMacro { name, body, .. } => Err(AssemblyError::RecursiveMacroDefinition {
				name:     (*name).to_string().into(),
				location: source_range(
					body.first().map_or_else(|| (0, 0).into(), ProgramElement::span).into(),
					body.last().map_or_else(|| (0, 0).into(), ProgramElement::span).into(),
				),
				outer:    replacement_parent.read().global_label().read().source_span(),
				src:      source_code.clone(),
			}
			.into()),
		}
	}

	fn resolve_relative_labels(
		&mut self,
		direction: sema::reference::RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RwLock<Label>>>,
	) {
		match self {
			Self::Table { values, .. } =>
				for value in values {
					value.value.resolve_relative_labels(direction, relative_labels);
				},
			Self::Conditional { true_block, false_block, .. } =>
				for element in true_block.iter_mut().chain(false_block.iter_mut()) {
					element.resolve_relative_labels(direction, relative_labels);
				},
			Self::Fill { parameter, value, .. } => {
				parameter.resolve_relative_labels(direction, relative_labels);
				if let Some(value) = value.as_mut() {
					value.resolve_relative_labels(direction, relative_labels);
				}
			},
			Self::String { .. }
			| Self::Include { .. }
			| Self::End
			| Self::PushSection
			| Self::Placeholder
			| Self::Brr { .. }
			| Self::SampleTable { .. }
			| Self::SetDirectiveParameters { .. }
			| Self::PopSection
			| Self::StartNamespace { .. }
			| Self::EndNamespace
			| Self::Org(_)
			| Self::UserDefinedMacro { .. } => (),
			Self::AssignReference { value, .. } => value.resolve_relative_labels(direction, relative_labels),
		}
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		match self {
			Self::Table { values, .. } =>
				for value in values {
					value.value.resolve_pseudo_labels(global_labels);
				},
			Self::Conditional { true_block, false_block, .. } =>
				for element in true_block.iter_mut().chain(false_block.iter_mut()) {
					element.resolve_pseudo_labels(global_labels);
				},
			Self::Fill { parameter, value, .. } => {
				parameter.resolve_pseudo_labels(global_labels);
				if let Some(value) = value.as_mut() {
					value.resolve_pseudo_labels(global_labels);
				}
			},
			Self::String { .. }
			| Self::Include { .. }
			| Self::End
			| Self::PushSection
			| Self::Placeholder
			| Self::Brr { .. }
			| Self::SampleTable { .. }
			| Self::SetDirectiveParameters { .. }
			| Self::PopSection
			| Self::StartNamespace { .. }
			| Self::EndNamespace
			| Self::Org(_)
			| Self::UserDefinedMacro { .. } => (),
			Self::AssignReference { value, .. } => value.resolve_pseudo_labels(global_labels),
		}
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Table { values, .. } =>
				try {
					for value in &mut *values {
						value.value.set_current_label(current_label, source_code)?;
					}
				},
			Self::Conditional { true_block, false_block, .. } =>
				try {
					for element in true_block.iter_mut().chain(false_block.iter_mut()) {
						element.set_current_label(current_label, source_code)?;
					}
				},
			Self::Fill { parameter, value, .. } =>
				try {
					parameter.set_current_label(current_label, source_code)?;
					if let Some(value) = value.as_mut() {
						value.set_current_label(current_label, source_code)?;
					}
				},
			Self::AssignReference { reference, value } => {
				reference.set_current_label_with_kind(
					current_label,
					sema::LabelUsageKind::AsDefinition,
					source_code,
				)?;
				value.set_current_label(current_label, source_code)
			},
			Self::UserDefinedMacro { .. }
			| Self::Include { .. }
			| Self::SampleTable { .. }
			| Self::Brr { .. }
			| Self::String { .. }
			| Self::SetDirectiveParameters { .. }
			| Self::Placeholder
			| Self::End
			| Self::PushSection
			| Self::PopSection
			| Self::StartNamespace { .. }
			| Self::EndNamespace
			| Self::Org(_) => Ok(()),
		}
	}
}

/// Global directive parameters. Many Asar-derived directives use global parameters which are set via a separate
/// directive. This enum lists all of the supported parameter kinds.
#[derive(Copy, Clone, Debug, Eq, PartialEq, ToPrimitive)]
#[repr(u8)]
pub enum DirectiveParameter {
	FillValue,
	PadValue,
	FillSize,
	PadSize,
}

// Special perfect hash for DirectiveParameters.
impl Hash for DirectiveParameter {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		state.write_u8(self.to_u8().expect("unreachable"));
	}
}

impl Display for DirectiveParameter {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.pad(match self {
			Self::FillValue => "fillvalue",
			Self::PadValue => "padvalue",
			Self::FillSize => "fillsize",
			Self::PadSize => "padsize",
		})
	}
}

/// Exact fill operation that should be performed by a fill-like directive.
#[derive(Clone, Debug, PartialEq)]
pub enum FillOperation {
	/// fill align
	ToAlignment { offset: Option<AssemblyTimeValue> },
	/// pad
	ToAddress,
	/// fill
	Amount,
}

impl FillOperation {
	/// Returns whether this operation represents a `fill` directive in source code (true), or a `pad` directive
	/// (false).
	#[must_use]
	pub const fn is_fill(&self) -> bool {
		match self {
			Self::ToAlignment { .. } | Self::Amount => true,
			Self::ToAddress => false,
		}
	}

	/// Returns the amount of bytes to fill, given the fill operation parameter and a current address from which to
	/// start filling. The result depends on which fill operation is performed:
	/// - [`FillOperation::ToAddress`] uses the parameter as the address to fill to. The amount to fill is the distance
	///   of that to the current address.
	/// - [`FillOperation::ToAlignment`] uses the parameter as the alignment to achieve. The amount to fill is the
	///   difference between the current memory address and the next correctly-aligned address. Also, the offset (if
	///   provided) is added to that fill amount.
	/// - [`FillOperation::Amount`] uses the parameter directly as the amount of bytes to fill.
	pub fn amount_to_fill(
		&self,
		parameter: MemoryAddress,
		current_address: MemoryAddress,
		location: SourceSpan,
		source_code: &Arc<AssemblyCode>,
	) -> Result<MemoryAddress, Box<AssemblyError>> {
		Ok(match self {
			Self::ToAlignment { offset } => {
				let alignment = parameter;
				// Technically speaking, every address is aligned to 0 :^)
				if alignment == 0 {
					0
				} else {
					// If this address is already aligned, no fill is required.
					let offset_to_aligned_address =
						if current_address % alignment == 0 { 0 } else { alignment - current_address % alignment };
					offset_to_aligned_address
						+ offset.clone().map_or(Ok(0), |offset| offset.try_value(location, source_code))?
				}
			},
			Self::ToAddress => parameter - current_address,
			Self::Amount => parameter,
		})
	}
}

impl Display for FillOperation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::ToAlignment { .. } => "fill align",
			Self::ToAddress => "pad",
			Self::Amount => "fill",
		})
	}
}

/// State of all directive parameters, used for state management in the segment structures.
#[derive(Clone, Debug, Default)]
pub struct DirectiveParameterTable {
	fill_value: Option<SizedAssemblyTimeValue>,
	pad_value:  Option<SizedAssemblyTimeValue>,
}
