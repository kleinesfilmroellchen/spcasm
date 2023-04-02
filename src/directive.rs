//! Assembly directives and user-defined macros
#![allow(clippy::module_name_repetitions)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::num::NonZeroU64;
use std::sync::Arc;

use miette::SourceSpan;
use num_derive::ToPrimitive;
use num_traits::{FromPrimitive, ToPrimitive};
#[allow(unused)]
use smartstring::alias::String;
use spcasm_derive::Parse;

use crate::parser::instruction::MemoryAddress;
use crate::parser::program::byte_vec_to_string;
use crate::parser::reference::{GlobalLabel, MacroParent, Reference, ReferenceResolvable};
use crate::parser::value::{Size, SizedAssemblyTimeValue};
use crate::parser::{self, source_range, AssemblyTimeValue, ProgramElement};
use crate::{AssemblyCode, AssemblyError, Segments};

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
	/// If the segments are mishandles, for example an empty segment stack.
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
								Size::from_i64(value.try_value(self.span, source_code.clone())?).unwrap(),
						DirectiveParameter::PadSize =>
							segments.directive_parameters.pad_value.get_or_insert_default().size =
								Size::from_i64(value.try_value(self.span, source_code.clone())?).unwrap(),
					}
				}
				Ok(())
			},
			DirectiveValue::Fill { value, operation, .. } => {
				*value = if operation.is_fill() {
					&segments.directive_parameters.fill_value
				} else {
					&segments.directive_parameters.pad_value
				}
				.clone();
				Ok(())
			},
			DirectiveValue::AssignReference { reference: reference @ Reference::MacroArgument { .. }, .. } =>
				return Err(AssemblyError::AssigningToReference {
					kind:     crate::error::ReferenceType::MacroArgument,
					name:     reference.to_string().into(),
					src:      source_code,
					location: self.span,
				}
				.into()),
			DirectiveValue::AssignReference { reference: reference @ Reference::MacroGlobal { .. }, .. } =>
				return Err(AssemblyError::AssigningToReference {
					name:     reference.to_string().into(),
					kind:     crate::error::ReferenceType::MacroGlobal,
					src:      source_code,
					location: self.span,
				}
				.into()),
			_ => Ok(()),
		}
		.map_err(|_| AssemblyError::NoSegmentOnStack { location: self.span, src: source_code }.into())
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
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<crate::AssemblyError>> {
		self.value.replace_macro_parent(replacement_parent, source_code)
	}

	fn resolve_relative_labels(
		&mut self,
		direction: parser::reference::RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RefCell<GlobalLabel>>>,
	) {
		self.value.resolve_relative_labels(direction, relative_labels);
	}
}

impl std::fmt::Display for Directive {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{} {:?} {}",
			parser::program::span_to_string(self.span),
			self.value,
			byte_vec_to_string(&self.expected_value),
		)
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
	End,
	Pushpc,
	Pullpc,
	Arch,
	Macro,
	EndMacro,
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
			Self::End => "end",
			Self::Pushpc => "pushpc",
			Self::Pullpc => "pullpc",
			Self::Arch => "arch",
			Self::Macro => "macro",
			Self::EndMacro => "endmacro",
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
		})
	}
}

/// An assembly directive's value and relevant data.
#[derive(Clone, Debug)]
pub enum DirectiveValue {
	/// A placeholder value, this is mostly auto-generated.
	Placeholder,
	/// org <memory address>
	Org(MemoryAddress),
	/// Various table directives, such as byte/db, word/dw, dl, dd, ascii(z), ...
	/// dw <16-bit word>
	Table {
		/// The entries of the table. For simple directives like "dw $0A", this only has one entry.
		values: Vec<SizedAssemblyTimeValue>,
	},
	/// brr <file name>
	Brr {
		/// Path to the WAV source file.
		file:      String,
		/// The range of samples to include.
		range:     Option<SourceSpan>,
		/// Whether to automatically trim silence at the beginning and end of the sample (after cutting the range)
		auto_trim: bool,
		/// Whether to add the sample to the sample directory (not currently implemented)
		directory: bool,
	},
	/// sampletable
	SampleTable {
		/// Whether to automatically align the sample table or throw an error.
		auto_align: bool,
	},
	/// ascii(z) <string>
	String { text: Vec<u8>, has_null_terminator: bool },
	/// <reference> = <value>
	AssignReference { reference: Reference, value: AssemblyTimeValue },
	/// incbin <file name>
	Include { file: String, range: Option<SourceSpan> },
	/// end
	End,
	/// pushpc
	PushSection,
	/// pullpc
	PopSection,
	/// macro
	UserDefinedMacro { name: String, arguments: Arc<RefCell<MacroParent>>, body: Vec<ProgramElement> },
	/// A variety of global parameters are changed with this directive.
	SetDirectiveParameters(HashMap<DirectiveParameter, AssemblyTimeValue>),
	/// fill, pad
	Fill {
		/// The exact fill operation to be performed.
		operation: FillOperation,
		/// The operation parameter which decides how much to fill. This is interpreted in a variety of ways depending
		/// on the fill operation.
		parameter: AssemblyTimeValue,
		/// The value to fill with, populated from a global directive parameter.
		value:     Option<SizedAssemblyTimeValue>,
	},
}

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
			| Self::Org(..) => 0,
			Self::Table { values } =>
				values.len() * values.first().and_then(|value| value.size.to_u8()).unwrap_or(0) as usize,
			Self::String { text, has_null_terminator } => text.len() + (usize::from(*has_null_terminator)),
			Self::Fill { operation: FillOperation::Amount, parameter, .. } => parameter
				.value_using_resolver(&|_| None)
				.unwrap_or_else(|| (Self::large_assembled_size).try_into().unwrap())
				as usize,
			// Use a large assembled size as a signal that we don't know at this point. This will force any later
			// reference out of the direct page, which will always yield correct behavior.
			Self::Include { .. } | Self::Brr { .. } | Self::SampleTable { .. } | Self::Fill { .. } =>
				Self::large_assembled_size,
		}
	}

	/// Whether the directive needs to be in the segmented AST (false) or not (true).
	pub const fn is_symbolic(&self) -> bool {
		match self {
			Self::Org(_)
			| Self::Placeholder
			| Self::PushSection
			| Self::SetDirectiveParameters { .. }
			| Self::PopSection
			| Self::End
			| Self::AssignReference { .. }
			| Self::UserDefinedMacro { .. } => true,
			Self::Table { .. }
			| Self::String { .. }
			| Self::Brr { .. }
			| Self::Fill { .. }
			| Self::SampleTable { .. }
			| Self::Include { .. } => false,
		}
	}

	/// Set this global label as the parent for all the unresolved local labels.
	pub fn set_global_label(&mut self, label: &Option<Arc<RefCell<GlobalLabel>>>, source_code: &Arc<AssemblyCode>) {
		if let Some(label) = label {
			match self {
				Self::Table { values, .. } =>
					for value in values.iter_mut() {
						value.value.set_global_label(label);
					},
				Self::AssignReference { reference, value } => {
					if let Reference::Local(assigned_local) = reference {
						*assigned_local = parser::reference::merge_local_into_parent(
							assigned_local.clone(),
							Some(label.clone()),
							source_code,
						)
						.unwrap();
					}
					value.set_global_label(label);
				},
				Self::UserDefinedMacro { .. } // TODO: Multi-labeled instructions in user defined macros will probably not work correctly!
				| Self::Include { .. }
				| Self::SampleTable { .. }
				| Self::Brr { .. }
				| Self::String { .. }
				| Self::SetDirectiveParameters { .. }
				| Self::Fill { .. }
				| Self::Placeholder
				| Self::End
				| Self::PushSection
				| Self::PopSection
				| Self::Org(_) => {},
			}
		}
	}
}

impl ReferenceResolvable for DirectiveValue {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Table { values, .. } => {
				for value in values {
					value.value.replace_macro_parent(replacement_parent.clone(), source_code)?;
				}
				Ok(())
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
			| Self::Org(_) => Ok(()),
			Self::AssignReference { value, .. } => value.replace_macro_parent(replacement_parent, source_code),
			Self::UserDefinedMacro { name, body, .. } => Err(AssemblyError::RecursiveMacroDefinition {
				name:     (*name).to_string().into(),
				location: source_range(
					body.first().map_or_else(|| (0, 0).into(), ProgramElement::span).into(),
					body.last().map_or_else(|| (0, 0).into(), ProgramElement::span).into(),
				),
				outer:    replacement_parent.borrow().global_label().borrow().span,
				src:      source_code.clone(),
			}
			.into()),
		}
	}

	fn resolve_relative_labels(
		&mut self,
		direction: parser::reference::RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RefCell<GlobalLabel>>>,
	) {
		match self {
			Self::Table { values, .. } =>
				for value in values {
					value.value.resolve_relative_labels(direction, relative_labels);
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
			| Self::Org(_)
			| Self::UserDefinedMacro { .. } => (),
			Self::AssignReference { value, .. } => value.resolve_relative_labels(direction, relative_labels),
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
	fn hash<H: ~const std::hash::Hasher>(&self, state: &mut H) {
		state.write_u8(self.to_u8().expect("unreachable"));
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
	/// - `ToAddress` uses the parameter as the address to fill to. The amount to fill is the distance of that to the
	///   current address.
	/// - `ToAlignment` uses the parameter as the alignment to achieve. The amount to fill is the difference between the
	///   current memory address and the next correctly-aligned address. Also, the offset (if provided) is added to that
	///   fill amount.
	/// - `Amount` uses the parameter directly as the amount of bytes to fill.
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
						+ offset.clone().map_or(Ok(0), |offset| offset.try_value(location, source_code.clone()))?
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
