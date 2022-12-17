//! Assembly directives and user-defined macros
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]

use std::cell::RefCell;
use std::fmt::Display;
use std::sync::Arc;

use miette::SourceSpan;
use spcasm_derive::Parse;

use crate::parser::instruction::MemoryAddress;
use crate::parser::reference::{MacroParent, MacroParentReplacable, Reference};
use crate::parser::{source_range, AssemblyTimeValue, ProgramElement};
use crate::{AssemblyCode, AssemblyError, Segments};

/// An assembly directive, often confusingly referred to as a "macro". spcasm uses the term "macro" to specifically mean
/// user-defined macros, and "directive" to mean builtin commands (i.e. directives) to the assembler.
#[derive(Clone, Debug)]
pub struct Directive {
	/// Actual data of the directive.
	pub value:       DirectiveValue,
	pub(crate) span: SourceSpan,
	/// Label at the start of the directive. Some directives ignore this.
	pub label:       Option<Reference>,
}

impl Directive {
	/// Perform the segments operations of this directive, if this directive does any segment operations.
	pub fn perform_segment_operations_if_necessary<Contained>(
		&self,
		segments: &mut Segments<Contained>,
		source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self.value {
			DirectiveValue::PopSection => segments.pop_segment(),
			DirectiveValue::PushSection => segments.push_segment(),
			DirectiveValue::Org(address) => {
				segments.new_segment(address);
				Ok(())
			},
			_ => Ok(()),
		}
		.map_err(|_| AssemblyError::NoSegmentOnStack { location: self.span, src: source_code }.into())
	}
}

impl Default for Directive {
	fn default() -> Self {
		// We use the table directive with no entries as default as that will do nothing.
		Self { value: DirectiveValue::Table { values: Vec::new(), entry_size: 1 }, label: None, span: (0, 0).into() }
	}
}

impl MacroParentReplacable for Directive {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<crate::AssemblyError>> {
		self.value.replace_macro_parent(replacement_parent, source_code)
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
	Incbin,
	Include,
	End,
	Pushpc,
	Pullpc,
	Arch,
	Macro,
	EndMacro,
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
			Self::Incbin => "incbin",
			Self::Include => "include",
			Self::End => "end",
			Self::Pushpc => "pushpc",
			Self::Pullpc => "pullpc",
			Self::Arch => "arch",
			Self::Macro => "macro",
			Self::EndMacro => "endmacro",
		})
	}
}

/// An assembly directive's value and relevant data.
#[derive(Clone, Debug)]
pub enum DirectiveValue {
	/// org <memory address>
	Org(MemoryAddress),
	/// Various table directives, such as byte/db, word/dw, dl, dd, ascii(z), ...
	/// dw <16-bit word>
	Table {
		/// The entries of the table. For simple directives like "dw $0A", this only has one entry.
		values:     Vec<AssemblyTimeValue>,
		/// How many bytes each entry occupies; depends on the specific directive used.
		entry_size: u8,
	},
	/// brr <file name>
	Brr(String),
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
}

impl DirectiveValue {
	/// Returns the final size of this directive once assembled. Note that:
	/// - some directives are purely symbolic and will not (directly) impact the size of the assembly, at least not
	///   here, and
	/// - the size of some directives is not known at this point, so a large placeholder is used instead. Because the
	///   purpose of this function is to estimate memory locations for coercing references into direct page mode, the
	///   large value will force later references into normal addressing, which is always correct.
	pub fn assembled_size(&self) -> usize {
		match self {
			// Symbolic operations take no space.
			Self::Include { .. }
			| Self::End
			| Self::PushSection
			| Self::PopSection
			| Self::UserDefinedMacro { .. }
			| Self::AssignReference { .. }
			| Self::Org(..) => 0,
			Self::Table { values, entry_size } => values.len() * *entry_size as usize,
			// Use a large assembled size as a signal that we don't know at this point. This will force any later
			// reference out of the direct page, which will always yield correct behavior.
			Self::Brr(..) => 65536,
			Self::String { text, has_null_terminator } => text.len() + (if *has_null_terminator { 1 } else { 0 }),
		}
	}
}

impl MacroParentReplacable for DirectiveValue {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Table { values, entry_size } => {
				for value in values {
					value.replace_macro_parent(replacement_parent.clone(), source_code)?;
				}
				Ok(())
			},
			Self::String { text, has_null_terminator } => Ok(()),
			Self::AssignReference { reference, value } => value.replace_macro_parent(replacement_parent, source_code),
			Self::Include { file, range } => Ok(()),
			Self::End | Self::PushSection | Self::Brr(_) | Self::PopSection | Self::Org(_) => Ok(()),
			Self::UserDefinedMacro { name, arguments, body } => Err(AssemblyError::RecursiveMacroDefinition {
				name:     (*name).to_string(),
				location: source_range(
					body.first().map_or_else(|| (0, 0).into(), |p| *p.span()).into(),
					body.last().map_or_else(|| (0, 0).into(), |p| *p.span()).into(),
				),
				src:      source_code.clone(),
			}
			.into()),
		}
	}
}
