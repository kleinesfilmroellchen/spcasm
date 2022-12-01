//! Assembly directives and macros.
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]

use std::cell::RefCell;
use std::fmt::Display;
use std::sync::Arc;

use miette::SourceSpan;
use spcasm_derive::Parse;

use crate::parser::instruction::{MemoryAddress, Number};
use crate::parser::reference::{Reference, MacroParent, MacroParentReplacable};
use crate::parser::{source_range, ProgramElement};
use crate::{AssemblyCode, AssemblyError};

/// An assembly macro.
#[derive(Clone, Debug)]
pub struct Macro {
	/// Actual data of the macro.
	pub value:       MacroValue,
	pub(crate) span: SourceSpan,
	/// Label at the start of the macro. Some macros ignore this.
	pub label:       Option<Reference>,
}

impl Default for Macro {
	fn default() -> Self {
		// We use the table macro with no entries as default as that will do nothing.
		Self { value: MacroValue::Table { values: Vec::new(), entry_size: 1 }, label: None, span: (0, 0).into() }
	}
}

impl MacroParentReplacable for Macro {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<crate::AssemblyError>> {
		self.value.replace_macro_parent(replacement_parent, source_code)
	}
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
	/// brr <file name>
	Brr(String),
	/// ascii(z) <string>
	String { text: Vec<u8>, has_null_terminator: bool },
	/// <reference> = <value>
	AssignReference { reference: Reference, value: Number },
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

impl MacroParentReplacable for MacroValue {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<crate::AssemblyError>> {
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
