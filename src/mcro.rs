//! Assembly directives and macros.
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]

use std::fmt::Display;

use miette::SourceSpan;
use spcasm_derive::Parse;

use crate::parser::instruction::{MemoryAddress, Number};
use crate::parser::label::Label;

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
	Brr,
	Incbin,
	Include,
	End,
	Pushpc,
	Pullpc,
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
	/// <label> = <value>
	AssignLabel { label: Label, value: Number },
	/// incbin <file name>
	Include { file: String, range: Option<SourceSpan> },
	/// end
	End,
	/// pushpc
	PushSection,
	/// pullpc
	PopSection,
}
