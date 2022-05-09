//! Assembly directives and macros.
#![allow(clippy::module_name_repetitions)]
use miette::SourceSpan;

use super::instruction::MemoryAddress;
/// An assembly macro.
#[derive(Clone, Copy, Debug)]
pub struct Macro {
	/// Actual data of the macro.
	pub value:       MacroValue,
	pub(crate) span: SourceSpan,
}

///An assembly macro's value and relevant data.
#[derive(Clone, Copy, Debug)]
pub enum MacroValue {
	/// org <memory address>
	Org(MemoryAddress),
}
