//! Semantic analysis and AST datastructures.

mod addressing_mode;
mod environment;
mod file;
pub mod instruction;
mod program;
pub(crate) mod reference;
mod register;
#[cfg(test)] mod test;
pub mod value;

pub use addressing_mode::{AddressingMode, AddressingModeCategory};
pub use environment::Environment;
pub use file::AssemblyFile;
pub use program::ProgramElement;
pub use reference::Reference;
pub use register::Register;
pub use value::AssemblyTimeValue;

/// How a looked-up reference is used. See [`crate::Environment::get_global_label`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum LabelUsageKind {
	/// Reference is used as a parameter, i.e. it's address is of interest.
	AsAddress,
	/// Label is being defined.
	AsDefinition,
}
