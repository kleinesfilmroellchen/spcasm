use std::sync::Arc;

use miette::SourceSpan;

use super::error::{AssemblyCode, AssemblyError};
use super::instruction::MemoryAddress;

pub trait Resolvable {
	/// Whether this label has already been resolved to a memory location.
	fn is_resolved(&self) -> bool;
	/// Resolves the label to the given memory location.
	fn resolve_to(&mut self, location: MemoryAddress, source_code: Arc<AssemblyCode>);
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Clone, Debug)]
pub enum Label {
	/// A label that's the same everywhere.
	Global(Arc<GlobalLabel>),
	/// A label only valid within a global label. It may be reused with a different value later on.
	Local(LocalLabel),
}

impl Resolvable for Label {
	#[must_use]
	fn is_resolved(&self) -> bool {
		match self {
			Self::Global(label) => label.is_resolved(),
			Self::Local(label) => label.is_resolved(),
		}
	}

	fn resolve_to(&mut self, location: MemoryAddress, source_code: Arc<AssemblyCode>) {
		match self {
			Self::Global(label) => unsafe { Arc::get_mut_unchecked(label) }.resolve_to(location, source_code),
			Self::Local(label) => label.resolve_to(location, source_code),
		}
	}
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point. It is global,
/// meaning that it refers to the same value everywhere.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GlobalLabel {
	/// User-given label name.
	pub name:            String,
	/// Resolved memory location of the label, if any.
	pub location:        Option<MemoryAddress>,
	/// Source code location where this label is defined.
	pub span:            SourceSpan,
	/// Whether anyone references this label as an address.
	pub used_as_address: bool,
}

impl Resolvable for GlobalLabel {
	#[must_use]
	fn is_resolved(&self) -> bool {
		self.location.is_some()
	}

	fn resolve_to(&mut self, location: MemoryAddress, source_code: Arc<AssemblyCode>) {
		if location <= 0xFF && self.used_as_address {
			println!(
				"{:?}",
				miette::Report::new(AssemblyError::NonDirectPageLabel {
					name:     self.name.clone(),
					location: self.span,
					src:      source_code,
				})
			);
		}
		self.location = Some(location);
	}
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Debug, Clone)]
pub struct LocalLabel {
	/// User-given label name.
	pub name:     String,
	/// Resolved memory location of the label, if any.
	pub location: Option<MemoryAddress>,
	/// Source code location where this label is defined.
	pub span:     SourceSpan,
	/// The parent label that this local label is contained within.
	pub parent:   Arc<GlobalLabel>,
}

impl LocalLabel {
	pub fn new(name: String, span: SourceSpan, parent: Arc<GlobalLabel>) -> Self {
		LocalLabel { name, span, location: None, parent }
	}
}

impl Resolvable for LocalLabel {
	fn is_resolved(&self) -> bool {
		todo!()
	}

	fn resolve_to(&mut self, _location: MemoryAddress, _source_code: Arc<AssemblyCode>) {
		todo!()
	}
}
