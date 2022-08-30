#![allow(clippy::module_name_repetitions)]

use std::collections::HashMap;
use std::sync::{Arc, Weak};

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

impl PartialEq for Label {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Self::Global(label) => match other {
				Self::Global(other_label) => label.eq(other_label),
				Self::Local(..) => false,
			},
			Self::Local(..) => false,
		}
	}
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
#[derive(Clone, Debug)]
pub struct GlobalLabel {
	/// User-given label name.
	pub name:            String,
	/// Resolved memory location of the label, if any.
	pub location:        Option<MemoryAddress>,
	/// Source code location where this label is defined.
	pub span:            SourceSpan,
	/// Whether anyone references this label as an address.
	pub used_as_address: bool,
	/// Local labels belonging to this global label.
	pub locals:          HashMap<String, LocalLabel>,
}

impl PartialEq for GlobalLabel {
	fn eq(&self, other: &Self) -> bool {
		self.name == other.name
	}
}
impl Eq for GlobalLabel {}

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
	pub parent:   Weak<GlobalLabel>,
}

impl LocalLabel {
	pub fn new(name: String, span: SourceSpan, parent: &Arc<GlobalLabel>) -> Self {
		Self { name, span, location: None, parent: Arc::downgrade(parent) }
	}

	pub fn strong_parent(&self) -> Arc<GlobalLabel> {
		self.parent.upgrade().expect("Parent deleted before label resolution finished")
	}
}

impl Resolvable for LocalLabel {
	fn is_resolved(&self) -> bool {
		self.location.is_some()
	}

	fn resolve_to(&mut self, location: MemoryAddress, source_code: Arc<AssemblyCode>) {
		if location <= 0xFF {
			println!(
				"{:?}",
				miette::Report::new(AssemblyError::NonDirectPageLabel {
					name:     format!(".{}", self.name),
					location: self.span,
					src:      source_code,
				})
			);
		}
		self.location = Some(location);
	}
}
