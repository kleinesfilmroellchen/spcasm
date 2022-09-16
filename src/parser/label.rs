#![allow(clippy::module_name_repetitions)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::{Arc, Weak};

use miette::SourceSpan;

use super::instruction::{MemoryAddress, Number};
use crate::error::{AssemblyCode, AssemblyError};

pub trait Resolvable {
	/// Whether this label has already been resolved to a memory location.
	fn is_resolved(&self) -> bool;
	/// Resolves the label to the given memory location.
	fn resolve_to(&mut self, location: MemoryAddress, usage_span: SourceSpan, source_code: Arc<AssemblyCode>);
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Clone, Debug)]
pub enum Label {
	/// A label that's the same everywhere.
	Global(Arc<RefCell<GlobalLabel>>),
	/// A label only valid within a global label. It may be reused with a different value later on.
	Local(Arc<RefCell<LocalLabel>>),
}

impl Label {
	pub fn source_span(&self) -> SourceSpan {
		match self {
			Self::Global(global) => global.borrow().span,
			Self::Local(label) => label.borrow().span,
		}
	}

	pub fn set_location(&mut self, location: Number) {
		match self {
			Self::Global(global) => global.borrow_mut().location = Some(location),
			Self::Local(local) => local.borrow_mut().location = Some(Box::new(location)),
		}
	}
}

impl Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::Global(global) => global.borrow().name.clone(),
			Self::Local(local) => format!(".{}", local.borrow().name),
		})
	}
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
			Self::Global(label) => label.borrow().is_resolved(),
			Self::Local(label) => label.borrow().is_resolved(),
		}
	}

	fn resolve_to(&mut self, location: MemoryAddress, usage_span: SourceSpan, source_code: Arc<AssemblyCode>) {
		match self {
			Self::Global(label) => label.borrow_mut().resolve_to(location, usage_span, source_code),
			Self::Local(label) => label.borrow_mut().resolve_to(location, usage_span, source_code),
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
	pub location:        Option<Number>,
	/// Source code location where this label is defined.
	pub span:            SourceSpan,
	/// Whether anyone references this label as an address.
	pub used_as_address: bool,
	/// Local labels belonging to this global label.
	pub locals:          HashMap<String, Arc<RefCell<LocalLabel>>>,
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

	fn resolve_to(&mut self, location: MemoryAddress, usage_span: SourceSpan, source_code: Arc<AssemblyCode>) {
		if location <= 0xFF && self.used_as_address {
			println!(
				"{:?}",
				miette::Report::new(AssemblyError::NonDirectPageLabel {
					name:             self.name.clone(),
					label_definition: self.span,
					address:          location,
					location:         usage_span,
					src:              source_code,
				})
			);
		}
		self.location = Some(Number::Literal(location));
	}
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Debug, Clone)]
pub struct LocalLabel {
	/// User-given label name.
	pub name:     String,
	/// Resolved memory location of the label, if any.
	pub location: Option<Box<Number>>,
	/// Source code location where this label is defined.
	pub span:     SourceSpan,
	/// The parent label that this local label is contained within.
	pub parent:   Weak<RefCell<GlobalLabel>>,
}

impl LocalLabel {
	pub fn new(name: String, span: SourceSpan, parent: &Arc<RefCell<GlobalLabel>>) -> Self {
		Self { name, span, location: None, parent: Arc::downgrade(parent) }
	}

	pub fn strong_parent(&self) -> Arc<RefCell<GlobalLabel>> {
		self.parent.upgrade().expect("Parent deleted before label resolution finished")
	}
}

impl Resolvable for LocalLabel {
	fn is_resolved(&self) -> bool {
		self.location.is_some()
	}

	fn resolve_to(&mut self, location: MemoryAddress, usage_span: SourceSpan, source_code: Arc<AssemblyCode>) {
		if location <= 0xFF {
			println!(
				"{:?}",
				miette::Report::new(AssemblyError::NonDirectPageLabel {
					name:             format!(".{}", self.name),
					label_definition: self.span,
					address:          location,
					location:         usage_span,
					src:              source_code,
				})
			);
		}
		self.location = Some(Box::new(Number::Literal(location)));
	}
}

/// Ensures that the local label is referencing the parent, and that the parent is referencing the local. The local
/// is overwritten with an identical local label in the parent if necessary, but any memory value in either label
/// "copy" is preserved. Thereby, this function deduplicates local labels and ensures parent references.
pub fn merge_local_into_parent(
	mut local: Arc<RefCell<LocalLabel>>,
	mut current_global_label: Option<Arc<RefCell<GlobalLabel>>>,
	source_code: &Arc<AssemblyCode>,
) -> Result<Arc<RefCell<LocalLabel>>, AssemblyError> {
	if let Some(mut actual_global_label) = current_global_label {
		let mut mutable_global = actual_global_label.borrow_mut();
		let label_value = local.borrow().location.clone();
		let label_name = local.borrow().name.clone();

		local = mutable_global.locals.entry(label_name).or_insert_with(|| local).clone();
		let mut mutable_local = local.borrow_mut();
		mutable_local.parent = Arc::downgrade(&actual_global_label);
		mutable_local.location = mutable_local.location.clone().or(label_value);
		drop(mutable_local);
		Ok(local)
	} else {
		Err(AssemblyError::MissingGlobalLabel {
			local_label: local.borrow().name.clone(),
			src:         source_code.clone(),
			location:    local.borrow().span,
		})
	}
}
