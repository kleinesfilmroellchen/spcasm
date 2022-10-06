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
	/// # Errors
	/// Warnings and errors are passed on to the caller. **The caller must separate out warnings and print or discard
	/// them!**
	fn resolve_to(
		&mut self,
		location: MemoryAddress,
		usage_span: SourceSpan,
		source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>>;
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Clone, Debug)]
pub enum Label {
	/// A label that's the same everywhere.
	Global(Arc<RefCell<GlobalLabel>>),
	/// A label only valid within a global label. It may be reused with a different value later on.
	Local(Arc<RefCell<LocalLabel>>),
	/// A macro argument that is resolved at a later point.
	MacroArgument {
		/// The name of the argument.
		name:         String,
		/// The resolved value of the argument. Note that this is always None for the macro definition.
		value:        Option<Box<Number>>,
		/// The location where this macro argument usage is defined. Note that because macro arguments are not coerced,
		/// this always points to the usage of the argument, never the definition in the macro argument list.
		span:         SourceSpan,
		/// The parent structure that this macro belongs to, used for resolving values.
		macro_parent: Arc<RefCell<MacroParent>>,
	},
}

impl Label {
	pub fn source_span(&self) -> SourceSpan {
		match self {
			Self::Global(global) => global.borrow().span,
			Self::Local(label) => label.borrow().span,
			Self::MacroArgument { span, .. } => *span,
		}
	}

	pub fn set_location(&mut self, location: Number) {
		match self {
			Self::Global(global) => global.borrow_mut().location = Some(location),
			Self::Local(local) => local.borrow_mut().location = Some(Box::new(location)),
			// noop on macro arguments
			Self::MacroArgument { .. } => {},
		}
	}
}

impl MacroParentReplacable for Label {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Global(global) => global.borrow_mut().replace_macro_parent(replacement_parent, source_code),
			Self::Local(local) => local.borrow_mut().replace_macro_parent(replacement_parent, source_code),
			Self::MacroArgument { macro_parent, name, span, value } => {
				*macro_parent = replacement_parent;
				macro_parent.borrow().get_value_of(name).map_or_else(
					|| {
						if macro_parent.borrow().has_argument_named(name) {
							// The parent is formal arguments, so we are a valid argument but there is no value yet.
							Ok(())
						} else {
							Err(AssemblyError::UnknownMacroArgument {
								name:            (*name).to_string(),
								available_names: macro_parent.borrow().argument_names(),
								location:        *span,
								src:             source_code.clone(),
							}
							.into())
						}
					},
					|argument_value| {
						*value = Some(Box::new(argument_value));
						Ok(())
					},
				)
			},
		}
	}
}

impl Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::Global(global) => global.borrow().name.clone(),
			Self::Local(local) => format!(".{}", local.borrow().name),
			Self::MacroArgument { name, .. } => format!("<{}>", name),
		})
	}
}

impl PartialEq for Label {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Self::Global(label) => match other {
				Self::Global(other_label) => label.eq(other_label),
				_ => false,
			},
			Self::Local(..) => false,
			Self::MacroArgument { name, .. } => match other {
				Self::MacroArgument { name: other_name, .. } => name.eq(other_name),
				_ => false,
			},
		}
	}
}

impl Resolvable for Label {
	#[must_use]
	fn is_resolved(&self) -> bool {
		match self {
			Self::Global(label) => label.borrow().is_resolved(),
			Self::Local(label) => label.borrow().is_resolved(),
			Self::MacroArgument { value: Some(resolved), .. } => true,
			Self::MacroArgument { .. } => false,
		}
	}

	fn resolve_to(
		&mut self,
		location: MemoryAddress,
		usage_span: SourceSpan,
		source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Global(label) => label.borrow_mut().resolve_to(location, usage_span, source_code),
			Self::Local(label) => label.borrow_mut().resolve_to(location, usage_span, source_code),
			Self::MacroArgument { name, value, span, .. } => {
				*value = Some(Box::new(Number::Literal(location)));
				Ok(())
			},
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

impl MacroParentReplacable for GlobalLabel {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.location
			.as_mut()
			.map(|location| location.replace_macro_parent(replacement_parent, source_code))
			.transpose()
			.map(|_| ())
	}
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

	fn resolve_to(
		&mut self,
		location: MemoryAddress,
		usage_span: SourceSpan,
		source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.location = Some(Number::Literal(location));
		if location <= 0xFF && self.used_as_address {
			Err(AssemblyError::NonDirectPageLabel {
				name:             self.name.clone(),
				label_definition: self.span,
				address:          location,
				location:         usage_span,
				src:              source_code,
			}
			.into())
		} else {
			Ok(())
		}
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

impl MacroParentReplacable for LocalLabel {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.location
			.as_mut()
			.map(|location| location.replace_macro_parent(replacement_parent, source_code))
			.transpose()
			.map(|_| ())
	}
}

impl Resolvable for LocalLabel {
	fn is_resolved(&self) -> bool {
		self.location.is_some()
	}

	fn resolve_to(
		&mut self,
		location: MemoryAddress,
		usage_span: SourceSpan,
		source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.location = Some(Box::new(Number::Literal(location)));
		if location <= 0xFF {
			Err(AssemblyError::NonDirectPageLabel {
				name:             format!(".{}", self.name),
				label_definition: self.span,
				address:          location,
				location:         usage_span,
				src:              source_code,
			}
			.into())
		} else {
			Ok(())
		}
	}
}

/// The structure holding all of the data for the macro arguments.
#[derive(Debug, Clone)]
pub enum MacroParent {
	Formal(Vec<(String, SourceSpan)>),
	Actual(HashMap<String, Number>),
}

impl MacroParent {
	/// Returns whether the macro parent has any argument with this name. This function will usually be faster than
	/// searching through the result of `argument_names`.
	pub fn has_argument_named(&self, name: &str) -> bool {
		match self {
			Self::Formal(list) => list.iter().any(|(formal_name, _)| formal_name == name),
			Self::Actual(list) => list.contains_key(name),
		}
	}

	/// Returns all argument names that this macro parent has.
	pub fn argument_names(&self) -> Vec<String> {
		match self {
			Self::Formal(list) => list.clone().into_iter().map(|(name, _)| name).collect(),
			Self::Actual(list) => list.keys().map(String::clone).collect(),
		}
	}

	pub fn get_value_of(&self, name: &str) -> Option<Number> {
		match self {
			Self::Formal(..) => None,
			Self::Actual(list) => list.get(name).cloned(),
		}
	}
}

pub trait MacroParentReplacable {
	/// Replace any macro argument's parent with the given macro parent. The macro parent stores the current macro's
	/// argument list.
	///
	/// # Errors
	/// If a macro argument is missing from the parent, it means that it is misnamed and an error is returned.
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>>;
}

/// Ensures that the local label is referencing the parent, and that the parent is referencing the local. The local
/// is overwritten with an identical local label in the parent if necessary, but any memory value in either label
/// "copy" is preserved. Thereby, this function deduplicates local labels and ensures parent references.
pub fn merge_local_into_parent(
	mut local: Arc<RefCell<LocalLabel>>,
	mut current_global_label: Option<Arc<RefCell<GlobalLabel>>>,
	source_code: &Arc<AssemblyCode>,
) -> Result<Arc<RefCell<LocalLabel>>, Box<AssemblyError>> {
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
		}
		.into())
	}
}
