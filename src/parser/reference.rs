#![allow(clippy::module_name_repetitions)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::num::NonZeroU64;
use std::sync::{Arc, Weak};

use miette::SourceSpan;
#[allow(unused)]
use smartstring::alias::String;

use super::instruction::MemoryAddress;
use super::AssemblyTimeValue;
use crate::error::AssemblyError;
use crate::AssemblyCode;

pub trait Resolvable {
	/// Whether this reference has already been resolved to a memory location.
	fn is_resolved(&self) -> bool;
	/// Resolves the reference to the given memory location.
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

/// A textual reference that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Clone, Debug)]
pub enum Reference {
	/// A reference that's the same everywhere.
	Global(Arc<RefCell<GlobalLabel>>),
	/// A reference only valid within a global label. It may be reused with a different value later on.
	Local(Arc<RefCell<LocalLabel>>),
	/// A relative label, declared with '+' or '-'.
	Relative {
		/// The direction of the label, either a '+' or a '-' label.
		direction: RelativeReferenceDirection,
		/// The id of this label, meaning how many '-' or '+' were used.
		id:        NonZeroU64,
		value:     Option<Box<AssemblyTimeValue>>,
		/// The location of definition of this label.
		span:      SourceSpan,
	},
	/// A macro argument that is resolved at a later point.
	MacroArgument {
		/// The name of the argument.
		name:         String,
		/// The resolved value of the argument. Note that this is always None for the macro definition.
		value:        Option<Box<AssemblyTimeValue>>,
		/// The location where this macro argument usage is defined. Note that because macro arguments are not coerced,
		/// this always points to the usage of the argument, never the definition in the macro argument list.
		span:         SourceSpan,
		/// The parent structure that this macro belongs to, used for resolving values.
		macro_parent: Arc<RefCell<MacroParent>>,
	},
	/// A global label placeholder that will become a unique global label for each use of the containing macro.
	/// This can be used to create a global label for a macro and use local labels within it safely.
	MacroGlobal { span: SourceSpan },
}

impl Reference {
	pub fn source_span(&self) -> SourceSpan {
		match self {
			Self::Global(global) => global.borrow().span,
			Self::Local(label) => label.borrow().span,
			Self::MacroArgument { span, .. } | Self::Relative { span, .. } | Self::MacroGlobal { span, .. } => *span,
		}
	}

	pub fn set_location(&mut self, location: AssemblyTimeValue) {
		match self {
			Self::Global(global) => global.borrow_mut().location = Some(location),
			Self::Local(local) => local.borrow_mut().location = Some(location),
			Self::Relative { value, .. } => *value = Some(location.into()),
			// noop on macro arguments
			Self::MacroArgument { .. } | Self::MacroGlobal { .. } => {},
		}
	}

	pub fn location(&self) -> Option<AssemblyTimeValue> {
		match self {
			Self::Global(global) => global.borrow().location.clone(),
			Self::Local(local) => local.borrow().location.clone(),
			Self::MacroArgument { value, .. } | Self::Relative { value, .. } => value.clone().map(|boxed| *boxed),
			Self::MacroGlobal { .. } => None,
		}
	}
}

impl ReferenceResolvable for Reference {
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
				let parameters = &macro_parent.borrow().parameters;
				parameters.get_value_of(name).map_or_else(
					|| {
						if parameters.has_argument_named(name) {
							// The parent is formal arguments, so we are a valid argument but there is no value yet.
							Ok(())
						} else {
							Err(AssemblyError::UnknownMacroArgument {
								name:            (*name).to_string().into(),
								available_names: parameters.argument_names(),
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
			Self::MacroGlobal { .. } | Self::Relative { .. } => Ok(()),
		}
	}

	fn resolve_relative_labels(
		&mut self,
		direction: RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RefCell<GlobalLabel>>>,
	) {
		// Only try to borrow mutably globals and locals. If there are circular references, this borrowing will fail and
		// we can stop resolving, since there will be a resolution error later on anyways.
		match self {
			Self::Global(global) => {
				let _ = global
					.try_borrow_mut()
					.map(|mut reference| reference.resolve_relative_labels(direction, relative_labels));
			},
			Self::Local(local) => {
				let _ = local
					.try_borrow_mut()
					.map(|mut reference| reference.resolve_relative_labels(direction, relative_labels));
			},
			Self::MacroArgument { value, .. } => {
				value.as_mut().map(|value| value.resolve_relative_labels(direction, relative_labels));
			},
			Self::MacroGlobal { .. } | Self::Relative { .. } => (),
		}
	}
}

impl Display for Reference {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::Global(global) => global.borrow().name.clone(),
			Self::Local(local) => format!(".{}", local.borrow().name).into(),
			Self::MacroArgument { name, .. } => format!("<{}>", name).into(),
			Self::MacroGlobal { .. } => "\\@".to_string().into(),
			Self::Relative { direction, .. } => direction.string().into(),
		})
	}
}

impl PartialEq for Reference {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Self::Global(label) => match other {
				Self::Global(other_label) => label.eq(other_label),
				_ => false,
			},
			Self::Local(..) |
			Self::Relative { .. } |
			// Equality doesn't really make sense for dynamic user macro globals.
			Self::MacroGlobal { .. } => false,
			Self::MacroArgument { name, .. } => match other {
				Self::MacroArgument { name: other_name, .. } => name.eq(other_name),
				_ => false,
			},
		}
	}
}

impl Resolvable for Reference {
	#[must_use]
	fn is_resolved(&self) -> bool {
		match self {
			Self::Global(label) => label.borrow().is_resolved(),
			Self::Local(label) => label.borrow().is_resolved(),
			Self::MacroArgument { value: Some(_), .. } | Self::Relative { value: Some(_), .. } => true,
			Self::MacroArgument { .. } | Self::Relative { .. } | Self::MacroGlobal { .. } => false,
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
			Self::MacroArgument { value, .. } | Self::Relative { value, .. } => {
				*value = Some(Box::new(AssemblyTimeValue::Literal(location)));
				Ok(())
			},
			Self::MacroGlobal { .. } => unimplemented!("macro global leaked to reference resolution, what to do?"),
		}
	}
}

/// The direction of a relative reference.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RelativeReferenceDirection {
	/// A "+" label, referencing some later "+" label.
	Forward,
	/// A "-" label, referencing some previous "-" label.
	Backward,
}

impl RelativeReferenceDirection {
	pub const fn string(self) -> &'static str {
		match self {
			Self::Forward => "+",
			Self::Backward => "-",
		}
	}
}

impl Display for RelativeReferenceDirection {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.string())
	}
}

/// A textual reference that refers to some location in memory and resolves to a numeric value at some point. It is
/// global, meaning that it refers to the same value everywhere.
#[derive(Clone, Debug)]
pub struct GlobalLabel {
	/// User-given reference name.
	pub name:            String,
	/// Resolved memory location of the reference, if any.
	pub location:        Option<AssemblyTimeValue>,
	/// Source code location where this reference is defined.
	pub span:            SourceSpan,
	/// Whether anyone references this reference as an address.
	pub used_as_address: bool,
	/// Local labels belonging to this global label.
	pub locals:          HashMap<String, Arc<RefCell<LocalLabel>>>,
}

impl ReferenceResolvable for GlobalLabel {
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

	fn resolve_relative_labels(
		&mut self,
		direction: RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RefCell<GlobalLabel>>>,
	) {
		self.location.as_mut().map(|location| location.resolve_relative_labels(direction, relative_labels));
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
		_usage_span: SourceSpan,
		_source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.location = Some(AssemblyTimeValue::Literal(location));
		Ok(())
	}
}

/// A textual reference that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Debug, Clone)]
pub struct LocalLabel {
	/// User-given reference name.
	pub name:     String,
	/// Resolved memory location of the reference, if any.
	pub location: Option<AssemblyTimeValue>,
	/// Source code location where this reference is defined.
	pub span:     SourceSpan,
	/// The parent label that this local label is contained within.
	pub parent:   Weak<RefCell<GlobalLabel>>,
}

impl ReferenceResolvable for LocalLabel {
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

	fn resolve_relative_labels(
		&mut self,
		direction: RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RefCell<GlobalLabel>>>,
	) {
		self.location.as_mut().map(|location| location.resolve_relative_labels(direction, relative_labels));
	}
}

impl Resolvable for LocalLabel {
	fn is_resolved(&self) -> bool {
		self.location.is_some()
	}

	fn resolve_to(
		&mut self,
		location: MemoryAddress,
		_usage_span: SourceSpan,
		_source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.location = Some(AssemblyTimeValue::Literal(location));
		Ok(())
	}
}

/// The structure holding all of the data for the macro arguments.
#[derive(Debug, Clone)]
pub struct MacroParent {
	label:          Arc<RefCell<GlobalLabel>>,
	pub parameters: MacroParameters,
}

impl MacroParent {
	pub fn new_actual(parameters: HashMap<String, AssemblyTimeValue>, label: GlobalLabel) -> Arc<RefCell<Self>> {
		Arc::new(RefCell::new(Self {
			label:      Arc::new(RefCell::new(label)),
			parameters: MacroParameters::Actual(parameters),
		}))
	}

	pub fn new_formal(parameters: Option<Vec<(String, SourceSpan)>>, span: SourceSpan) -> Arc<RefCell<Self>> {
		Arc::new(RefCell::new(Self {
			label:      Arc::new(RefCell::new(GlobalLabel {
				name: "macro global placeholder".into(),
				location: None,
				span,
				used_as_address: false,
				locals: HashMap::new(),
			})),
			parameters: MacroParameters::Formal(parameters.unwrap_or_default()),
		}))
	}

	pub fn global_label(&self) -> Arc<RefCell<GlobalLabel>> {
		self.label.clone()
	}
}

/// The kinds of parameters that a macro parent can have.
#[derive(Debug, Clone)]
pub enum MacroParameters {
	/// Formal parameters, used in the macro's definition.
	Formal(Vec<(String, SourceSpan)>),
	/// Actual parameters, used while a macro is being resolved.
	Actual(HashMap<String, AssemblyTimeValue>),
}

impl MacroParameters {
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

	pub fn get_value_of(&self, name: &str) -> Option<AssemblyTimeValue> {
		match self {
			Self::Formal(..) => None,
			Self::Actual(list) => list.get(name).cloned(),
		}
	}
}

pub trait ReferenceResolvable {
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

	/// Replace any relative label with the stand-in global label from the given list, if possible. Only operates in one
	/// direction, since the two directions are entirely distinct.
	fn resolve_relative_labels(
		&mut self,
		direction: RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RefCell<GlobalLabel>>>,
	);
}

/// Ensures that the local label is referencing the parent, and that the parent is referencing the local. The local
/// is overwritten with an identical local label in the parent if necessary, but any memory value in either label
/// "copy" is preserved. Thereby, this function deduplicates local labels and ensures parent references.
pub fn merge_local_into_parent(
	mut local: Arc<RefCell<LocalLabel>>,
	current_global_label: Option<Arc<RefCell<GlobalLabel>>>,
	source_code: &Arc<AssemblyCode>,
) -> Result<Arc<RefCell<LocalLabel>>, Box<AssemblyError>> {
	if let Some(actual_global_label) = current_global_label {
		let mut mutable_global = actual_global_label.borrow_mut();
		let reference_value = local.borrow().location.clone();
		let reference_name = local.borrow().name.clone();

		// Only assign the parent if there is none at the moment.
		if local.borrow_mut().parent.upgrade().is_none() {
			local = mutable_global.locals.entry(reference_name).or_insert_with(|| local).clone();
			let mut mutable_local = local.borrow_mut();
			mutable_local.parent = Arc::downgrade(&actual_global_label);
		}
		let mut mutable_local = local.borrow_mut();
		mutable_local.location = mutable_local.location.clone().or(reference_value);
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
