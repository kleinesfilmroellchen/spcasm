#![allow(clippy::module_name_repetitions)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::{Arc, Weak};

use miette::SourceSpan;

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
			Self::MacroArgument { span, .. } | Self::MacroGlobal { span, .. } => *span,
		}
	}

	pub fn set_location(&mut self, location: AssemblyTimeValue) {
		match self {
			Self::Global(global) => global.borrow_mut().location = Some(location),
			Self::Local(local) => local.borrow_mut().location = Some(Box::new(location)),
			// noop on macro arguments
			Self::MacroArgument { .. } | Self::MacroGlobal { .. } => {},
		}
	}
}

impl MacroParentReplacable for Reference {
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
								name:            (*name).to_string(),
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
			Self::MacroGlobal { .. } => Ok(()),
		}
	}
}

impl Display for Reference {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::Global(global) => global.borrow().name.clone(),
			Self::Local(local) => format!(".{}", local.borrow().name),
			Self::MacroArgument { name, .. } => format!("<{}>", name),
			Self::MacroGlobal { .. } => "\\@".to_string(),
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
			Self::MacroArgument { value: Some(_), .. } => true,
			Self::MacroArgument { .. } | Self::MacroGlobal { .. } => false,
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
			Self::MacroArgument { value, .. } => {
				*value = Some(Box::new(AssemblyTimeValue::Literal(location)));
				Ok(())
			},
			Self::MacroGlobal { .. } => unimplemented!("macro global leaked to reference resolution, what to do?"),
		}
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
	pub location: Option<Box<AssemblyTimeValue>>,
	/// Source code location where this reference is defined.
	pub span:     SourceSpan,
	/// The parent label that this local label is contained within.
	pub parent:   Weak<RefCell<GlobalLabel>>,
}

impl LocalLabel {
	pub fn new(name: String, span: SourceSpan, parent: &Arc<RefCell<GlobalLabel>>) -> Self {
		Self { name, span, location: None, parent: Arc::downgrade(parent) }
	}

	pub fn strong_parent(&self) -> Arc<RefCell<GlobalLabel>> {
		self.parent.upgrade().expect("Parent deleted before reference resolution finished")
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
		_usage_span: SourceSpan,
		_source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.location = Some(Box::new(AssemblyTimeValue::Literal(location)));
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
	current_global_label: Option<Arc<RefCell<GlobalLabel>>>,
	source_code: &Arc<AssemblyCode>,
) -> Result<Arc<RefCell<LocalLabel>>, Box<AssemblyError>> {
	if let Some(actual_global_label) = current_global_label {
		let mut mutable_global = actual_global_label.borrow_mut();
		let reference_value = local.borrow().location.clone();
		let reference_name = local.borrow().name.clone();

		local = mutable_global.locals.entry(reference_name).or_insert_with(|| local).clone();
		let mut mutable_local = local.borrow_mut();
		mutable_local.parent = Arc::downgrade(&actual_global_label);
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
