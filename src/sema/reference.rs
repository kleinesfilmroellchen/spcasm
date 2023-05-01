#![allow(clippy::module_name_repetitions)]

use std::collections::{BTreeMap, HashMap};
use std::fmt::Display;
use std::num::{NonZeroU64, NonZeroUsize};
use std::sync::{Arc, Weak};

use miette::SourceSpan;
use parking_lot::RwLock;
#[allow(unused)]
use smartstring::alias::String;

use super::instruction::MemoryAddress;
use super::{AssemblyTimeValue, LabelUsageKind};
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
	/// A reference that is most often used to label some place in memory.
	/// Labels form a hierarchical structure among themselves.
	Label(Arc<RwLock<Label>>),
	/// A local label that knows its nesting count, but hasn't been converted to a real label yet due to lack of
	/// environmental information when it was created.
	UnresolvedLocalLabel {
		/// Name of the label.
		name:          String,
		/// Nesting level of the label, indicated by the number of preceding dots.
		/// This is important for figuring out which label is this label's parent.
		nesting_level: NonZeroUsize,
		/// The location of definition of this label.
		span:          SourceSpan,
		/// The final value of the label, if it has already been assigned one.
		value:         Option<Box<AssemblyTimeValue>>,
	},
	/// A relative label, declared with '+' or '-'.
	Relative {
		/// The direction of the label, either a '+' or a '-' label.
		direction: RelativeReferenceDirection,
		/// The id of this label, meaning how many '-' or '+' were used.
		id:        NonZeroU64,
		/// The final value of the label, if it has already been assigned one.
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
		macro_parent: Arc<RwLock<MacroParent>>,
	},
	/// A global label placeholder that will become a unique global label for each use of the containing macro.
	/// This can be used to create a global label for a macro and use local labels within it safely.
	MacroGlobal {
		/// Source code location of the reference.
		span: SourceSpan,
	},
}

impl Reference {
	/// Returns the source span where this reference is defined. In some instances, this will return a usage span of a
	/// reference that wasn't actually defined.
	#[must_use]
	pub fn source_span(&self) -> SourceSpan {
		match self {
			Self::Label(global) => global.read().source_span(),
			Self::MacroArgument { span, .. }
			| Self::UnresolvedLocalLabel { span, .. }
			| Self::Relative { span, .. }
			| Self::MacroGlobal { span, .. } => *span,
		}
	}

	/// Returns the user-specified name of the reference.
	#[allow(clippy::missing_panics_doc)]
	#[must_use]
	pub fn name(&self) -> String {
		match self {
			Self::Label(label) => label.read_recursive().name.clone(),
			Self::Relative { direction, id, .. } =>
				direction.string().repeat(usize::try_from(u64::from(*id)).unwrap()).into(),
			Self::UnresolvedLocalLabel { name, .. } | Self::MacroArgument { name, .. } => name.clone(),
			Self::MacroGlobal { .. } => "\\@".into(),
		}
	}

	/// Sets the value of the reference.
	pub fn set_location(&mut self, location: AssemblyTimeValue) {
		match self {
			Self::Label(global) => global.write().location = Some(location),
			Self::UnresolvedLocalLabel { value, .. } | Self::Relative { value, .. } => *value = Some(location.into()),
			// noop on macro arguments
			Self::MacroArgument { .. } | Self::MacroGlobal { .. } => {},
		}
	}

	/// Returns the value of the reference.
	#[must_use]
	pub fn location(&self) -> Option<AssemblyTimeValue> {
		match self {
			Self::Label(global) => global.read().location.clone(),
			Self::MacroArgument { value, .. } | Self::Relative { value, .. } => value.clone().map(|boxed| *boxed),
			Self::MacroGlobal { .. } | Self::UnresolvedLocalLabel { .. } => None,
		}
	}

	/// Sets the current label at the position that this reference is used, and additionally specifies how the label is
	/// used here: as an address/value or as the definition itself?
	///
	/// # Errors
	/// If there is no global label, but we try to create a local label, a "missing global label" error is returned.
	pub fn set_current_label_with_kind(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		kind: LabelUsageKind,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::UnresolvedLocalLabel { name, nesting_level, span, value } =>
				try {
					*self = Self::Label(create_local_at_this_position(
						name.clone(),
						*nesting_level,
						*span,
						value.clone(),
						kind,
						current_label.clone(),
						source_code,
					)?)
				},
			Self::MacroArgument { value: Some(value), .. } | Self::Relative { value: Some(value), .. } =>
				value.set_current_label(current_label, source_code),
			Self::Label(_) | Self::MacroGlobal { .. } | Self::MacroArgument { .. } | Self::Relative { .. } => Ok(()),
		}
	}
}

impl ReferenceResolvable for Reference {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RwLock<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Label(global) => global.write().replace_macro_parent(replacement_parent, source_code),
			Self::MacroArgument { macro_parent, name, span, value } => {
				*macro_parent = replacement_parent;
				let parameters = &macro_parent.read().parameters;
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
			Self::MacroGlobal { .. } | Self::Relative { .. } | Self::UnresolvedLocalLabel { .. } => Ok(()),
		}
	}

	fn resolve_relative_labels(
		&mut self,
		direction: RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RwLock<Label>>>,
	) {
		// Only try to borrow mutably globals and locals. If there are circular references, this borrowing will fail and
		// we can stop resolving, since there will be a resolution error later on anyways.
		match self {
			Self::Label(global) => {
				let _ = global
					.try_write()
					.map(|mut reference| reference.resolve_relative_labels(direction, relative_labels));
			},
			Self::MacroArgument { value, .. } =>
				if let Some(value) = value.as_mut() {
					value.resolve_relative_labels(direction, relative_labels);
				},
			Self::MacroGlobal { .. } | Self::Relative { .. } | Self::UnresolvedLocalLabel { .. } => (),
		}
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		match self {
			Self::Label(this_label) => {
				if !this_label.try_read().is_some_and(|label| label.has_definition()) {
					// Keep a list of possibly matching labels, as a pair of (pseudo_name, label).
					// Repeatedly extend that list with the children of candidates where the pseudo_name matches a
					// prefix of our name.
					let global_candidates = global_labels.iter().cloned().filter_map(|label| {
						// Recursive labels; will error later but we do not want to deadlock.
						if label.is_locked() {
							None
						} else {
							// Hack to force a drop of the read lock before label is moved later.
							let name = {
								let name = label.read().name.clone();
								name
							};
							if this_label.read().name.starts_with(name.as_str()) {
								Some((name, label))
							} else {
								None
							}
						}
					});

					let mut candidates: Vec<(String, _)> = Vec::new();
					macro_rules! run_check_on_labels {
						($labels:expr) => {
							for (label_name, candidate) in $labels {
								if this_label.read().name == label_name && candidate.read().has_definition() {
									*this_label = candidate;
									return;
								}

								for (child_name, child_label) in &candidate.read().children {
									let combined_name = format!("{}_{}", label_name, child_name);
									if this_label.read().name.starts_with(&combined_name) {
										candidates.push((combined_name.into(), child_label.clone()));
									}
								}
							}
						};
					}

					run_check_on_labels!(global_candidates);
					while !candidates.is_empty() {
						let old_labels = candidates;
						candidates = Vec::new();
						run_check_on_labels!(old_labels);
					}
				}
				// Only try to borrow mutably globals and locals. If there are circular references, this borrowing will
				// fail and we can stop resolving, since there will be a resolution error later on anyways.
				if let Some(mut reference) = this_label.try_write() {
					reference.resolve_pseudo_labels(global_labels);
				}
			},
			Self::MacroArgument { value, .. } =>
				if let Some(value) = value.as_mut() {
					value.resolve_pseudo_labels(global_labels);
				},
			Self::MacroGlobal { .. } | Self::Relative { .. } | Self::UnresolvedLocalLabel { .. } => (),
		}
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		// Any callers that are aware of us being a definition will not call the trait function, but the with_kind
		// function directly.
		self.set_current_label_with_kind(current_label, LabelUsageKind::AsAddress, source_code)
	}
}

impl Display for Reference {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.pad(&match self {
			Self::Label(label) => label.read().to_string(),
			Self::UnresolvedLocalLabel { name, nesting_level, .. } =>
				format!("{}{}", ".".repeat((*nesting_level).into()), name),
			Self::MacroArgument { name, .. } => format!("<{}>", name),
			Self::MacroGlobal { .. } => "\\@".to_string(),
			Self::Relative { direction, id, .. } => direction.string().repeat(usize::try_from(u64::from(*id)).unwrap()),
		})
	}
}

impl PartialEq for Reference {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Self::Label(label) => match other {
				// Use a recursive lock to allow comparing a label to itself.
				Self::Label(other_label) => label.read_recursive().eq(&other_label.read_recursive()),
				_ => false,
			},
			Self::Relative { .. } |
			Self::UnresolvedLocalLabel { .. } |
			// Equality doesn't really make sense for dynamic user macro globals.
			Self::MacroGlobal { .. } => false,
			Self::MacroArgument { name, .. } => match other {
				Self::MacroArgument { name: other_name, .. } => name.eq(other_name),
				_ => false,
			},
		}
	}
}

impl PartialEq<Arc<RwLock<Label>>> for Reference {
	fn eq(&self, other: &Arc<RwLock<Label>>) -> bool {
		self == &Self::Label(other.clone())
	}
}

impl Resolvable for Reference {
	#[must_use]
	fn is_resolved(&self) -> bool {
		match self {
			Self::Label(label) => label.read().is_resolved(),
			Self::MacroArgument { value: Some(_), .. } | Self::Relative { value: Some(_), .. } => true,
			Self::MacroArgument { .. }
			| Self::Relative { .. }
			| Self::MacroGlobal { .. }
			| Self::UnresolvedLocalLabel { .. } => false,
		}
	}

	fn resolve_to(
		&mut self,
		location: MemoryAddress,
		usage_span: SourceSpan,
		source_code: Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Label(label) => label.write().resolve_to(location, usage_span, source_code),
			Self::MacroArgument { value, .. } | Self::Relative { value, .. } => {
				*value = Some(Box::new(AssemblyTimeValue::Literal(location)));
				Ok(())
			},
			Self::MacroGlobal { .. } | Self::UnresolvedLocalLabel { .. } =>
				unimplemented!("{:?} leaked to reference resolution, this is a sema bug", self),
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

/// A textual reference that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Clone, Debug)]
pub struct Label {
	/// User-given reference name.
	pub name:            String,
	/// Resolved memory location of the reference, if any.
	pub location:        Option<AssemblyTimeValue>,
	/// Source code location where this reference is defined.
	pub definition_span: Option<SourceSpan>,
	/// All source code locations where the label is used.
	pub usage_spans:     Vec<SourceSpan>,
	/// Whether this is a synthetic label. Synthetic labels are transparent to label hierarchy resolution.
	pub synthetic:       bool,
	/// Child labels belonging to this label.
	pub children:        BTreeMap<String, Arc<RwLock<Label>>>,
	/// Parent label of this label. If not set, this label is global.
	pub parent:          Weak<RwLock<Label>>,
}

impl Label {
	/// Returns the nesting count of this label, equivalent to the number of "." dots before the name specification in
	/// the assembler syntax. 0 means that the label is a global label.
	pub fn nesting_count(&self) -> usize {
		self.parent.upgrade().map_or(0, |parent| 1 + parent.read().nesting_count())
	}

	/// Returns whether the label is a global label.
	pub fn is_global(&self) -> bool {
		self.nesting_count() == 0
	}

	/// Returns whether the label is defined somewhere.
	/// Otherwise, the label was just used as part of an expression, such as a jump target or an address to load from.
	pub const fn has_definition(&self) -> bool {
		self.definition_span.is_some()
	}

	/// Returns this label's source span, which is usually the definition span. As a fallback, the first usage span is
	/// used.
	///
	/// # Errors
	/// If no source span exists at all, this label was created programmatically in an incorrect way and the program
	/// panics.
	pub fn source_span(&self) -> SourceSpan {
		self.definition_span.or_else(|| self.usage_spans.first().copied()).expect("label without any source spans")
	}

	/// Creates a new label with the given name and definition location.
	pub fn new_with_definition(name: String, span: SourceSpan) -> Arc<RwLock<Self>> {
		Arc::new(RwLock::new(Self {
			children: BTreeMap::default(),
			location: None,
			definition_span: Some(span),
			synthetic: false,
			name,
			usage_spans: Vec::default(),
			parent: Weak::default(),
		}))
	}

	/// Creates a new synthetic label with the given name and definition location.
	pub fn new_synthetic(name: String, span: SourceSpan) -> Arc<RwLock<Self>> {
		Arc::new(RwLock::new(Self {
			children: BTreeMap::default(),
			location: None,
			definition_span: Some(span),
			synthetic: true,
			name,
			usage_spans: Vec::default(),
			parent: Weak::default(),
		}))
	}

	/// Creates a new label with the given name and one usage location.
	pub fn new_with_use(name: String, span: SourceSpan) -> Arc<RwLock<Self>> {
		Arc::new(RwLock::new(Self {
			children: BTreeMap::default(),
			location: None,
			definition_span: None,
			synthetic: false,
			name,
			usage_spans: vec![span],
			parent: Weak::default(),
		}))
	}
}

impl ReferenceResolvable for Label {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RwLock<MacroParent>>,
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
		relative_labels: &HashMap<NonZeroU64, Arc<RwLock<Label>>>,
	) {
		if let Some(location) = self.location.as_mut() {
			location.resolve_relative_labels(direction, relative_labels);
		}
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		if let Some(location) = self.location.as_mut() {
			location.resolve_pseudo_labels(global_labels);
		}
	}

	fn set_current_label(
		&mut self,
		_current_label: &Option<Arc<RwLock<Label>>>,
		_source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		Ok(())
	}
}

impl PartialEq for Label {
	fn eq(&self, other: &Self) -> bool {
		self.name == other.name
	}
}
impl Eq for Label {}

impl Resolvable for Label {
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

impl Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.pad(&format!("{}{}", ".".repeat(self.nesting_count()), self.name))
	}
}

/// The structure holding all of the data for the macro arguments.
#[derive(Debug, Clone)]
pub struct MacroParent {
	label:          Arc<RwLock<Label>>,
	pub parameters: MacroParameters,
}

impl MacroParent {
	pub fn new_actual(parameters: HashMap<String, AssemblyTimeValue>, label: Arc<RwLock<Label>>) -> Arc<RwLock<Self>> {
		Arc::new(RwLock::new(Self { label, parameters: MacroParameters::Actual(parameters) }))
	}

	pub fn new_formal(parameters: Option<Vec<(String, SourceSpan)>>, span: SourceSpan) -> Arc<RwLock<Self>> {
		Arc::new(RwLock::new(Self {
			label:      Arc::new(RwLock::new(Label {
				name:            "macro global placeholder".into(),
				location:        None,
				synthetic:       true,
				definition_span: Some(span),
				usage_spans:     Vec::default(),
				children:        BTreeMap::new(),
				parent:          Weak::new(),
			})),
			parameters: MacroParameters::Formal(parameters.unwrap_or_default()),
		}))
	}

	pub fn global_label(&self) -> Arc<RwLock<Label>> {
		self.label.clone()
	}
}

impl Display for MacroParent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.parameters.fmt(f)
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

impl Display for MacroParameters {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.pad(&match self {
			Self::Formal(formal) =>
				formal.iter().map(|(parameter, _)| parameter).intersperse(&", ".to_owned().into()).collect::<String>(),
			Self::Actual(actual) => actual
				.iter()
				.map(|(parameter, value)| format!("{parameter} = {value:04X}"))
				.intersperse(", ".into())
				.collect::<String>(),
		})
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
		replacement_parent: Arc<RwLock<MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>>;

	/// Replace any relative label with the stand-in global label from the given list, if possible. Only operates in one
	/// direction, since the two directions are entirely distinct.
	fn resolve_relative_labels(
		&mut self,
		direction: RelativeReferenceDirection,
		relative_labels: &HashMap<NonZeroU64, Arc<RwLock<Label>>>,
	);

	/// Resolve all labels accessed with pseudo syntax, e.g. `global_local` for the hierarchy `global: .local: ...`.
	/// This is an Asar compatibility feature and its use is discouraged.
	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]);

	/// Resolve all pseudo-local labels into real labels by using the current label to figure out their parents.
	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>>;
}

/// Creates a local label at this position from the given data (name and nesting level), as well as the current label.
/// The new label is a child of that current label or one of its parents, and if such a label already exists, a new
/// reference is returned.
///
/// Note that the given current label may not end up being the label's parent, just one of its parents. This depends on
/// the hierarchical position of this label, indicated by `nesting_level`
pub fn create_local_at_this_position(
	local_name: String,
	local_nesting_level: NonZeroUsize,
	local_location: SourceSpan,
	local_value: Option<Box<AssemblyTimeValue>>,
	creation_kind: LabelUsageKind,
	current_label: Option<Arc<RwLock<Label>>>,
	source_code: &Arc<AssemblyCode>,
) -> Result<Arc<RwLock<Label>>, Box<AssemblyError>> {
	if let Some(actual_current_label) = current_label {
		let mut parent_label = actual_current_label;
		// Nesting level of the current label we're considering.
		let mut current_nesting_level = parent_label.read().nesting_count();
		// Move up the parent chain while the "parent" is still on our level (or below us)
		while current_nesting_level >= local_nesting_level.into() {
			let parent_borrow = parent_label.read();
			if let Some(new_parent) = parent_borrow.parent.upgrade() {
				let new_parent_clone = new_parent.clone();
				// Clarify for the borrow checker that we do not need the borrow into the parent anymore, since we
				// cloned the new parent.
				drop(parent_borrow);
				parent_label = new_parent_clone;
			} else {
				break;
			}
			current_nesting_level -= 1;
		}
		// We used "break" in the previous loop because the parent chain of the current label is broken.
		// This should not happen (given `nesting_count()`), but it's a safe error to return.
		if current_nesting_level >= local_nesting_level.into() {
			return Err(AssemblyError::MissingGlobalLabel {
				local_label: local_name,
				src:         source_code.clone(),
				location:    local_location,
			}
			.into());
		}

		let mut mutable_parent = parent_label.write();
		let local_label = mutable_parent
			.children
			.entry(local_name.clone())
			.or_insert_with(|| match creation_kind {
				LabelUsageKind::AsDefinition => Label::new_with_definition(local_name, local_location),
				LabelUsageKind::AsAddress => Label::new_with_use(local_name, local_location),
			})
			.clone();
		let mut mutable_label = local_label.write();
		mutable_label.parent = Arc::downgrade(&parent_label);
		if creation_kind == LabelUsageKind::AsAddress {
			mutable_label.usage_spans.push(local_location);
		}
		mutable_label.location = mutable_label.location.clone().or_else(|| local_value.map(|v| *v));
		drop(mutable_label);
		Ok(local_label)
	} else {
		Err(AssemblyError::MissingGlobalLabel {
			local_label: local_name,
			src:         source_code.clone(),
			location:    local_location,
		}
		.into())
	}
}
