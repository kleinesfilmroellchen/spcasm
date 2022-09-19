//! Parser infrastructure; Utility functions for LALRPOP driver code.
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::result::Result;
use std::sync::{Arc, Weak};

use miette::{SourceOffset, SourceSpan};

use self::instruction::{AddressingMode, Instruction, Number, Opcode};
use self::label::{GlobalLabel, Label};
use self::lexer::lex;
use crate::assembler::resolve_file;
use crate::error::{AssemblyCode, AssemblyError};
use crate::mcro::MacroValue;
use crate::{lalrpop_adaptor, Macro};

pub mod instruction;
pub(crate) mod label;
pub mod lexer;
pub(crate) mod program;
pub(crate) mod register;
pub mod token;

pub use program::ProgramElement;
pub use register::Register;
pub use token::Token;

/// How a looked-up label is used. See ``Environment::get_global_label``.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum LabelUsageKind {
	/// Label is used as a parameter, i.e. it's address is of interest.
	AsAddress,
	/// Label is being defined.
	AsDefinition,
}

/// Anything that can be primitively parsed from a string into an enum variant.
/// This trait is intended to be derived with the macro from ``spcasm_derive``.
pub trait Parse
where
	Self: Sized,
{
	/// Parse this enum from the string representation.
	/// # Errors
	/// If the string doesn't correspond with any enum variant.
	fn parse(value: &str, location: SourceSpan, src: Arc<AssemblyCode>) -> Result<Self, Box<AssemblyError>>;

	/// Returns whether this string corresponds with an enum variant; i.e. parsing would succeed.
	fn is_valid(value: &str) -> bool;
}

/// Environment object for parsing. Holds the list of labels.
#[derive(Debug)]
pub struct Environment {
	/// The list of labels.
	pub labels:       Vec<Arc<RefCell<GlobalLabel>>>,
	/// The files included in this "tree" created by include statements.
	pub(crate) files: HashMap<PathBuf, Arc<RefCell<AssemblyFile>>>,
}

#[derive(Debug)]
pub(crate) struct AssemblyFile {
	/// Parsed contents.
	pub content:     Vec<ProgramElement>,
	/// Underlying source code and file name.
	pub source_code: Arc<AssemblyCode>,
	/// The environment that this file is parsed in.
	pub parent:      Weak<RefCell<Environment>>,
}

impl Environment {
	/// Creates an empty environment.
	#[must_use]
	pub fn new() -> Arc<RefCell<Self>> {
		Arc::new(RefCell::new(Self { labels: Vec::new(), files: HashMap::new() }))
	}

	/// Searches for an existing parsed file in this environment given that file's source code.
	/// Note that the source code does not have to be the identical object in memory, it just has to compare equal.
	/// See ``AssemblyCode::eq`` for the equality semantics of the source code objects.
	pub(crate) fn find_file_by_source(
		&self,
		source_code: &Arc<AssemblyCode>,
	) -> Result<Option<Arc<RefCell<AssemblyFile>>>, Box<AssemblyError>> {
		self.files
			.get(&source_code.name)
			// Keep around a tuple with the original Arc so we can return it at the end.
			.map(|file| file.try_borrow().map(|maybe_file| (file, maybe_file)))
			.transpose()
			.map(|maybe_file| {
				maybe_file.filter(|(_, file)| *file.source_code == **source_code).map(|(file, _)| file.clone())
			})
			.map_err(|_| AssemblyError::IncludeCycle {
				cycle_trigger_file: source_code.file_name(),
				src:                source_code.clone(),
				include:            (0, 0).into(),
			}.into())
	}

	/// Parse a program given a set of tokens straight from the lexer.
	/// The parser makes sure that all pre-processing of the token stream and the final reference resolutions are
	/// performed.
	///
	/// In terms of multi-file behavior, the resulting file is added to this environment's list of source files. If the
	/// source code already was parsed in this environment, that parsed data is returned instead. If, however, the
	/// source code was parsed but didn't have its includes fully resolved yet, that constitutes an include cycle and an
	/// error is returned.
	///
	/// # Errors
	/// Whenever something goes wrong in parsing.
	pub(crate) fn parse(
		this: &Arc<RefCell<Self>>,
		tokens: Vec<Token>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<Arc<RefCell<AssemblyFile>>, Box<AssemblyError>> {
		if let Some(already_parsed_file) = this.borrow().find_file_by_source(source_code)? {
			// If we're in a cycle, the already parsed file still has unresolved labels.
			// I'm not sure whether this can happen in the first place given that find_file_by_source can't borrow such
			// a file and therefore won't return it, but let's better be safe than sorry.
			return if already_parsed_file.try_borrow().is_ok_and(|file| file.has_unresolved_source_includes()) {
				Ok(already_parsed_file)
			} else {
				Err(AssemblyError::IncludeCycle {
					cycle_trigger_file: source_code.file_name(),
					src:                source_code.clone(),
					include:            (0, 0).into(),
				}
				.into())
			};
		}

		let lexed = lalrpop_adaptor::disambiguate_indexing_parenthesis(tokens);
		let lexed = lalrpop_adaptor::LalrpopAdaptor::from(lexed);
		let mut program = crate::asm::ProgramParser::new()
			.parse(this, source_code, lexed)
			.map_err(|err| AssemblyError::from_lalrpop(err, source_code.clone()))?;

		let mut rc_file = Arc::new(RefCell::new(AssemblyFile {
			content:     program,
			source_code: source_code.clone(),
			parent:      Arc::downgrade(this),
		}));
		let mut file = rc_file.borrow_mut();

		file.fill_in_label_references()?;
		file.coerce_to_direct_page_addressing();

		drop(file);
		// Insert the file into the list of source files so that we can detect cycles...
		this.borrow_mut().files.insert(source_code.name.clone(), rc_file.clone());

		// ...once we start including source files here.
		rc_file.borrow_mut().resolve_source_includes()?;

		Ok(rc_file)
	}

	/// Lookup a global label in this environment, and create it if necessary.
	pub fn get_global_label(
		&mut self,
		name: &'_ str,
		span: SourceSpan,
		usage_kind: LabelUsageKind,
	) -> Arc<RefCell<GlobalLabel>> {
		if let Some(matching_label) = self.labels.iter_mut().find(|label| label.borrow().name == name) {
			let mut mutable_matching_label = matching_label.borrow_mut();
			if usage_kind == LabelUsageKind::AsAddress && !mutable_matching_label.used_as_address {
				mutable_matching_label.used_as_address = true;
			}
			// If the caller flags this use of the label as its definition, we override the label's position with what
			// we were just given.
			if usage_kind == LabelUsageKind::AsDefinition {
				mutable_matching_label.span = span;
			}
			matching_label.clone()
		} else {
			let new_label = Arc::new(RefCell::new(GlobalLabel {
				name: name.to_owned(),
				location: None,
				span,
				used_as_address: usage_kind == LabelUsageKind::AsAddress,
				locals: HashMap::new(),
			}));
			self.labels.push(new_label.clone());
			new_label
		}
	}
}

impl AssemblyFile {
	/// Fills in the global label references for all local labels. Existing ones are overwritten, so the labels are
	/// always consistent.
	/// # Errors
	/// If a local label precedes any global labels.
	/// # Panics
	/// All panics are programming errors.
	pub fn fill_in_label_references(&mut self) -> Result<(), Box<AssemblyError>> {
		let mut current_global_label: Option<Arc<RefCell<GlobalLabel>>> = None;

		for element in &mut self.content {
			// First match for label reference resolution in instruction position
			match element {
				ProgramElement::Macro(Macro { value, label: Some(Label::Local(ref mut local)), .. }) => {
					if let MacroValue::AssignLabel { label: Label::Local(assigned_local), .. } = value {
						*assigned_local = label::merge_local_into_parent(
							assigned_local.clone(),
							current_global_label.clone(),
							&self.source_code,
						)?;
					}
					*local =
						label::merge_local_into_parent(local.clone(), current_global_label.clone(), &self.source_code)?;
				},
				ProgramElement::Macro(Macro { label: Some(Label::Global(ref global)), value, .. }) => {
					current_global_label = Some(global.clone());
					if let MacroValue::AssignLabel { label: Label::Local(local), .. } = value {
						*local = label::merge_local_into_parent(
							local.clone(),
							current_global_label.clone(),
							&self.source_code,
						)?;
					}
				},
				ProgramElement::Instruction(Instruction { label: Some(Label::Global(ref global)), .. })
				| ProgramElement::IncludeSource { label: Some(Label::Global(ref global)), .. } =>
					current_global_label = Some(global.clone()),
				ProgramElement::Macro(Macro {
					value: MacroValue::AssignLabel { label: Label::Local(ref mut local), .. },
					..
				})
				| ProgramElement::Instruction(Instruction { label: Some(Label::Local(ref mut local)), .. })
				| ProgramElement::IncludeSource { label: Some(Label::Local(ref mut local)), .. } =>
					*local =
						label::merge_local_into_parent(local.clone(), current_global_label.clone(), &self.source_code)?,

				ProgramElement::Instruction(Instruction { label: None, .. })
				| ProgramElement::IncludeSource { label: None, .. }
				| ProgramElement::Macro(Macro { label: None, .. }) => (),
			}
			if let ProgramElement::Instruction(Instruction {
				opcode: Opcode { first_operand, second_operand, .. },
				..
			}) = element && let Some(ref actual_global_label) = current_global_label
			{
				if let Some(mode) = first_operand.as_mut() { mode.set_global_label(actual_global_label) }
				if let Some(mode) = second_operand.as_mut() { mode.set_global_label(actual_global_label) }
			}
		}
		Ok(())
	}

	/// Tries to coerce addressing modes to direct page addressing wherever possible. This needs to be done again as the
	/// unresolved local labels did not provide memory locations before merging.
	pub fn coerce_to_direct_page_addressing(&mut self) {
		for element in &mut self.content {
			if let ProgramElement::Instruction(Instruction {
				opcode: Opcode { first_operand, second_operand, .. },
				..
			}) = element
			{
				*first_operand = first_operand.clone().map(AddressingMode::coerce_to_direct_page_addressing);
				*second_operand = second_operand.clone().map(AddressingMode::coerce_to_direct_page_addressing);
			}
		}
	}

	/// Sets the first label in this file if a label was given.
	pub fn set_first_label(&mut self, label: Option<Label>) {
		if let Some(first) = self.content.get_mut(0) {
			*first = first.clone().set_label(label);
		}
	}

	/// Returns whether this file's parsed content contains any unresolved include directives.
	pub(crate) fn has_unresolved_source_includes(&self) -> bool {
		self.content.iter().any(|element| matches!(element, ProgramElement::IncludeSource { .. }))
	}

	/// Resolves all source include directives by recursively calling into lexer and parser.
	///
	/// # Errors
	/// All errors from other files are propagated, as well as include cycles.
	pub fn resolve_source_includes(&mut self) -> Result<(), Box<AssemblyError>> {
		let mut index = 0;
		while index < self.content.len() {
			let mut element = self.content[index].clone();
			if let ProgramElement::IncludeSource { ref file, label, span } = element {
				let environment = self.parent.upgrade().expect("parent deleted while we're still parsing");
				let file = resolve_file(&self.source_code, span, file)?.to_string_lossy().to_string();
				let mut included_code =
					AssemblyCode::from_file(&file).map_err(|os_error| AssemblyError::FileNotFound {
						os_error,
						file_name: file,
						src: self.source_code.clone(),
						location: span,
					})?;
				let mut child_include_path = &mut unsafe { Arc::get_mut_unchecked(&mut included_code) }.include_path;
				child_include_path.push(self.source_code.name.clone());
				child_include_path.append(&mut self.source_code.include_path.clone());

				let tokens = lex(included_code.clone())?;
				let mut included_file = Environment::parse(&environment, tokens, &included_code)?;

				included_file.borrow_mut().set_first_label(label);
				self.content.splice(index ..= index, included_file.borrow().content.clone());
				continue;
			}
			index += 1;
		}
		Ok(())
	}
}

/// Creates the direct page addressing mode if the number is a legal direct page address.
/// # Panics
/// To-do: Handle errors properly in the given functions!
pub fn try_make_direct_page_addressing_mode<T>(
	value: T,
	dp_mode: impl FnOnce(T) -> AddressingMode,
	non_dp_mode: impl FnOnce(T) -> AddressingMode,
) -> AddressingMode
where
	T: Into<Number> + Clone,
{
	let number: Number = value.clone().into().try_resolve();
	match number {
		Number::Literal(literal) if literal <= 0xFF => dp_mode(value),
		_ => non_dp_mode(value),
	}
}

/// A simple union type for source spans and (zero-width) source offsets.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum SpanOrOffset {
	///
	Span(SourceSpan),
	///
	Offset(SourceOffset),
}

impl Default for SpanOrOffset {
	fn default() -> Self {
		Self::Offset(0.into())
	}
}

impl From<SourceOffset> for SpanOrOffset {
	fn from(offset: SourceOffset) -> Self {
		Self::Offset(offset)
	}
}

impl From<SourceSpan> for SpanOrOffset {
	fn from(span: SourceSpan) -> Self {
		Self::Span(span)
	}
}

impl From<&SourceSpan> for SpanOrOffset {
	fn from(span: &SourceSpan) -> Self {
		Self::Span(*span)
	}
}

#[allow(clippy::from_over_into)]
impl Into<SourceSpan> for SpanOrOffset {
	fn into(self) -> SourceSpan {
		match self {
			Self::Span(span) => span,
			Self::Offset(offset) => (offset, 0.into()).into(),
		}
	}
}

/// Creates a new source span from the given start and end source spans. This is used for constructing larger syntactic
/// elements that span multiple tokens or sub-elements.
#[must_use]
pub fn source_range(start: SpanOrOffset, end: SpanOrOffset) -> SourceSpan {
	let start: SourceSpan = start.into();
	let end: SourceSpan = end.into();
	(start.offset(), end.offset() + end.len() - start.offset()).into()
}
