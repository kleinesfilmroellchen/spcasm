//! Parser infrastructure; Utility functions for LALRPOP driver code.
use std::cell::RefCell;
use std::collections::HashMap;
use std::result::Result;
use std::sync::Arc;

use miette::SourceSpan;

use super::error::{AssemblyCode, AssemblyError};
use super::instruction::{AddressingMode, Instruction, Number, Opcode};
use super::label::{GlobalLabel, Label};
use super::ProgramElement;
use crate::mcro::MacroValue;
use crate::{label, lalrpop_adaptor, Macro, Token};

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
	fn parse(value: &str, location: SourceSpan, src: Arc<AssemblyCode>) -> Result<Self, AssemblyError>;

	/// Returns whether this string corresponds with an enum variant; i.e. parsing would succeed.
	fn is_valid(value: &str) -> bool;
}

/// Environment object for parsing. Holds the list of labels.
#[derive(Debug)]
pub struct Environment {
	/// The list of labels.
	pub labels:      Vec<Arc<RefCell<GlobalLabel>>>,
	/// The source code of the assembly code.
	pub source_code: Arc<AssemblyCode>,
}

impl Environment {
	/// Creates an empty environment.
	#[must_use]
	pub const fn new(source_code: Arc<AssemblyCode>) -> Self {
		Self { labels: Vec::new(), source_code }
	}

	/// Parse a program given a set of tokens straight from the lexer.
	/// The parser makes sure that all pre-processing of the token stream and the final reference resolutions are
	/// performed.
	/// # Errors
	/// Whenever something goes wrong in parsing.
	pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<ProgramElement>, AssemblyError> {
		let lexed = lalrpop_adaptor::disambiguate_indexing_parenthesis(tokens);
		let lexed = lalrpop_adaptor::LalrpopAdaptor::from(lexed);
		let mut program = crate::asm::ProgramParser::new()
			.parse(self, lexed)
			.map_err(|err| AssemblyError::from_lalrpop(err, self.source_code.clone()))?;
		self.fill_in_label_references(&mut program)?;
		Ok(program)
	}

	/// Fills in the global label references for all local labels. Existing ones are overwritten, so the labels are
	/// always consistent.
	/// # Errors
	/// If a local label precedes any global labels.
	/// # Panics
	/// All panics are programming errors.
	pub fn fill_in_label_references(&self, program: &mut Vec<ProgramElement>) -> Result<(), AssemblyError> {
		let mut current_global_label: Option<Arc<RefCell<GlobalLabel>>> = None;

		for element in program {
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
				ProgramElement::Instruction(Instruction { label: Some(Label::Global(ref global)), .. }) =>
					current_global_label = Some(global.clone()),
				ProgramElement::Macro(Macro {
					value: MacroValue::AssignLabel { label: Label::Local(ref mut local), .. },
					..
				})
				| ProgramElement::Instruction(Instruction { label: Some(Label::Local(ref mut local)), .. }) =>
					*local =
						label::merge_local_into_parent(local.clone(), current_global_label.clone(), &self.source_code)?,

				ProgramElement::Instruction(Instruction { label: None, .. })
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
