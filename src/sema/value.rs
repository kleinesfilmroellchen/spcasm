//! Assembly-time values.

use core::fmt;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::fmt::{Display, Error, Formatter, UpperHex, Write};
use std::num::NonZeroU64;
use std::result::Result;
use std::sync::Arc;

use miette::SourceSpan;
use num_derive::{FromPrimitive, ToPrimitive};
#[allow(unused)]
use smartstring::alias::String;

use super::instruction::MemoryAddress;
use super::reference::{self, Label, ReferenceResolvable, Reference, RelativeReferenceDirection};
use crate::error::AssemblyError;
use crate::AssemblyCode;

/// Any numeric value that can be calculated at assembly time.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum AssemblyTimeValue {
	/// A literal.
	Literal(MemoryAddress),
	/// A reference that will resolve later.
	Reference(Reference),
	/// A unary math operation.
	UnaryOperation(Box<AssemblyTimeValue>, UnaryOperator),
	/// A binary math operation.
	BinaryOperation(Box<AssemblyTimeValue>, Box<AssemblyTimeValue>, BinaryOperator),
}

impl AssemblyTimeValue {
	/// Return the first reference that can be found in this expression.
	#[must_use]
	pub fn first_reference(&self) -> Option<Reference> {
		match self {
			Self::Literal(..) => None,
			Self::Reference(reference) => Some(reference.clone()),
			Self::UnaryOperation(number, _) => number.first_reference(),
			Self::BinaryOperation(lhs, rhs, _) => lhs.first_reference().or_else(|| rhs.first_reference()),
		}
	}

	/// Returns all references in this expression.
	#[must_use]
	pub fn references(&self) -> Vec<&Reference> {
		match self {
			Self::Literal(..) => Vec::default(),
			Self::Reference(reference) => vec![reference],
			Self::UnaryOperation(value, _) => value.references(),
			Self::BinaryOperation(lhs, rhs, _) => {
				let mut references = lhs.references();
				let mut more_references = rhs.references();
				references.append(&mut more_references);
				references
			},
		}
	}

	/// Extracts the concrete value, if possible.
	/// # Errors
	/// If the value cannot be resolved.
	pub fn try_value(
		&self,
		location: SourceSpan,
		source_code: Arc<AssemblyCode>,
	) -> Result<MemoryAddress, Box<AssemblyError>> {
		let possibly_resolved = self.clone().try_resolve();
		if let Self::Literal(value) = possibly_resolved {
			Ok(value)
		} else {
			let first_reference = self.first_reference().expect("unresolved value without a reference");
			Err(AssemblyError::UnresolvedReference {
				reference:          first_reference.to_string().into(),
				reference_location: Some(first_reference.source_span()),
				usage_location:     location,
				src:                source_code,
			}
			.into())
		}
	}

	/// Try to resolve this value down to a literal. Even if that's not entirely possible, sub-expressions are
	/// collapsed and resolved as far as possible.
	#[must_use]
	pub fn try_resolve(self) -> Self {
		self.try_resolve_impl(Vec::new())
	}

	/// The implementation for ``try_resolve``. This function keeps track of which references have been tried to be
	/// resolved already to prevent infinite recursion.
	#[must_use]
	pub fn try_resolve_impl(self, resolution_attempts: Vec<&Reference>) -> Self {
		match self {
			Self::Reference(ref reference @ Reference::Label(ref global)) if let Some(memory_location) = global.clone().read().location.clone() => {
				// Recursive reference definition, we need to abort.
				if resolution_attempts.contains(&reference) {
					self
				} else {
					let mut attempts_with_this = resolution_attempts;
					attempts_with_this.push(reference);
					memory_location.try_resolve_impl(attempts_with_this)
				}
			},
			Self::Reference(ref reference @ (Reference::MacroArgument { value: Some(ref memory_location) , .. } | Reference::Relative { value: Some(ref memory_location) , .. })) => {
				if resolution_attempts.contains(&reference) {
					self
				} else {
					let mut attempts_with_this = resolution_attempts;
					attempts_with_this.push(reference);
					memory_location.clone().try_resolve_impl(attempts_with_this)
				}
			},
			Self::UnaryOperation(number, operator) => match number.try_resolve_impl(resolution_attempts) {
				Self::Literal(value) => Self::Literal(operator.execute(value)),
				resolved => Self::UnaryOperation(Box::new(resolved), operator),
			},
			Self::BinaryOperation(lhs, rhs, operator) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Self::Literal(lhs_value), Self::Literal(rhs_value)) => Self::Literal(operator.execute(lhs_value, rhs_value)),
				(lhs, rhs) => Self::BinaryOperation(Box::new(lhs), Box::new(rhs), operator),
			},
			Self::Literal(..) 
			| Self::Reference(
				| Reference::Label(..) 
				| Reference::MacroArgument { value: None, .. } 
				| Reference::Relative { value: None, .. } 
				| Reference::MacroGlobal { .. }
				| Reference::UnresolvedLocalLabel { .. }
			) => self,
		}
	}

	/// Resolve this value while using a provided resolver function to obtain preliminary values for unresolved
	/// references. If the resolver function can't do that, then we cannot determine a value either.
	pub fn value_using_resolver(
		&self,
		resolver: &impl Fn(Reference) -> Option<MemoryAddress>,
	) -> Option<MemoryAddress> {
		// TODO: figure out how to share this code with try_value (function APIs and assumptions are currently
		// fundamentally incompatible)
		match self {
			Self::Literal(value) => Some(*value),
			Self::Reference(ref reference) => match reference {
				Reference::Label(label) if let Some(ref value) = label.read().location => value.value_using_resolver(resolver),
				Reference::MacroArgument { value: Some(value), .. }
				| Reference::Relative { value: Some(value), .. } => value.value_using_resolver(resolver),
				| Reference::Label(_)
				| Reference::MacroGlobal { .. }
				| Reference::Relative { value: None, .. }
				| Reference::UnresolvedLocalLabel { .. }
				| Reference::MacroArgument { value: None, .. } => resolver(reference.clone()),
			},
			Self::UnaryOperation(number, operator) => number.value_using_resolver(resolver).map(|value| operator.execute(value)),
			Self::BinaryOperation(lhs, rhs, operator) => lhs.value_using_resolver(resolver).and_then(|lhs|rhs.value_using_resolver(resolver).map(|rhs| operator.execute(lhs, rhs))),
		}
	}

	/// Returns whether this value is resolved.
	#[must_use]
	pub fn is_resolved(&self) -> bool {
		matches!(self.clone().try_resolve(), Self::Literal(_))
	}
}

impl ReferenceResolvable for AssemblyTimeValue {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RwLock<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Literal(_) => Ok(()),
			Self::Reference(reference @ Reference::MacroGlobal { .. }) => {
				let new_global = replacement_parent.read().global_label();
				*reference = Reference::Label(new_global);
				Ok(())
			},
			Self::Reference(reference) => reference.replace_macro_parent(replacement_parent, source_code),
			Self::UnaryOperation(number, _) => number.replace_macro_parent(replacement_parent, source_code),
			Self::BinaryOperation(lhs, rhs, _) => {
				lhs.replace_macro_parent(replacement_parent.clone(), source_code)?;
				rhs.replace_macro_parent(replacement_parent, source_code)
			},
		}
	}

	fn resolve_relative_labels(&mut self, direction: RelativeReferenceDirection, relative_labels: &HashMap<NonZeroU64, Arc<RwLock<Label>>>) {
		match self {
			// Awkward match since the borrow checker is not smart enough for this.
			Self::Reference(reference @ Reference::Relative { .. }) => {
				let (id, own_direction) = match reference {
					Reference::Relative { id, direction,.. } => (*id, *direction),
					_ => unreachable!(),
				};
				if let Some(new_reference) = relative_labels.get(&id).cloned() && own_direction == direction {
					*reference = Reference::Label(new_reference);
				}
			},
			Self::Literal(_) => (),
			Self::Reference(reference) => reference.resolve_relative_labels(direction, relative_labels),
			Self::UnaryOperation(number, _) => number.resolve_relative_labels(direction, relative_labels),
			Self::BinaryOperation(lhs, rhs, _) => {
				lhs.resolve_relative_labels(direction, relative_labels);
				rhs.resolve_relative_labels(direction, relative_labels);
			},
		}
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		match self {
			Self::Literal(_) => (),
			Self::Reference(reference) => reference.resolve_pseudo_labels(global_labels),
			Self::UnaryOperation(number, _) => number.resolve_pseudo_labels(global_labels),
			Self::BinaryOperation(lhs, rhs, _) => {
				lhs.resolve_pseudo_labels(global_labels);
				rhs.resolve_pseudo_labels(global_labels);
			},
		}
	}
	
	/// Sets the given global label as the parent for all unresolved local labels.
	/// # Panics
	/// All panics are programming errors.
	fn set_current_label(&mut self, label: &Option<Arc<RwLock<Label>>>, source_code: &Arc<AssemblyCode>) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Reference(reference) => reference.set_current_label(label, source_code),
			Self::UnaryOperation(val, _) => val.set_current_label(label, source_code),
			Self::BinaryOperation(lhs, rhs, _) => {
				lhs.set_current_label(label, source_code).and_then(|_|
				rhs.set_current_label(label, source_code))
			},
			Self::Literal(..) => Ok(()),
		}
	}
}

impl From<MemoryAddress> for AssemblyTimeValue {
	fn from(address: MemoryAddress) -> Self {
		Self::Literal(address)
	}
}

impl<T> From<(Self, T)> for AssemblyTimeValue {
	fn from(value: (Self, T)) -> Self {
		value.0
	}
}

// should be closure in upperhex formatter but borrow checker says no, again, no passing references to other closures
fn write_correctly(prefix: &str, f: &mut Formatter<'_>, address: &AssemblyTimeValue) -> Result<(), Error> {
	f.write_str(prefix)?;
	fmt::UpperHex::fmt(address, f)
}

impl UpperHex for AssemblyTimeValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		let write_binary = |op, f: &mut Formatter, lhs: &Self, rhs: &Self| {
			write_correctly("(", f, lhs)?;
			write_correctly(op, f, rhs)?;
			f.write_char(')')
		};

		match self {
			Self::Reference(reference) => match reference.location() {
				Some(ref numeric_address) => fmt::UpperHex::fmt(numeric_address, f),
				None => write!(f, "{}", reference),
			},
			Self::Literal(numeric_address) => {
				f.write_char('$')?;
				fmt::UpperHex::fmt(numeric_address, f)
			},
			Self::UnaryOperation(number, operator) => write_correctly(&operator.to_string(), f, number.as_ref()),
			Self::BinaryOperation(lhs, rhs, operator) => write_binary(&operator.to_string(), f, lhs, rhs),
		}
	}
}

impl std::fmt::Display for AssemblyTimeValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{:X}", self)
	}
}

/// Size of a value in memory. SPC-700 assembly uses 65c816 naming:
/// - byte: 1 byte
/// - word: 2 bytes
/// - long: 3 bytes (a 65c816 address)
/// - doubleword/dword: 4 bytes
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, FromPrimitive, ToPrimitive)]
#[repr(u8)]
pub enum Size {
	/// 1 byte
	Byte = 1,
	/// 2 bytes
	Word = 2,
	/// 3 bytes
	Long = 3,
	/// 4 bytes
	DWord = 4,
}

/// An assembly-time value that also carries a size.
/// The size determines what truncation happens before the value is written out.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub struct SizedAssemblyTimeValue {
	/// The value itself.
	pub value: AssemblyTimeValue,
	/// The size of the value in memory.
	pub size:  Size,
}

impl Default for SizedAssemblyTimeValue {
	fn default() -> Self {
		Self { value: AssemblyTimeValue::Literal(0), size: Size::Byte }
	}
}

impl ReferenceResolvable for SizedAssemblyTimeValue {
	fn replace_macro_parent(
			&mut self,
			replacement_parent: Arc<RwLock<reference::MacroParent>>,
			source_code: &Arc<AssemblyCode>,
		) -> Result<(), Box<AssemblyError>> {
		self.value.replace_macro_parent(replacement_parent, source_code)
	}

	fn resolve_relative_labels(
			&mut self,
			direction: RelativeReferenceDirection,
			relative_labels: &HashMap<NonZeroU64, Arc<RwLock<Label>>>,
		) {
		self.value.resolve_relative_labels(direction, relative_labels);
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		self.value.resolve_pseudo_labels(global_labels);
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.value.set_current_label(current_label, source_code)
	}
	
}

/// Unary operators for assembly time calculations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
	/// -expr
	Negate,
	/// ~expr
	Not,
}

impl UnaryOperator {
	/// Run the math operation this operator represents.
	#[must_use]
	pub const fn execute(&self, value: MemoryAddress) -> MemoryAddress {
		match self {
			Self::Not => !value,
			Self::Negate => -value,
		}
	}
}

impl Display for UnaryOperator {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::Negate => '-',
			Self::Not => '~',
		})
	}
}

/// The kinds of binary operators supported for assembly-time calculations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
	/// expr + expr
	Add,
	/// expr - expr
	Subtract,
	/// expr * expr
	Multiply,
	/// expr / expr
	Divide,
	/// expr % expr
	Modulus,
	/// expr << expr
	LeftShift,
	/// expr >> expr
	RightShift,
	/// expr & expr
	And,
	/// expr | expr
	Or,
	/// expr ^ expr
	Xor,
	/// expr ** expr
	Exponentiation,
}

impl BinaryOperator {
	/// Run the math operation this binary operator represents.
	#[must_use]
	pub const fn execute(&self, lhs: MemoryAddress, rhs: MemoryAddress) -> MemoryAddress {
		match self {
			Self::Add => lhs + rhs,
			Self::Subtract => lhs - rhs,
			Self::Multiply => lhs * rhs,
			Self::Divide => lhs / rhs,
			Self::Modulus => lhs % rhs,
			Self::LeftShift => lhs << rhs,
			Self::RightShift => lhs >> rhs,
			Self::And => lhs & rhs,
			Self::Or => lhs | rhs,
			Self::Xor => lhs ^ rhs,
			#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
			Self::Exponentiation => lhs.pow(rhs as u32),
		}
	}
}

impl Display for BinaryOperator {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::Add => "+",
			Self::Subtract => "-",
			Self::Multiply => "*",
			Self::Divide => "/",
			Self::Modulus => "%",
			Self::LeftShift => "<<",
			Self::RightShift => ">>",
			Self::And => "&",
			Self::Or => "|",
			Self::Xor => "^",
			Self::Exponentiation => "**",
		})
	}
}
