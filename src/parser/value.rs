//! Assembly-time values.

use core::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Error, Formatter, UpperHex, Write};
use std::path::PathBuf;
use std::result::Result;
use std::sync::{Arc, Weak};

use miette::{SourceOffset, SourceSpan};
use spcasm_derive::{Parse, VariantName};

use super::instruction::{Instruction, MemoryAddress, Opcode};
use super::lexer::lex;
use super::reference::{self, GlobalLabel, MacroParameters, MacroParentReplacable, Reference};
use super::register::Register;
use crate::assembler::resolve_file;
use crate::cli::{default_backend_options, BackendOptions};
use crate::directive::DirectiveValue;
use crate::error::{AssemblyCode, AssemblyError};
use crate::{lalrpop_adaptor, Directive, VariantName};

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

	/// Sets the given global label as the parent for all unresolved local labels.
	/// # Panics
	/// All panics are programming errors.
	pub fn set_global_label(&mut self, label: &Arc<RefCell<GlobalLabel>>) {
		match self {
			Self::Reference(Reference::Local(local)) =>
				*local =
					reference::merge_local_into_parent(local.clone(), Some(label.clone()), &Arc::default()).unwrap(),
			Self::UnaryOperation(val, _) => val.set_global_label(label),
			Self::BinaryOperation(lhs, rhs, _) => {
				lhs.set_global_label(label);
				rhs.set_global_label(label);
			},
			Self::Literal(..)
			| Self::Reference(
				Reference::Global(..) | Reference::MacroArgument { .. } | Reference::MacroGlobal { .. },
			) => (),
		}
	}

	/// Extracts the concrete value.
	/// # Panics
	/// If the reference was not yet resolved, this function panics as it assumes that that has already happened.
	#[must_use]
	pub fn value(&self) -> MemoryAddress {
		self.try_value((0, 0).into(), Arc::default()).unwrap()
	}

	/// Extracts the concrete value, if possible.
	/// # Errors
	/// If the value cannot be resolved.
	pub fn try_value(
		&self,
		location: SourceSpan,
		source_code: Arc<AssemblyCode>,
	) -> Result<MemoryAddress, Box<AssemblyError>> {
		Ok(match self {
			Self::Literal(value) => *value,
			// necessary because matching through an Rc is not possible right now (would be super dope though).
			Self::Reference(ref reference) => match reference {
				Reference::Global(global_label) if let Some(ref value) = global_label.borrow().location => value.value(),
				Reference::Local(local) if let Some(ref value) = local.borrow().location => value.value(),
				Reference::Local(local) => return Err(AssemblyError::UnresolvedReference {
					reference: reference.to_string(),
					reference_location: Some(local.borrow().span),
					usage_location: location,
					src: source_code
				}.into()),
				Reference::Global(global) => return Err(AssemblyError::UnresolvedReference {
					reference: reference.to_string(),
					reference_location: Some(global.borrow().span),
					usage_location: location,
					src: source_code
				}.into()),
				Reference::MacroGlobal { span, .. } |
				Reference::MacroArgument{value: None, span, ..} => return Err(AssemblyError::UnresolvedReference {
					reference: reference.to_string(),
					reference_location: None,
					usage_location: *span,
					src: source_code
				}.into()),
				Reference::MacroArgument{value: Some(value), span, ..} => value.try_value(location, source_code)?,
			},
			Self::UnaryOperation(number, operator) => operator.execute(number.try_value(location, source_code)?),
			Self::BinaryOperation(lhs, rhs, operator) => operator.execute(lhs.try_value(location, source_code.clone())?, rhs.try_value(location, source_code)?),
		})
	}

	/// Try to resolve this value down to a literal. Even if that's not entirely possible, sub-expressions are
	/// collapsed and resolved as far as possible.
	#[must_use]
	pub fn try_resolve(self) -> Self {
		match self {
			Self::Reference(Reference::Global(global)) if let Some(memory_location) = global.clone().borrow().location.clone() => memory_location.try_resolve(),
			Self::Reference(Reference::Local(local)) if let Some(memory_location) = local.clone().borrow().location.clone() => memory_location.try_resolve(),
			Self::Reference(Reference::MacroArgument { value: Some(memory_location) , .. }) => memory_location.try_resolve(),
			Self::UnaryOperation(number, operator) => match number.try_resolve() {
				Self::Literal(value) => Self::Literal(operator.execute(value)),
				resolved => Self::UnaryOperation(Box::new(resolved), operator),
			},
			Self::BinaryOperation(lhs, rhs, operator) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Self::Literal(lhs_value), Self::Literal(rhs_value)) => Self::Literal(operator.execute(lhs_value, rhs_value)),
				(lhs, rhs) => Self::BinaryOperation(Box::new(lhs), Box::new(rhs), operator),
			},
			Self::Literal(..) | Self::Reference(Reference::Local(..) | Reference::Global(..) | Reference::MacroArgument { value: None, .. } | Reference::MacroGlobal { .. }) => self,
		}
	}
}

impl MacroParentReplacable for AssemblyTimeValue {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Literal(_) => Ok(()),
			Self::Reference(reference @ Reference::MacroGlobal { .. }) => {
				let new_global = replacement_parent.borrow().global_label();
				*reference = Reference::Global(new_global);
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
			Self::Reference(Reference::Global(ref unresolved_reference)) => {
				let unresolved_reference = unresolved_reference.borrow();
				match unresolved_reference.location {
					Some(ref numeric_address) => write_correctly("$", f, numeric_address),
					None => write!(f, "{}", unresolved_reference.name),
				}
			},
			Self::Reference(Reference::Local(ref local)) => {
				let local = local.borrow();
				match &local.location {
					Some(numeric_address) => f.pad(&format!("{:0X}", **numeric_address)),
					None => write!(f, "{}", local.name),
				}
			},
			Self::Reference(Reference::MacroArgument { value, name, .. }) => match value {
				Some(numeric_address) => f.pad(&format!("{:0X}", **numeric_address)),
				None => write!(f, "{}", name),
			},
			Self::Reference(Reference::MacroGlobal { .. }) => f.write_str("\\@"),
			Self::Literal(numeric_address) => {
				f.write_char('$')?;
				fmt::UpperHex::fmt(numeric_address, f)
			},
			Self::UnaryOperation(number, operator) => write_correctly(&operator.to_string(), f, number.as_ref()),
			Self::BinaryOperation(lhs, rhs, operator) => write_binary(&operator.to_string(), f, lhs, rhs),
		}
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
	pub fn execute(&self, value: MemoryAddress) -> MemoryAddress {
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
	pub fn execute(&self, lhs: MemoryAddress, rhs: MemoryAddress) -> MemoryAddress {
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
