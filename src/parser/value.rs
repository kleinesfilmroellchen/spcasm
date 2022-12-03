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
pub enum AssemblyTimeValue {
	/// A literal.
	Literal(MemoryAddress),
	/// A reference that will resolve later.
	Reference(Reference),
	/// - expr; negating another value.
	Negate(Box<AssemblyTimeValue>),
	/// expr + expr
	Add(Box<AssemblyTimeValue>, Box<AssemblyTimeValue>),
	/// expr - expr
	Subtract(Box<AssemblyTimeValue>, Box<AssemblyTimeValue>),
	/// expr * expr
	Multiply(Box<AssemblyTimeValue>, Box<AssemblyTimeValue>),
	/// expr / expr
	Divide(Box<AssemblyTimeValue>, Box<AssemblyTimeValue>),
}

impl AssemblyTimeValue {
	/// Return the first reference that can be found in this expression.
	#[must_use]
	pub fn first_reference(&self) -> Option<Reference> {
		match self {
			Self::Literal(..) => None,
			Self::Reference(reference) => Some(reference.clone()),
			Self::Negate(number) => number.first_reference(),
			Self::Add(lhs, rhs) | Self::Subtract(lhs, rhs) | Self::Multiply(lhs, rhs) | Self::Divide(lhs, rhs) =>
				lhs.first_reference().or_else(|| rhs.first_reference()),
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
			Self::Negate(val) => val.set_global_label(label),
			Self::Add(lhs, rhs) | Self::Subtract(lhs, rhs) | Self::Multiply(lhs, rhs) | Self::Divide(lhs, rhs) => {
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
				Reference::MacroGlobal { span, .. } => return Err(AssemblyError::UnresolvedReference {
					reference: reference.to_string(),
					reference_location: None,
					usage_location: *span,
					src: source_code
				}.into()),
				Reference::MacroArgument{value: None, span, ..} => return Err(AssemblyError::UnresolvedReference {
					reference: reference.to_string(),
					reference_location: None,
					usage_location: *span,
					src: source_code
				}.into()),
				Reference::MacroArgument{value: Some(value), span, ..} => value.try_value(location, source_code)?,
			},
			Self::Negate(number) => -number.try_value(location, source_code)?,
			Self::Add(lhs, rhs) => lhs.try_value(location, source_code.clone())? + rhs.try_value(location, source_code)?,
			Self::Subtract(lhs, rhs) => lhs.try_value(location, source_code.clone())? - rhs.try_value(location, source_code)?,
			Self::Multiply(lhs, rhs) => lhs.try_value(location, source_code.clone())? * rhs.try_value(location, source_code)?,
			Self::Divide(lhs, rhs) => lhs.try_value(location, source_code.clone())? / rhs.try_value(location, source_code)?,
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
			Self::Negate(number) => match number.try_resolve() {
				Self::Literal(value) => Self::Literal(-value),
				resolved => Self::Negate(Box::new(resolved)),
			},
			Self::Add(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Self::Literal(lhs_value), Self::Literal(rhs_value)) => Self::Literal(lhs_value + rhs_value),
				(lhs, rhs) => Self::Add(Box::new(lhs), Box::new(rhs)),
			},
			Self::Subtract(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Self::Literal(lhs_value), Self::Literal(rhs_value)) => Self::Literal(lhs_value - rhs_value),
				(lhs, rhs) => Self::Subtract(Box::new(lhs), Box::new(rhs)),
			},
			Self::Multiply(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Self::Literal(lhs_value), Self::Literal(rhs_value)) => Self::Literal(lhs_value * rhs_value),
				(lhs, rhs) => Self::Multiply(Box::new(lhs), Box::new(rhs)),
			},
			Self::Divide(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Self::Literal(lhs_value), Self::Literal(rhs_value)) => Self::Literal(lhs_value / rhs_value),
				(lhs, rhs) => Self::Divide(Box::new(lhs), Box::new(rhs)),
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
			Self::Negate(number) => number.replace_macro_parent(replacement_parent, source_code),
			Self::Add(lhs, rhs) | Self::Subtract(lhs, rhs) | Self::Multiply(lhs, rhs) | Self::Divide(lhs, rhs) => {
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

impl<T> From<(AssemblyTimeValue, T)> for AssemblyTimeValue {
	fn from(value: (AssemblyTimeValue, T)) -> Self {
		value.0
	}
}

// should be closure in upperhex formatter but borrow checker says no, again, no passing references to other closures
fn write_correctly(prefix: char, f: &mut Formatter<'_>, address: &AssemblyTimeValue) -> Result<(), Error> {
	f.write_char(prefix)?;
	fmt::UpperHex::fmt(address, f)
}

impl UpperHex for AssemblyTimeValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		let write_binary = |op, f: &mut Formatter, lhs: &AssemblyTimeValue, rhs: &AssemblyTimeValue| {
			write_correctly('(', f, lhs)?;
			write_correctly(op, f, rhs)?;
			f.write_char(')')
		};

		match self {
			Self::Reference(Reference::Global(ref unresolved_reference)) => {
				let unresolved_reference = unresolved_reference.borrow();
				match unresolved_reference.location {
					Some(ref numeric_address) => write_correctly('$', f, numeric_address),
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
			AssemblyTimeValue::Negate(number) => write_correctly('-', f, number.as_ref()),
			AssemblyTimeValue::Add(lhs, rhs) => write_binary('+', f, lhs, rhs),
			AssemblyTimeValue::Subtract(lhs, rhs) => write_binary('-', f, lhs.as_ref(), rhs.as_ref()),
			AssemblyTimeValue::Multiply(lhs, rhs) => write_binary('*', f, lhs.as_ref(), rhs.as_ref()),
			AssemblyTimeValue::Divide(lhs, rhs) => write_binary('/', f, lhs.as_ref(), rhs.as_ref()),
		}
	}
}