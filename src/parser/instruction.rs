//! Instruction/AST-related structs created in the parser and consumed in the assembler.
#![allow(clippy::use_self)]

use core::fmt;
use std::cell::RefCell;
use std::fmt::{Display, Error, Formatter, UpperHex, Write};
use std::result::Result;
use std::sync::Arc;

use miette::SourceSpan;
use serde::Serialize;
use serde_variant::to_variant_name;
use spcasm_derive::Parse;

use super::label::{self, GlobalLabel, Label};
use super::register::Register;
use crate::error::{AssemblyCode, AssemblyError};

/// Types for representing data and memory addresses (this is overkill).
pub type MemoryAddress = i64;

/// One CPU instruction.
#[derive(Clone, Debug)]
pub struct Instruction {
	/// Label of this instruction, if any.
	pub label:          Option<Label>,
	/// Opcode of this instruction (slightly misnamed)
	pub opcode:         Opcode,
	pub(crate) span:    SourceSpan,
	/// Only used for testing purposes: this is the data that the instruction should assemble to according to the test
	/// file.
	#[cfg(test)]
	pub expected_value: Option<Vec<u8>>,
	#[cfg(test)]
	pub assembled_size: Option<u8>,
}

impl Default for Instruction {
	fn default() -> Self {
		Self {
			label:                       None,
			opcode:                      Opcode {
				mnemonic:       Mnemonic::Nop,
				first_operand:  None,
				second_operand: None,
			},
			span:                        (0, 0).into(),
			#[cfg(test)]
			expected_value:              None,
			#[cfg(test)]
			assembled_size:              None,
		}
	}
}

/// An instruction's core data that's used to generate machine code.
#[derive(Clone, Debug, PartialEq)]
pub struct Opcode {
	/// Instruction mnemonic.
	pub mnemonic:       Mnemonic,
	/// First operand, usually instruction target.
	pub first_operand:  Option<AddressingMode>,
	/// Second operand, usually instruction source. This is unused on many instructions.
	pub second_operand: Option<AddressingMode>,
}

impl Opcode {
	/// Create an instruction with two operands. It might not actually be valid with some combinations of addressing
	/// modes.
	#[must_use]
	pub const fn make_two_operand_instruction(
		mnemonic: Mnemonic,
		destination: AddressingMode,
		source: AddressingMode,
	) -> Self {
		Self { mnemonic, first_operand: Some(destination), second_operand: Some(source) }
	}

	/// Create an instruction with one operand. It might not actually be valid with some combinations of addressing
	/// modes.
	#[must_use]
	pub const fn make_single_operand_instruction(mnemonic: Mnemonic, destination: AddressingMode) -> Self {
		Self { mnemonic, first_operand: Some(destination), second_operand: None }
	}
}

/// Instruction mnemonics of the SPC700.
#[allow(missing_docs, clippy::use_self)]
#[derive(Clone, Debug, Copy, Eq, PartialEq, Parse, Serialize)]
pub enum Mnemonic {
	Mov,
	Adc,
	Sbc,
	Cmp,
	And,
	Or,
	Eor,
	Inc,
	Dec,
	Asl,
	Lsr,
	Rol,
	Ror,
	Xcn,
	Movw,
	Incw,
	Decw,
	Addw,
	Subw,
	Cmpw,
	Mul,
	Div,
	Daa,
	Das,
	Bra,
	Beq,
	Bne,
	Bcs,
	Bcc,
	Bvs,
	Bvc,
	Bmi,
	Bpl,
	Bbs,
	Bbc,
	Cbne,
	Dbnz,
	Jmp,
	Call,
	Pcall,
	Tcall,
	Brk,
	Ret,
	Ret1,
	Push,
	Pop,
	Set1,
	Clr1,
	Tset1,
	Tclr1,
	And1,
	Or1,
	Eor1,
	Not1,
	Mov1,
	Clrc,
	Setc,
	Notc,
	Clrv,
	Clrp,
	Setp,
	Ei,
	Di,
	Nop,
	Sleep,
	Stop,
}

impl Display for Mnemonic {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", to_variant_name(self).unwrap().to_uppercase())
	}
}

/// Any number, may be an expression that can be calculated at assembly time.
#[derive(Clone, Debug, PartialEq)]
pub enum Number {
	/// A literal number.
	Literal(MemoryAddress),
	/// A label that will resolve to a number later.
	Label(Label),
	/// - expr; negating another number.
	Negate(Box<Number>),
	/// expr + expr
	Add(Box<Number>, Box<Number>),
	/// expr - expr
	Subtract(Box<Number>, Box<Number>),
	/// expr * expr
	Multiply(Box<Number>, Box<Number>),
	/// expr / expr
	Divide(Box<Number>, Box<Number>),
}

impl Number {
	/// Return the first label that can be found in this expression.
	#[must_use]
	pub fn first_label(&self) -> Option<Label> {
		match self {
			Self::Literal(..) => None,
			Self::Label(label) => Some(label.clone()),
			Self::Negate(number) => number.first_label(),
			Self::Add(lhs, rhs) | Self::Subtract(lhs, rhs) | Self::Multiply(lhs, rhs) | Self::Divide(lhs, rhs) =>
				lhs.first_label().or_else(|| rhs.first_label()),
		}
	}

	/// Sets the given global label as the parent for all unresolved local labels.
	/// # Panics
	/// All panics are programming errors.
	pub fn set_global_label(&mut self, label: &Arc<RefCell<GlobalLabel>>) {
		match self {
			Self::Label(Label::Local(local)) =>
				*local = label::merge_local_into_parent(local.clone(), Some(label.clone()), &Arc::default()).unwrap(),
			Self::Negate(val) => val.set_global_label(label),
			Self::Add(lhs, rhs) | Self::Subtract(lhs, rhs) | Self::Multiply(lhs, rhs) | Self::Divide(lhs, rhs) => {
				lhs.set_global_label(label);
				rhs.set_global_label(label);
			},
			Self::Literal(..) | Self::Label(Label::Global(..)) => (),
		}
	}

	/// Extracts the actual value of this number.
	/// # Panics
	/// If the label was not yet resolved, this function panics as it assumes that that has already happened.
	#[must_use]
	pub fn value(&self) -> MemoryAddress {
		self.try_value((0,0).into(), Arc::default()).unwrap()
	}

	/// Extracts the actual value of this number, if possible.
	/// # Errors
	/// If the number cannot be resolved to a final numeric value.
	pub fn try_value(&self, location: SourceSpan, source_code: Arc<AssemblyCode>) -> Result<MemoryAddress, AssemblyError> {
		Ok(match self {
			Self::Literal(value) => *value,
			// necessary because matching through an Rc is not possible right now (would be super dope though).
			Self::Label(ref label) => match label {
				Label::Global(global_label) if let Some(ref value) = global_label.borrow().location => value.value(),
				Label::Local(local) => return Err(AssemblyError::UnresolvedLabel { 
					label: local.borrow().name.clone(),
					label_location: local.borrow().span,
					usage_location: location,
					src: source_code
				}),
				Label::Global(global) => return Err(AssemblyError::UnresolvedLabel {
					label: global.borrow().name.clone(),
					label_location: global.borrow().span,
					usage_location: location,
					src: source_code
				}),
			},
			Self::Negate(number) => -number.try_value(location, source_code)?,
			Self::Add(lhs, rhs) => lhs.try_value(location, source_code.clone())? + rhs.try_value(location, source_code)?,
			Self::Subtract(lhs, rhs) => lhs.try_value(location, source_code.clone())? - rhs.try_value(location, source_code)?,
			Self::Multiply(lhs, rhs) => lhs.try_value(location, source_code.clone())? * rhs.try_value(location, source_code)?,
			Self::Divide(lhs, rhs) => lhs.try_value(location, source_code.clone())? / rhs.try_value(location, source_code)?,
		})
	}

	/// Try to resolve this number down to a literal. Even if that's not entirely possible, sub-expressions are
	/// collapsed and resolved as far as possible.
	#[must_use]
	pub fn try_resolve(self) -> Self {
		match self {
			Self::Label(Label::Global(global)) if let Some(memory_location) = global.clone().borrow().location.clone() => memory_location.try_resolve(),
			Self::Label(Label::Local(local)) if let Some(memory_location) = local.clone().borrow().location.clone() => memory_location.try_resolve(),
			Number::Negate(number) => match number.try_resolve() {
				Number::Literal(value) => Number::Literal(-value),
				resolved => Number::Negate(Box::new(resolved)),
			},
			Number::Add(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Number::Literal(lhs_value), Number::Literal(rhs_value)) => Number::Literal(lhs_value + rhs_value),
				(lhs, rhs) => Number::Add(Box::new(lhs), Box::new(rhs)),
			},
			Number::Subtract(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Number::Literal(lhs_value), Number::Literal(rhs_value)) => Number::Literal(lhs_value - rhs_value),
				(lhs, rhs) => Number::Subtract(Box::new(lhs), Box::new(rhs)),
			},
			Number::Multiply(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Number::Literal(lhs_value), Number::Literal(rhs_value)) => Number::Literal(lhs_value * rhs_value),
				(lhs, rhs) => Number::Multiply(Box::new(lhs), Box::new(rhs)),
			},
			Number::Divide(lhs, rhs) => match (lhs.try_resolve(), rhs.try_resolve()) {
				(Number::Literal(lhs_value), Number::Literal(rhs_value)) => Number::Literal(lhs_value / rhs_value),
				(lhs, rhs) => Number::Divide(Box::new(lhs), Box::new(rhs)),
			},
			_ => self,
		}
	}
}

impl From<MemoryAddress> for Number {
	fn from(address: MemoryAddress) -> Self {
		Self::Literal(address)
	}
}

impl<T> From<(Number, T)> for Number {
	fn from(value: (Number, T)) -> Self {
		value.0
	}
}

// should be closure in upperhex formatter but borrow checker says no, again, no passing references to other closures
fn write_correctly(prefix: char, f: &mut Formatter<'_>, address: &Number) -> Result<(), Error> {
	f.write_char(prefix)?;
	fmt::UpperHex::fmt(address, f)
}

impl UpperHex for Number {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		let write_binary = |op, f: &mut Formatter, lhs: &Number, rhs: &Number| {
			write_correctly('(', f, lhs)?;
			write_correctly(op, f, rhs)?;
			f.write_char(')')
		};

		match self {
			Self::Label(Label::Global(ref unresolved_label)) => {
				let unresolved_label = unresolved_label.borrow();
				match unresolved_label.location {
					Some(ref numeric_address) => write_correctly('$', f, numeric_address),
					None => write!(f, "{}", unresolved_label.name),
				}
			},
			Self::Label(Label::Local(ref local)) => {
				let local = local.borrow();
				match &local.location {
					Some(numeric_address) => f.pad(&format!("{:0X}", **numeric_address)),
					None => write!(f, "{}", local.name),
				}
			},
			Self::Literal(numeric_address) => {
				f.write_char('$')?;
				fmt::UpperHex::fmt(numeric_address, f)
			},
			Number::Negate(number) => write_correctly('-', f, number.as_ref()),
			Number::Add(lhs, rhs) => write_binary('+', f, lhs, rhs),
			Number::Subtract(lhs, rhs) => write_binary('-', f, lhs.as_ref(), rhs.as_ref()),
			Number::Multiply(lhs, rhs) => write_binary('*', f, lhs.as_ref(), rhs.as_ref()),
			Number::Divide(lhs, rhs) => write_binary('/', f, lhs.as_ref(), rhs.as_ref()),
		}
	}
}

/// Addressing modes of the SPC700. Not all of these are supported everywhere (in fact, most aren't).
#[derive(Clone, Debug, PartialEq)]
pub enum AddressingMode {
	/// #immediate
	Immediate(Number),
	/// (X)
	IndirectX,
	/// (Y)
	IndirectY,
	/// (X) with automatic X++
	IndirectXAutoIncrement,
	/// (dp)
	DirectPage(Number),
	/// dp+X
	DirectPageXIndexed(Number),
	/// dp+Y
	DirectPageYIndexed(Number),
	/// abs
	Address(Number),
	/// abs+X
	XIndexed(Number),
	/// abs+Y
	YIndexed(Number),
	/// (dp+X)
	DirectPageXIndexedIndirect(Number),
	/// (dp)+Y
	DirectPageIndirectYIndexed(Number),
	/// dp.bit
	DirectPageBit(Number, u8),
	/// abs.bit
	AddressBit(Number, u8),
	/// /abs.bit
	NegatedAddressBit(Number, u8),
	/// C
	CarryFlag,
	/// A, X, Y, SP, ...
	Register(Register),
}

impl AddressingMode {
	/// Set this global label as the parent for all the unresolved local labels.
	pub fn set_global_label(&mut self, label: &Arc<RefCell<GlobalLabel>>) {
		if let Some(number) = self.number_mut() {
			number.set_global_label(label);
		}
	}

	/// Return the number that this addressing mode references (mostly as an address), if any.
	#[must_use]
	pub fn number(&self) -> Option<Number> {
		match self {
			Self::Immediate(number)
			| Self::DirectPage(number)
			| Self::DirectPageBit(number, ..)
			| Self::DirectPageIndirectYIndexed(number, ..)
			| Self::DirectPageXIndexed(number)
			| Self::DirectPageXIndexedIndirect(number)
			| Self::DirectPageYIndexed(number)
			| Self::AddressBit(number, ..)
			| Self::NegatedAddressBit(number, ..)
			| Self::Address(number)
			| Self::XIndexed(number)
			| Self::YIndexed(number) => Some(number.clone()),
			_ => None,
		}
	}

	/// Returns a mutable reference to the number this addressing mode references, if any.
	pub fn number_mut(&mut self) -> Option<&mut Number> {
		match self {
			Self::Immediate(number)
			| Self::DirectPage(number)
			| Self::DirectPageBit(number, ..)
			| Self::DirectPageIndirectYIndexed(number, ..)
			| Self::DirectPageXIndexed(number)
			| Self::DirectPageXIndexedIndirect(number)
			| Self::DirectPageYIndexed(number)
			| Self::AddressBit(number, ..)
			| Self::NegatedAddressBit(number, ..)
			| Self::Address(number)
			| Self::XIndexed(number)
			| Self::YIndexed(number) => Some(number),
			_ => None,
		}
	}

	/// Try to coerce this addressing mode to direct page addressing if the internal number allows it.
	#[must_use]
	pub fn coerce_to_direct_page_addressing(self) -> Self {
		if let Some(Number::Literal(resolved_address)) = self.number().map(Number::try_resolve) && resolved_address <= 0xFF {
			let number = Number::Literal(resolved_address);
			match self {
				Self::Address(..) => Self::DirectPage(number),
				Self::XIndexed(..) => Self::DirectPageXIndexed(number),
				Self::YIndexed(..) => Self::DirectPageYIndexed(number),
				Self::AddressBit(_, bit) => Self::DirectPageBit(number, bit),
				_ => self,
			}
		} else {
			self
		}
	}
}

impl Display for AddressingMode {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		f.pad(&match self {
			Self::Immediate(number) => format!("#{:02X}", number),
			Self::IndirectX => "(X)".to_owned(),
			Self::IndirectY => "(Y)".to_owned(),
			Self::CarryFlag => "C".to_owned(),
			Self::IndirectXAutoIncrement => "(X)+".to_owned(),
			Self::DirectPage(address) => format!("{:02X}", address),
			Self::DirectPageXIndexed(address) => format!("{:02X}+X", address),
			Self::DirectPageYIndexed(address) => format!("{:02X}+Y", address),
			Self::DirectPageXIndexedIndirect(address) => format!("({:02X}+X)", address),
			Self::DirectPageIndirectYIndexed(address) => format!("({:02X})+Y", address),
			Self::Address(address) => format!("{:04X}", address),
			Self::XIndexed(address) => format!("{:04X}+X", address),
			Self::YIndexed(address) => format!("{:04X}+Y", address),
			Self::DirectPageBit(address, bit) => format!("{:02X}.{:01}", address, bit),
			Self::AddressBit(address, bit) => format!("{:04X}.{:01}", address, bit),
			Self::NegatedAddressBit(address, bit) => format!("/{:04X}.{:01}", address, bit),
			Self::Register(register) => format!("{}", register),
		})
	}
}