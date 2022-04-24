//! Instruction/AST-related structs created in the parser and consumed in the assembler.
#![allow(clippy::use_self)]

use std::fmt::{Display, Error, Formatter, UpperHex};
use std::result::Result;
use std::sync::Arc;

use miette::SourceSpan;
use serde::Serialize;
use serde_variant::to_variant_name;
use spcasm_derive::Parse;

use crate::error::{AssemblyCode, AssemblyError};
use crate::Register;
/// Types for representing data and memory addresses (this is overkill).
pub type MemoryAddress = i64;

/// One CPU instruction.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
	/// Label of this instruction, if any.
	pub label:          Option<Arc<Label>>,
	/// Opcode of this instruction (slightly misnamed)
	pub opcode:         Opcode,
	pub(crate) span:    SourceSpan,
	/// Only used for testing purposes: this is the data that the instruction should assemble to according to the test
	/// file.
	#[cfg(test)]
	pub expected_value: Vec<u8>,
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
	/// User-given label name.
	pub name:            String,
	/// Resolved memory location of the label, if any.
	pub location:        Option<MemoryAddress>,
	/// Source code location where this label is defined.
	pub span:            SourceSpan,
	/// Whether anyone references this label as an address.
	pub used_as_address: bool,
}

impl Label {
	/// Whether this label has already been resolved to a memory location.
	#[must_use]
	pub const fn is_resolved(&self) -> bool {
		self.location.is_some()
	}

	/// Resolves the label to the given memory location.
	pub fn resolve_to(&mut self, location: MemoryAddress, source_code: Arc<AssemblyCode>) {
		if location <= 0xFF && self.used_as_address {
			println!(
				"{:?}",
				miette::Report::new(AssemblyError::NonDirectPageLabel {
					name:     self.name.clone(),
					location: self.span,
					src:      source_code,
				})
			);
		}
		self.location = Some(location);
	}
}

/// An instruction's core data that's used to generate machine code.
#[derive(Clone, Debug, Eq, PartialEq)]
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

/// Any number, either a literal or a label that's resolved to a number later.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Number {
	/// A literal number.
	Literal(MemoryAddress),
	/// A label that will resolve to a number later.
	Label(Arc<Label>),
	// TODO: support assembly-time calculations
}

impl Number {
	/// Extracts the actual value of this number.
	/// # Panics
	/// If the label was not yet resolved, this function panics as it assumes that that has already happened.
	#[must_use]
	pub fn value(&self) -> MemoryAddress {
		match self {
			Self::Literal(value) => *value,
			// necessary because matching through an Rc is not possible right now (would be super dope though).
			Self::Label(label) => match **label {
				Label { location: Some(value), .. } => value,
				_ => panic!("Unresolved label {:?}", label),
			},
		}
	}
}

impl From<MemoryAddress> for Number {
	fn from(address: MemoryAddress) -> Self {
		Self::Literal(address)
	}
}

impl UpperHex for Number {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		match self {
			Self::Literal(numeric_address) => write!(f, "{:X}", numeric_address),
			Self::Label(ref unresolved_label) => match &**unresolved_label {
				Label { location: Some(numeric_address), .. } => write!(f, "{:X}", numeric_address),
				Label { name, .. } => write!(f, "<{}>", name),
			},
		}
	}
}

/// Addressing modes of the SPC700. Not all of these are supported everywhere (in fact, most aren't).
#[derive(Clone, Debug, Eq, PartialEq)]
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
	// ...
	/// A, X, Y, SP, ...
	Register(Register),
}

impl Display for AddressingMode {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		write!(f, "{}", match self {
			Self::Immediate(number) => format!("#${:02X}", number),
			Self::IndirectX => "(X)".to_owned(),
			Self::IndirectY => "(Y)".to_owned(),
			Self::IndirectXAutoIncrement => "(X)+".to_owned(),
			Self::DirectPage(address) => format!("${:02X}", address),
			Self::DirectPageXIndexed(address) => format!("${:02X}+X", address),
			Self::DirectPageYIndexed(address) => format!("${:02X}+Y", address),
			Self::DirectPageXIndexedIndirect(address) => format!("(${:02X}+X)", address),
			Self::DirectPageIndirectYIndexed(address) => format!("(${:02X})+Y", address),
			Self::Address(address) => format!("${:04X}", address),
			Self::XIndexed(address) => format!("${:04X}+X", address),
			Self::YIndexed(address) => format!("${:04X}+Y", address),
			Self::DirectPageBit(address, bit) => format!("${:02X}.{:01}", address, bit),
			Self::AddressBit(address, bit) => format!("${:04X}.{:01}", address, bit),
			Self::Register(register) => format!("{}", register),
		})
	}
}
