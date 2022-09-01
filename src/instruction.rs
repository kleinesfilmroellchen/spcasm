//! Instruction/AST-related structs created in the parser and consumed in the assembler.
#![allow(clippy::use_self)]

use std::fmt::{Display, Error, Formatter, UpperHex};
use std::result::Result;

use miette::SourceSpan;
use serde::Serialize;
use serde_variant::to_variant_name;
use spcasm_derive::Parse;

use super::label::{GlobalLabel, Label};
use super::Register;
use crate::label::LocalLabel;
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
	pub expected_value: Vec<u8>,
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

/// Any number, either a literal or a label that's resolved to a number later.
#[derive(Clone, Debug, PartialEq)]
pub enum Number {
	/// A literal number.
	Literal(MemoryAddress),
	/// A label that will resolve to a number later.
	Label(Label),
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
			Self::Label(ref label) => match label {
				Label::Global(global_label) if let Some(value) = global_label.location => value,
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
			Self::Label(Label::Global(ref unresolved_label)) => match &**unresolved_label {
				GlobalLabel { location: Some(numeric_address), .. } => write!(f, "${:X}", numeric_address),
				GlobalLabel { name, .. } => write!(f, "{}", name),
			},
			Self::Label(Label::Local(LocalLabel { location: Some(numeric_address), .. }))
			| Self::Literal(numeric_address) => write!(f, "${:X}", numeric_address),
			Self::Label(Label::Local(LocalLabel { name, .. })) => write!(f, "{}", name),
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

impl Display for AddressingMode {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		write!(f, "{}", match self {
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
