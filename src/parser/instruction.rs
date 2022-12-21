//! Instruction/AST-related structs created in the parser and consumed in the assembler.
#![allow(clippy::use_self)]

use core::fmt;
use std::cell::RefCell;
use std::fmt::{Display, Error, Formatter, UpperHex, Write};
use std::result::Result;
use std::sync::Arc;

use miette::SourceSpan;
use spcasm_derive::{Parse, VariantName};

use super::reference::{self, GlobalLabel, MacroParentReplacable, Reference};
use super::register::Register;
use crate::error::{AssemblyCode, AssemblyError};
use crate::parser::AssemblyTimeValue;
use crate::VariantName;

/// Types for representing data and memory addresses (this is overkill).
pub type MemoryAddress = i64;

/// One CPU instruction.
#[derive(Clone, Debug)]
pub struct Instruction {
	/// Label of this instruction, if any.
	pub label:          Option<Reference>,
	/// Opcode of this instruction (slightly misnamed)
	pub opcode:         Opcode,
	pub(crate) span:    SourceSpan,
	/// Only used for testing purposes: this is the data that the instruction should assemble to according to the test
	/// file.
	#[cfg(test)]
	pub expected_value: Option<Vec<u8>>,
}

impl Default for Instruction {
	fn default() -> Self {
		Self {
			label:                       None,
			opcode:                      Opcode {
				mnemonic:          Mnemonic::Nop,
				first_operand:     None,
				second_operand:    None,
				force_direct_page: false,
			},
			span:                        (0, 0).into(),
			#[cfg(test)]
			expected_value:              None,
		}
	}
}

impl Instruction {
	/// Returns the assembled size of this instruction. Every instruction requires at least 1 byte, but additional bytes
	/// are required for addresses and immediates.
	#[must_use]
	pub fn assembled_size(&self) -> u8 {
		self.opcode.assembled_size()
	}
}

impl MacroParentReplacable for Instruction {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.opcode.replace_macro_parent(replacement_parent, source_code)
	}
}

/// An instruction's core data that's used to generate machine code.
#[derive(Clone, Debug, PartialEq)]
pub struct Opcode {
	/// Instruction mnemonic.
	pub mnemonic:          Mnemonic,
	/// First operand, usually instruction target.
	pub first_operand:     Option<AddressingMode>,
	/// Second operand, usually instruction source. This is unused on many instructions.
	pub second_operand:    Option<AddressingMode>,
	/// Whether this opcode is forced to use direct page addressing.
	pub force_direct_page: bool,
}

impl Opcode {
	/// Returns the assembled size of this opcode. Every opcode requires at least 1 byte, but additional bytes are
	/// required for addresses and immediates.
	pub fn assembled_size(&self) -> u8 {
		match self.mnemonic {
			// These instructions depend 100% on their operands for their size, so we just add that up.
			Mnemonic::Mov
			| Mnemonic::Adc
			| Mnemonic::Sbc
			| Mnemonic::Cmp
			| Mnemonic::And
			| Mnemonic::Or
			| Mnemonic::Eor
			| Mnemonic::Inc
			| Mnemonic::Dec
			| Mnemonic::Asl
			| Mnemonic::Lsr
			| Mnemonic::Rol
			| Mnemonic::Ror
			| Mnemonic::Dbnz =>
				1 + self.first_operand.clone().map_or(0, AddressingMode::assembled_size)
					+ self.second_operand.clone().map_or(0, AddressingMode::assembled_size),
			// Just to be sure: for these instructions, we know they have a constant size, so we explicitly use that.
			Mnemonic::Xcn
			| Mnemonic::Mul
			| Mnemonic::Div
			| Mnemonic::Daa
			| Mnemonic::Das
			| Mnemonic::Tcall
			| Mnemonic::Brk
			| Mnemonic::Ret
			| Mnemonic::Ret1
			| Mnemonic::Reti
			| Mnemonic::Push
			| Mnemonic::Pop
			| Mnemonic::Clrc
			| Mnemonic::Setc
			| Mnemonic::Notc
			| Mnemonic::Clrv
			| Mnemonic::Clrp
			| Mnemonic::Setp
			| Mnemonic::Ei
			| Mnemonic::Di
			| Mnemonic::Nop
			| Mnemonic::Sleep
			| Mnemonic::Stop => 1,
			Mnemonic::Bra
			| Mnemonic::Beq
			| Mnemonic::Bne
			| Mnemonic::Bcs
			| Mnemonic::Bcc
			| Mnemonic::Bvs
			| Mnemonic::Bvc
			| Mnemonic::Bmi
			| Mnemonic::Bpl
			| Mnemonic::Movw
			| Mnemonic::Incw
			| Mnemonic::Decw
			| Mnemonic::Addw
			| Mnemonic::Subw
			| Mnemonic::Cmpw
			| Mnemonic::Pcall
			| Mnemonic::Set1
			| Mnemonic::Clr1 => 2,
			Mnemonic::And1
			| Mnemonic::Or1
			| Mnemonic::Eor1
			| Mnemonic::Not1
			| Mnemonic::Mov1
			| Mnemonic::Tset1
			| Mnemonic::Tset
			| Mnemonic::Tclr1
			| Mnemonic::Tclr
			| Mnemonic::Bbs
			| Mnemonic::Bbc
			| Mnemonic::Cbne
			| Mnemonic::Jmp
			| Mnemonic::Call => 3,
		}
	}

	/// Returns whether this opcode contains a two-byte "long" address.
	pub fn has_long_address(&self) -> bool {
		self.first_operand.clone().map_or(false, AddressingMode::has_long_address)
			|| self.second_operand.clone().map_or(false, AddressingMode::has_long_address)
	}

	/// Returns whether this opcode can use a direct page addressing mode. Many opcodes can, but some, like JMP,
	/// actually can't. Note that the result of this function may be wrong for opcodes that shouldn't contain addresses
	/// at all. These will fail the compiler later on, so the correctness is not important.
	pub fn can_use_direct_page_addressing(&self) -> bool {
		return ![
			Mnemonic::Jmp,
			Mnemonic::Call,
			Mnemonic::Tset1,
			Mnemonic::Tset,
			Mnemonic::Tclr,
			Mnemonic::Tclr1,
			Mnemonic::And1,
			Mnemonic::Or1,
			Mnemonic::Eor1,
			Mnemonic::Not1,
			Mnemonic::Mov1,
		]
		.contains(&self.mnemonic);
	}

	/// Return all references that this opcode points to.
	pub fn references(&self) -> Vec<&Reference> {
		let mut references = self.first_operand.as_ref().map(AddressingMode::references).unwrap_or_default();
		let mut more_references = self.second_operand.as_ref().map(AddressingMode::references).unwrap_or_default();
		references.append(&mut more_references);
		references
	}
}

impl MacroParentReplacable for Opcode {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		for operand in self.first_operand.iter_mut().chain(self.second_operand.iter_mut()) {
			operand.replace_macro_parent(replacement_parent.clone(), source_code)?;
		}
		Ok(())
	}
}

/// Instruction mnemonics of the SPC700.
#[allow(missing_docs, clippy::use_self)]
#[derive(Clone, Debug, Copy, Eq, PartialEq, Parse, VariantName)]
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
	Reti,
	Push,
	Pop,
	Set1,
	Clr1,
	Tset1,
	Tset,
	Tclr1,
	Tclr,
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

impl Mnemonic {
	/// Coerce alternative mnemonics into the canonical version the assembler uses internally.
	#[must_use]
	pub const fn coerce_alternate_mnemonics(self) -> Self {
		match self {
			Self::Reti => Self::Ret1,
			Self::Tset => Self::Tset1,
			Self::Tclr => Self::Tclr1,
			_ => self,
		}
	}
}

impl Display for Mnemonic {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", self.variant_name().to_uppercase())
	}
}

/// Addressing modes of the SPC700. Not all of these are supported everywhere (in fact, most aren't).
#[derive(Clone, Debug, PartialEq)]
pub enum AddressingMode {
	/// #immediate
	Immediate(AssemblyTimeValue),
	/// (X)
	IndirectX,
	/// (Y)
	IndirectY,
	/// (X) with automatic X++
	IndirectXAutoIncrement,
	/// (dp)
	DirectPage(AssemblyTimeValue),
	/// dp+X
	DirectPageXIndexed(AssemblyTimeValue),
	/// dp+Y
	DirectPageYIndexed(AssemblyTimeValue),
	/// abs
	Address(AssemblyTimeValue),
	/// abs+X
	XIndexed(AssemblyTimeValue),
	/// abs+Y
	YIndexed(AssemblyTimeValue),
	/// (dp+X)
	DirectPageXIndexedIndirect(AssemblyTimeValue),
	/// (dp)+Y
	DirectPageIndirectYIndexed(AssemblyTimeValue),
	/// dp.bit
	DirectPageBit(AssemblyTimeValue, u8),
	/// abs.bit
	AddressBit(AssemblyTimeValue, u8),
	/// /abs.bit
	NegatedAddressBit(AssemblyTimeValue, u8),
	/// C
	CarryFlag,
	/// A, X, Y, SP, ...
	Register(Register),
}

impl AddressingMode {
	/// Checks the given bit index for validity and possibly errors out.
	/// # Errors
	/// If the bit index is not valid.
	#[allow(clippy::result_large_err, clippy::missing_const_for_fn)]
	pub fn check_bit(bit_index: u8, location: SourceSpan, src: &Arc<AssemblyCode>) -> Result<u8, AssemblyError> {
		if bit_index <= 7 {
			Ok(bit_index)
		} else {
			Err(AssemblyError::InvalidBitIndex { index: bit_index, location, src: src.clone() })
		}
	}

	/// Set this global label as the parent for all the unresolved local labels.
	pub fn set_global_label(&mut self, label: &Arc<RefCell<GlobalLabel>>) {
		if let Some(number) = self.number_mut() {
			number.set_global_label(label);
		}
	}

	/// Return the number that this addressing mode references (mostly as an address), if any. The original number is
	/// borrowed.
	#[must_use]
	#[allow(clippy::missing_const_for_fn)]
	pub fn number_ref(&self) -> Option<&AssemblyTimeValue> {
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

	/// Return the number that this addressing mode references (mostly as an address), if any.
	#[must_use]
	#[allow(clippy::missing_const_for_fn)]
	pub fn number(&self) -> Option<AssemblyTimeValue> {
		self.number_ref().cloned()
	}

	/// Returns a mutable reference to the number this addressing mode references, if any.
	pub fn number_mut(&mut self) -> Option<&mut AssemblyTimeValue> {
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
		if let Some(AssemblyTimeValue::Literal(resolved_address)) = self.number().map(AssemblyTimeValue::try_resolve) && resolved_address <= 0xFF {
			let number = AssemblyTimeValue::Literal(resolved_address);
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

	/// Force this addressing mode into direct page addressing regardless of the internal number.
	#[must_use]
	#[allow(clippy::missing_const_for_fn)] // false positive
	pub fn force_to_direct_page_addressing(self) -> Self {
		match self {
			Self::Address(number) => Self::DirectPage(number),
			Self::XIndexed(number) => Self::DirectPageXIndexed(number),
			Self::YIndexed(number) => Self::DirectPageYIndexed(number),
			Self::AddressBit(number, bit) => Self::DirectPageBit(number, bit),
			_ => self,
		}
	}

	/// Returns the assembled size of this addressing mode. This size is always added to the instruction using the
	/// addressing mode.
	#[allow(clippy::missing_const_for_fn)]
	#[must_use]
	pub fn assembled_size(self) -> u8 {
		match self {
			AddressingMode::IndirectX
			| AddressingMode::IndirectY
			| AddressingMode::IndirectXAutoIncrement
			| AddressingMode::CarryFlag
			| AddressingMode::Register(_) => 0,
			AddressingMode::Immediate(_)
			| AddressingMode::DirectPageXIndexedIndirect(_)
			| AddressingMode::DirectPageIndirectYIndexed(_)
			| AddressingMode::DirectPage(_)
			| AddressingMode::DirectPageXIndexed(_)
			| AddressingMode::DirectPageBit(_, _) // bit will be merged into opcode byte, so it needs no extra space
			| AddressingMode::DirectPageYIndexed(_) => 1,
			AddressingMode::AddressBit(_, _)
			| AddressingMode::NegatedAddressBit(_, _)
			| AddressingMode::Address(_)
			| AddressingMode::XIndexed(_)
			| AddressingMode::YIndexed(_) => 2,
		}
	}

	/// Returns whether this addressing mode contains a long, i.e. two-byte, address.
	#[allow(clippy::missing_const_for_fn)]
	#[must_use]
	pub fn has_long_address(self) -> bool {
		matches!(self, Self::XIndexed(..) | Self::YIndexed(..) | Self::AddressBit(..) | Self::Address(..))
	}

	fn references(&self) -> Vec<&Reference> {
		self.number_ref().map(AssemblyTimeValue::references).unwrap_or_default()
	}
}

impl MacroParentReplacable for AddressingMode {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			AddressingMode::Immediate(number)
			| AddressingMode::DirectPage(number)
			| AddressingMode::DirectPageXIndexed(number)
			| AddressingMode::DirectPageYIndexed(number)
			| AddressingMode::Address(number)
			| AddressingMode::XIndexed(number)
			| AddressingMode::YIndexed(number)
			| AddressingMode::DirectPageXIndexedIndirect(number)
			| AddressingMode::DirectPageIndirectYIndexed(number)
			| AddressingMode::DirectPageBit(number, _)
			| AddressingMode::AddressBit(number, _)
			| AddressingMode::NegatedAddressBit(number, _) => number.replace_macro_parent(replacement_parent, source_code),
			AddressingMode::IndirectX
			| AddressingMode::IndirectY
			| AddressingMode::IndirectXAutoIncrement
			| AddressingMode::CarryFlag
			| AddressingMode::Register(_) => Ok(()),
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
