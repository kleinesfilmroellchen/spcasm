//! Instruction/AST-related structs created in the parser and consumed in the assembler.
#![allow(clippy::use_self)]

use std::cell::RefCell;
use std::fmt::{Display, Error, Formatter};
use std::result::Result;
use std::sync::Arc;

use miette::SourceSpan;
#[allow(unused)]
use smartstring::alias::String;
use spcasm_derive::{Parse, VariantName};

use super::reference::{self, Label, Reference, ReferenceResolvable};
use super::register::Register;
use crate::error::AssemblyError;
use crate::parser::program::span_to_string;
use crate::parser::AssemblyTimeValue;
use crate::{AssemblyCode, VariantName};

/// Types for representing data and memory addresses (this is overkill).
pub type MemoryAddress = i64;

/// One CPU instruction.
#[derive(Clone, Debug)]
pub struct Instruction {
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

impl ReferenceResolvable for Instruction {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RefCell<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.opcode.replace_macro_parent(replacement_parent, source_code)
	}

	fn resolve_relative_labels(
		&mut self,
		direction: reference::RelativeReferenceDirection,
		relative_labels: &std::collections::HashMap<std::num::NonZeroU64, Arc<RefCell<Label>>>,
	) {
		self.opcode.resolve_relative_labels(direction, relative_labels);
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RefCell<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.opcode.set_current_label(current_label, source_code)
	}
}

impl std::fmt::Display for Instruction {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}", span_to_string(self.span), self.opcode)?;
		#[cfg(test)]
		write!(f, " {}", crate::parser::program::byte_vec_to_string(&self.expected_value))?;
		Ok(())
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
	#[allow(clippy::too_many_lines)]
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
			| Mnemonic::Set
			| Mnemonic::Set0
			| Mnemonic::Set1
			| Mnemonic::Set2
			| Mnemonic::Set3
			| Mnemonic::Set4
			| Mnemonic::Set5
			| Mnemonic::Set6
			| Mnemonic::Set7
			| Mnemonic::Clr
			| Mnemonic::Clr0
			| Mnemonic::Clr1
			| Mnemonic::Clr2
			| Mnemonic::Clr3
			| Mnemonic::Clr4
			| Mnemonic::Clr5
			| Mnemonic::Clr6
			| Mnemonic::Clr7 => 2,
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
			| Mnemonic::Bbs0
			| Mnemonic::Bbs1
			| Mnemonic::Bbs2
			| Mnemonic::Bbs3
			| Mnemonic::Bbs4
			| Mnemonic::Bbs5
			| Mnemonic::Bbs6
			| Mnemonic::Bbs7
			| Mnemonic::Bbc
			| Mnemonic::Bbc0
			| Mnemonic::Bbc1
			| Mnemonic::Bbc2
			| Mnemonic::Bbc3
			| Mnemonic::Bbc4
			| Mnemonic::Bbc5
			| Mnemonic::Bbc6
			| Mnemonic::Bbc7
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
	#[must_use]
	pub fn can_use_direct_page_addressing(&self) -> bool {
		// Some instructions plain-out never accept direct page addressing.
		!([
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
		.contains(&self.mnemonic) ||
		// The MOV, ADC, SBC, CMP instruction allow some forms of direct page addressing, but not others! For example, dp+Y,A is impossible, but dp+Y,X is possible.
		([Mnemonic::Mov, Mnemonic::Adc, Mnemonic::Sbc, Mnemonic::Cmp, Mnemonic::And, Mnemonic::Or, Mnemonic::Eor].contains(&self.mnemonic) &&
			matches!((&self.first_operand, &self.second_operand),
				(Some(AddressingMode::Register(Register::A)), Some(AddressingMode::YIndexed(..))) |
				(Some(AddressingMode::YIndexed(..)), Some(AddressingMode::Register(Register::A)))
			)
		))
	}

	/// Return all references that this opcode points to, and the corresponding assembly time calculations.
	#[must_use]
	pub fn references_and_calculations(&self) -> Vec<(&Reference, &AssemblyTimeValue)> {
		let mut references = self
			.first_operand
			.as_ref()
			.map(|first_operand| {
				first_operand
					.references()
					.into_iter()
					.map(|reference| {
						(
							reference,
							first_operand
								.number_ref()
								.expect("if references exist on first operand, so must an assembly time value"),
						)
					})
					.collect::<Vec<_>>()
			})
			.unwrap_or_default();
		let mut more_references = self
			.second_operand
			.as_ref()
			.map(|second_operand| {
				second_operand
					.references()
					.into_iter()
					.map(|reference| {
						(
							reference,
							second_operand
								.number_ref()
								.expect("if references exist on second operand, so must an assembly time value"),
						)
					})
					.collect::<Vec<_>>()
			})
			.unwrap_or_default();
		references.append(&mut more_references);
		references
	}
}

impl ReferenceResolvable for Opcode {
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

	fn resolve_relative_labels(
		&mut self,
		direction: reference::RelativeReferenceDirection,
		relative_labels: &std::collections::HashMap<std::num::NonZeroU64, Arc<RefCell<Label>>>,
	) {
		for operand in self.first_operand.iter_mut().chain(self.second_operand.iter_mut()) {
			operand.resolve_relative_labels(direction, relative_labels);
		}
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RefCell<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		for operand in self.first_operand.iter_mut().chain(self.second_operand.iter_mut()) {
			operand.set_current_label(current_label, source_code)?;
		}
		Ok(())
	}
}

impl std::fmt::Display for Opcode {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{:5} {:>12} , {:>12}",
			self.mnemonic,
			self.first_operand.as_ref().map_or_else(|| "[none]".into(), AddressingMode::to_string),
			self.second_operand.as_ref().map_or_else(|| "[none]".into(), AddressingMode::to_string),
		)
	}
}

/// Instruction mnemonics of the SPC700.
#[allow(missing_docs, clippy::use_self)]
#[derive(Clone, Debug, Copy, Eq, PartialEq, Hash, Parse, VariantName)]
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
	Bbs0,
	Bbs1,
	Bbs2,
	Bbs3,
	Bbs4,
	Bbs5,
	Bbs6,
	Bbs7,
	Bbc,
	Bbc0,
	Bbc1,
	Bbc2,
	Bbc3,
	Bbc4,
	Bbc5,
	Bbc6,
	Bbc7,
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
	Set,
	Set0,
	Set1,
	Set2,
	Set3,
	Set4,
	Set5,
	Set6,
	Set7,
	Clr,
	Clr0,
	Clr1,
	Clr2,
	Clr3,
	Clr4,
	Clr5,
	Clr6,
	Clr7,
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

	/// Extract the bit index embedded in this mnemonic. spcasm supports some alternate mnemonics that provide the bit
	/// index themselves instead of it being part of the addressing mode.
	#[must_use]
	pub const fn bit_index(&self) -> Option<u8> {
		match self {
			Self::Bbc0 | Self::Bbs0 | Self::Set0 | Self::Clr0 => Some(0),
			Self::Bbc1 | Self::Bbs1 | Self::Set1 | Self::Clr1 => Some(1),
			Self::Bbc2 | Self::Bbs2 | Self::Set2 | Self::Clr2 => Some(2),
			Self::Bbc3 | Self::Bbs3 | Self::Set3 | Self::Clr3 => Some(3),
			Self::Bbc4 | Self::Bbs4 | Self::Set4 | Self::Clr4 => Some(4),
			Self::Bbc5 | Self::Bbs5 | Self::Set5 | Self::Clr5 => Some(5),
			Self::Bbc6 | Self::Bbs6 | Self::Set6 | Self::Clr6 => Some(6),
			Self::Bbc7 | Self::Bbs7 | Self::Set7 | Self::Clr7 => Some(7),
			_ => None,
		}
	}
}

impl Display for Mnemonic {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		f.pad(&self.variant_name().to_uppercase())
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

	/// Return the bit index in this addressing mode, if there is any.
	#[must_use]
	pub const fn bit_index(&self) -> Option<u8> {
		match self {
			Self::Immediate(_)
			| Self::IndirectX
			| Self::IndirectY
			| Self::IndirectXAutoIncrement
			| Self::DirectPage(_)
			| Self::DirectPageXIndexed(_)
			| Self::DirectPageYIndexed(_)
			| Self::Address(_)
			| Self::XIndexed(_)
			| Self::YIndexed(_)
			| Self::DirectPageXIndexedIndirect(_)
			| Self::CarryFlag
			| Self::Register(_)
			| Self::DirectPageIndirectYIndexed(_) => None,
			Self::DirectPageBit(_, bit_index)
			| Self::AddressBit(_, bit_index)
			| Self::NegatedAddressBit(_, bit_index) => Some(*bit_index),
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

	/// Optimize any numbers in this addressing mode as far as possible, including the removal of references. This
	/// simplifies and improves later optimization steps.
	#[must_use]
	pub fn optimize_numbers(self) -> Self {
		match self {
			Self::Immediate(number) => Self::Immediate(number.try_resolve()),
			Self::DirectPage(number) => Self::DirectPage(number.try_resolve()),
			Self::DirectPageXIndexed(number) => Self::DirectPageXIndexed(number.try_resolve()),
			Self::DirectPageYIndexed(number) => Self::DirectPageYIndexed(number.try_resolve()),
			Self::Address(number) => Self::Address(number.try_resolve()),
			Self::XIndexed(number) => Self::XIndexed(number.try_resolve()),
			Self::YIndexed(number) => Self::YIndexed(number.try_resolve()),
			Self::DirectPageXIndexedIndirect(number) => Self::DirectPageXIndexedIndirect(number.try_resolve()),
			Self::DirectPageIndirectYIndexed(number) => Self::DirectPageIndirectYIndexed(number.try_resolve()),
			Self::DirectPageBit(number, bit) => Self::DirectPageBit(number.try_resolve(), bit),
			Self::AddressBit(number, bit) => Self::AddressBit(number.try_resolve(), bit),
			Self::NegatedAddressBit(number, bit) => Self::NegatedAddressBit(number.try_resolve(), bit),
			Self::Register(_) | Self::IndirectY | Self::IndirectX | Self::IndirectXAutoIncrement | Self::CarryFlag =>
				self,
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

impl ReferenceResolvable for AddressingMode {
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

	fn resolve_relative_labels(
		&mut self,
		direction: reference::RelativeReferenceDirection,
		relative_labels: &std::collections::HashMap<std::num::NonZeroU64, Arc<RefCell<Label>>>,
	) {
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
			| AddressingMode::NegatedAddressBit(number, _) => number.resolve_relative_labels(direction, relative_labels),
			AddressingMode::IndirectX
			| AddressingMode::IndirectY
			| AddressingMode::IndirectXAutoIncrement
			| AddressingMode::CarryFlag
			| AddressingMode::Register(_) => (),
		}
	}

	/// Set this global label as the parent for all the unresolved local labels.
	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RefCell<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.number_mut().map_or_else(|| Ok(()), |number| number.set_current_label(current_label, source_code))
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

/// The categories of addressing modes. This enum is mainly used for the main assembler lookup table to discern which
/// opcode to use. It is mostly just a list of the variants of the `AddressingMode` sum type, but it also factors out
/// all register addressing modes separately, as that is very important for opcode generation.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum AddressingModeCategory {
	/// #immediate
	Immediate,
	/// (X)
	IndirectX,
	/// (Y)
	IndirectY,
	/// (X) with automatic X++
	IndirectXAutoIncrement,
	/// (dp)
	DirectPage,
	/// dp+X
	DirectPageXIndexed,
	/// dp+Y
	DirectPageYIndexed,
	/// abs
	Address,
	/// abs+X
	XIndexed,
	/// abs+Y
	YIndexed,
	/// (dp+X)
	DirectPageXIndexedIndirect,
	/// (dp)+Y
	DirectPageIndirectYIndexed,
	/// dp.bit
	DirectPageBit,
	/// abs.bit
	AddressBit,
	/// /abs.bit
	NegatedAddressBit,
	/// C
	CarryFlag,
	/// A
	ARegister,
	/// X
	XRegister,
	/// Y
	YRegister,
	/// YA
	YARegister,
	/// PSW
	FlagsRegister,
	/// SP
	StackPointerRegister,
}

impl From<&AddressingMode> for AddressingModeCategory {
	fn from(value: &AddressingMode) -> Self {
		match value {
			AddressingMode::Immediate(_) => Self::Immediate,
			AddressingMode::IndirectX => Self::IndirectX,
			AddressingMode::IndirectY => Self::IndirectY,
			AddressingMode::IndirectXAutoIncrement => Self::IndirectXAutoIncrement,
			AddressingMode::DirectPage(_) => Self::DirectPage,
			AddressingMode::DirectPageXIndexed(_) => Self::DirectPageXIndexed,
			AddressingMode::DirectPageYIndexed(_) => Self::DirectPageYIndexed,
			AddressingMode::Address(_) => Self::Address,
			AddressingMode::XIndexed(_) => Self::XIndexed,
			AddressingMode::YIndexed(_) => Self::YIndexed,
			AddressingMode::DirectPageXIndexedIndirect(_) => Self::DirectPageXIndexedIndirect,
			AddressingMode::DirectPageIndirectYIndexed(_) => Self::DirectPageIndirectYIndexed,
			AddressingMode::DirectPageBit(_, _) => Self::DirectPageBit,
			AddressingMode::AddressBit(_, _) => Self::AddressBit,
			AddressingMode::NegatedAddressBit(_, _) => Self::NegatedAddressBit,
			AddressingMode::CarryFlag | AddressingMode::Register(Register::C) => Self::CarryFlag,
			AddressingMode::Register(Register::A) => Self::ARegister,
			AddressingMode::Register(Register::X) => Self::XRegister,
			AddressingMode::Register(Register::Y) => Self::YRegister,
			AddressingMode::Register(Register::YA) => Self::YARegister,
			AddressingMode::Register(Register::SP) => Self::StackPointerRegister,
			AddressingMode::Register(Register::PSW | Register::P) => Self::FlagsRegister,
		}
	}
}

impl Display for AddressingModeCategory {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		f.pad(match self {
			Self::Immediate => "#immediate",
			Self::IndirectX => "(X)",
			Self::IndirectY => "(Y)",
			Self::CarryFlag => "C",
			Self::IndirectXAutoIncrement => "(X)+",
			Self::DirectPage => "direct_page",
			Self::DirectPageXIndexed => "direct_page+X",
			Self::DirectPageYIndexed => "direct_page+Y",
			Self::DirectPageXIndexedIndirect => "(direct_page+X)",
			Self::DirectPageIndirectYIndexed => "(direct_page)+Y",
			Self::Address => "address",
			Self::XIndexed => "address+X",
			Self::YIndexed => "address+Y",
			Self::DirectPageBit => "direct_page.bit",
			Self::AddressBit => "address.bit",
			Self::NegatedAddressBit => "/address.bit",
			Self::ARegister => "A",
			Self::XRegister => "X",
			Self::YRegister => "Y",
			Self::YARegister => "YA",
			Self::FlagsRegister => "PSW",
			Self::StackPointerRegister => "SP",
		})
	}
}
