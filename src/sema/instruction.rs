//! Instruction/AST-related structs created in the parser and consumed in the assembler.

use std::fmt::{Display, Formatter};
use std::result::Result;
use std::sync::Arc;

use miette::SourceSpan;
use parking_lot::RwLock;
#[allow(unused)]
use smartstring::alias::String;
use spcasm_derive::{Parse, VariantName};

use super::reference::{self, Label, Reference, ReferenceResolvable};
use super::register::Register;
use super::AddressingMode;
#[allow(unused)]
use crate::byte_vec_to_string;
use crate::error::AssemblyError;
use crate::sema::AssemblyTimeValue;
use crate::{span_to_string, AssemblyCode, VariantName};

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
		replacement_parent: Arc<RwLock<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.opcode.replace_macro_parent(replacement_parent, source_code)
	}

	fn resolve_relative_labels(
		&mut self,
		direction: reference::RelativeReferenceDirection,
		relative_labels: &std::collections::HashMap<std::num::NonZeroU64, Arc<RwLock<Label>>>,
	) {
		self.opcode.resolve_relative_labels(direction, relative_labels);
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		self.opcode.resolve_pseudo_labels(global_labels);
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.opcode.set_current_label(current_label, source_code)
	}
}

impl std::fmt::Display for Instruction {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}", span_to_string(self.span), self.opcode)?;
		#[cfg(test)]
		write!(f, " {}", byte_vec_to_string(&self.expected_value))?;
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
		replacement_parent: Arc<RwLock<reference::MacroParent>>,
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
		relative_labels: &std::collections::HashMap<std::num::NonZeroU64, Arc<RwLock<Label>>>,
	) {
		for operand in self.first_operand.iter_mut().chain(self.second_operand.iter_mut()) {
			operand.resolve_relative_labels(direction, relative_labels);
		}
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		for operand in self.first_operand.iter_mut().chain(self.second_operand.iter_mut()) {
			operand.resolve_pseudo_labels(global_labels);
		}
	}

	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
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
