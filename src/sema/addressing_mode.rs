//! [`AddressingMode`].

use std::fmt::{Display, Formatter};
use std::result::Result;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};
use miette::SourceSpan;
use parking_lot::RwLock;

use super::reference::{self, Label, Reference, ReferenceResolvable};
use super::register::Register;
use crate::error::AssemblyError;
use crate::sema::AssemblyTimeValue;
use crate::AssemblyCode;

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
		if let Some(AssemblyTimeValue::Literal(resolved_address, span)) =
			self.number().map(AssemblyTimeValue::try_resolve)
			&& resolved_address <= 0xFF
		{
			let number = AssemblyTimeValue::Literal(resolved_address, span);
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

	/// Force this addressing mode into wide addressing (out of direct page addressing) regardless of the internal
	/// number.
	#[must_use]
	#[allow(clippy::missing_const_for_fn)] // false positive
	pub fn force_to_wide_addressing(self) -> Self {
		match self {
			Self::DirectPage(number) => Self::Address(number),
			Self::DirectPageXIndexed(number) => Self::XIndexed(number),
			Self::DirectPageYIndexed(number) => Self::YIndexed(number),
			Self::DirectPageBit(number, bit) => Self::AddressBit(number, bit),
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
			Self::IndirectX
			| Self::IndirectY
			| Self::IndirectXAutoIncrement
			| Self::CarryFlag
			| Self::Register(_) => 0,
			Self::Immediate(_)
			| Self::DirectPageXIndexedIndirect(_)
			| Self::DirectPageIndirectYIndexed(_)
			| Self::DirectPage(_)
			| Self::DirectPageXIndexed(_)
			| Self::DirectPageBit(_, _) // bit will be merged into opcode byte, so it needs no extra space
			| Self::DirectPageYIndexed(_) => 1,
			Self::AddressBit(_, _)
			| Self::NegatedAddressBit(_, _)
			| Self::Address(_)
			| Self::XIndexed(_)
			| Self::YIndexed(_) => 2,
		}
	}

	/// Returns whether this addressing mode contains a long, i.e. two-byte, address.
	#[allow(clippy::missing_const_for_fn)]
	#[must_use]
	pub fn has_long_address(self) -> bool {
		matches!(self, Self::XIndexed(..) | Self::YIndexed(..) | Self::AddressBit(..) | Self::Address(..))
	}

	pub(super) fn references(&self) -> Vec<&Reference> {
		self.number_ref().map(AssemblyTimeValue::references).unwrap_or_default()
	}
}

impl ReferenceResolvable for AddressingMode {
	fn replace_macro_parent(
		&mut self,
		replacement_parent: Arc<RwLock<reference::MacroParent>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		match self {
			Self::Immediate(number)
			| Self::DirectPage(number)
			| Self::DirectPageXIndexed(number)
			| Self::DirectPageYIndexed(number)
			| Self::Address(number)
			| Self::XIndexed(number)
			| Self::YIndexed(number)
			| Self::DirectPageXIndexedIndirect(number)
			| Self::DirectPageIndirectYIndexed(number)
			| Self::DirectPageBit(number, _)
			| Self::AddressBit(number, _)
			| Self::NegatedAddressBit(number, _) => number.replace_macro_parent(replacement_parent, source_code),
			Self::IndirectX | Self::IndirectY | Self::IndirectXAutoIncrement | Self::CarryFlag | Self::Register(_) =>
				Ok(()),
		}
	}

	fn resolve_relative_labels(
		&mut self,
		direction: reference::RelativeReferenceDirection,
		relative_labels: &std::collections::HashMap<std::num::NonZeroU64, Arc<RwLock<Label>>>,
	) {
		match self {
			Self::Immediate(number)
			| Self::DirectPage(number)
			| Self::DirectPageXIndexed(number)
			| Self::DirectPageYIndexed(number)
			| Self::Address(number)
			| Self::XIndexed(number)
			| Self::YIndexed(number)
			| Self::DirectPageXIndexedIndirect(number)
			| Self::DirectPageIndirectYIndexed(number)
			| Self::DirectPageBit(number, _)
			| Self::AddressBit(number, _)
			| Self::NegatedAddressBit(number, _) => number.resolve_relative_labels(direction, relative_labels),
			Self::IndirectX | Self::IndirectY | Self::IndirectXAutoIncrement | Self::CarryFlag | Self::Register(_) =>
				(),
		}
	}

	fn resolve_pseudo_labels(&mut self, global_labels: &[Arc<RwLock<Label>>]) {
		match self {
			Self::Immediate(number)
			| Self::DirectPage(number)
			| Self::DirectPageXIndexed(number)
			| Self::DirectPageYIndexed(number)
			| Self::Address(number)
			| Self::XIndexed(number)
			| Self::YIndexed(number)
			| Self::DirectPageXIndexedIndirect(number)
			| Self::DirectPageIndirectYIndexed(number)
			| Self::DirectPageBit(number, _)
			| Self::AddressBit(number, _)
			| Self::NegatedAddressBit(number, _) => number.resolve_pseudo_labels(global_labels),
			Self::IndirectX | Self::IndirectY | Self::IndirectXAutoIncrement | Self::CarryFlag | Self::Register(_) =>
				(),
		}
	}

	/// Set this global label as the parent for all the unresolved local labels.
	fn set_current_label(
		&mut self,
		current_label: &Option<Arc<RwLock<Label>>>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<(), Box<AssemblyError>> {
		self.number_mut().map_or_else(|| Ok(()), |number| number.set_current_label(current_label, source_code))
	}
}

impl Display for AddressingMode {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		f.pad(&match self {
			Self::Immediate(number) => format!("#{number:02X}"),
			Self::IndirectX => "(X)".to_owned(),
			Self::IndirectY => "(Y)".to_owned(),
			Self::CarryFlag => "C".to_owned(),
			Self::IndirectXAutoIncrement => "(X)+".to_owned(),
			Self::DirectPage(address) => format!("{address:02X}"),
			Self::DirectPageXIndexed(address) => format!("{address:02X}+X"),
			Self::DirectPageYIndexed(address) => format!("{address:02X}+Y"),
			Self::DirectPageXIndexedIndirect(address) => format!("({address:02X}+X)"),
			Self::DirectPageIndirectYIndexed(address) => format!("({address:02X})+Y"),
			Self::Address(address) => format!("{address:04X}"),
			Self::XIndexed(address) => format!("{address:04X}+X"),
			Self::YIndexed(address) => format!("{address:04X}+Y"),
			Self::DirectPageBit(address, bit) => format!("{address:02X}.{bit:01}"),
			Self::AddressBit(address, bit) => format!("{address:04X}.{bit:01}"),
			Self::NegatedAddressBit(address, bit) => format!("/{address:04X}.{bit:01}"),
			Self::Register(register) => format!("{register}"),
		})
	}
}

/// The categories of addressing modes.
///
/// This enum is mainly used for the main assembler lookup table to discern which opcode to use. It is mostly just a
/// list of the variants of the [`AddressingMode`] sum type, but it also factors out all register addressing modes
/// separately, as that is very important for opcode generation.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
#[allow(clippy::module_name_repetitions)]
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
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
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
