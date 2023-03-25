//! Assembler lookup table.
//!
//! This module mainly provides the ``assembly_table`` data structure.
#![allow(clippy::module_name_repetitions, clippy::unnecessary_wraps, clippy::needless_pass_by_value)]

use std::collections::HashMap;

use miette::SourceSpan;
#[allow(unused)]
use smartstring::alias::String;

use super::AssembledData;
use crate::parser::instruction::{AddressingModeCategory, Mnemonic};
use crate::parser::AssemblyTimeValue;
use crate::AssemblyError;

/// An action of what to do additionally with the segment. This function handles two operands, such as addresses.
/// Because of synchronization constraints, these have to be function pointers. All required actions are defined as
/// functions within this module.
type TwoOperandSegmentAction = fn(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	second: &AssemblyTimeValue,
	bit_index: u8,
) -> Result<(), Box<AssemblyError>>;

/// An action of what to do additionally with the segment. This function handles one operand, such as an address.
/// Because of synchronization constraints, these have to be function pointers. All required actions are defined as
/// functions within this module.
type SingleOperandSegmentAction = fn(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	bit_index: u8,
) -> Result<(), Box<AssemblyError>>;

/// The most deeply-nested table that maps a mnemonic and two operands to the corresponding opcode and action.
type TwoOperandTable = HashMap<AddressingModeCategory, TwoOperandEntry>;

/// Either a direct table entry (for instructions that take no first operand), or a lookup table for the first operand.
pub enum EntryOrFirstOperandTable {
	/// A direct table entry; the instruction takes no operands at all.
	Entry(u8),
	/// A lookup table for the first operand.
	Table(HashMap<AddressingModeCategory, EntryOrSecondOperandTable>),
}

impl<const N: usize> From<[(AddressingModeCategory, EntryOrSecondOperandTable); N]> for EntryOrFirstOperandTable {
	fn from(value: [(AddressingModeCategory, EntryOrSecondOperandTable); N]) -> Self {
		Self::Table(HashMap::from(value))
	}
}

/// Either a direct table entry (for instructions that only take a first operand, but no second operand), or a lookup
/// table for the second operand.
pub enum EntryOrSecondOperandTable {
	/// A direct table entry; the instruction takes no second operand.
	Entry(u8, SingleOperandSegmentAction),
	/// A direct table entry; the instruction allows an extra "A" as second operand.
	ImplicitAEntry(u8, SingleOperandSegmentAction),
	/// A direct table entry, but the opcode also takes a bit index in the highest three bits. This is filled in by the
	/// A direct table entry, but the opcode also takes a bit index in the highest three bits. This is filled in by the
	/// driver code; the table just provides the opcode and extra operands.
	BitEntry(u8, SingleOperandSegmentAction),
	/// A direct table entry for tcall, where the operand is put into the upper nybble of the opcode.
	TcallEntry(u8),
	/// A lookup table for the second operand; the entry always has to be a final table entry.
	Table(TwoOperandTable),
}

impl<const N: usize> From<[(AddressingModeCategory, (u8, TwoOperandSegmentAction)); N]> for EntryOrSecondOperandTable {
	fn from(value: [(AddressingModeCategory, (u8, TwoOperandSegmentAction)); N]) -> Self {
		Self::Table(TwoOperandTable::from(
			value.map(|(category, (op, action))| (category, TwoOperandEntry::Entry(op, action))),
		))
	}
}

impl<const N: usize> From<[(AddressingModeCategory, TwoOperandEntry); N]> for EntryOrSecondOperandTable {
	fn from(value: [(AddressingModeCategory, TwoOperandEntry); N]) -> Self {
		Self::Table(TwoOperandTable::from(value))
	}
}

/// An entry for two operand instructions.
pub enum TwoOperandEntry {
	/// A normal entry with an opcode and an action.
	Entry(u8, TwoOperandSegmentAction),
	/// An entry where the opcode also takes a bit index in the highest three bits. This is filled in by the
	/// driver code; the table just provides the opcode and extra operands.
	BitEntry(u8, TwoOperandSegmentAction),
}

type Table = HashMap<Mnemonic, EntryOrFirstOperandTable>;

lazy_static! {
	/// The `assembly_table` static data structure contains mappings from mnemonics and operands to the assembled data. This
	/// data is given in the form of functions executed on a `Segments` instance.
	pub static ref assembly_table: Table = {
		let mut table = HashMap::new();

		table.insert(Mnemonic::Mov, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0xE8, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::IndirectX, (0xE6, two_operand_nop)),
				(AddressingModeCategory::IndirectXAutoIncrement, (0xBF, two_operand_nop)),
				(AddressingModeCategory::DirectPage, (0xE4, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0xF4, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0xE5, append_second_as_16_bits)),
				(AddressingModeCategory::XIndexed, (0xF5, append_second_as_16_bits)),
				(AddressingModeCategory::YIndexed, (0xF6, append_second_as_16_bits)),
				(AddressingModeCategory::DirectPageXIndexedIndirect, (0xE7, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageIndirectYIndexed, (0xF7, append_second_as_8_bits)),
				(AddressingModeCategory::XRegister, (0x7D, two_operand_nop)),
				(AddressingModeCategory::YRegister, (0xDD, two_operand_nop)),
			])),

			(AddressingModeCategory::XRegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0xCD, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::DirectPage, (0xF8, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageYIndexed, (0xF9, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0xE9, append_second_as_16_bits)),
				(AddressingModeCategory::ARegister, (0x5D, two_operand_nop)),
				(AddressingModeCategory::StackPointerRegister, (0x9D, two_operand_nop)),
			])),

			(AddressingModeCategory::YRegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0x8D, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::DirectPage, (0xEB, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0xFB, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0xEC, append_second_as_16_bits)),
				(AddressingModeCategory::ARegister, (0xFD, two_operand_nop)),
			])),

			(AddressingModeCategory::IndirectX, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xC6, two_operand_nop as TwoOperandSegmentAction)),
			])),

			(AddressingModeCategory::IndirectXAutoIncrement, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xAF, two_operand_nop as TwoOperandSegmentAction)),
			])),

			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xC4, append_first_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::XRegister, (0xD8, append_first_as_8_bits)),
				(AddressingModeCategory::YRegister, (0xCB, append_first_as_8_bits)),
				(AddressingModeCategory::DirectPage, (0xFA, append_both_as_8_bits)),
				(AddressingModeCategory::Immediate, (0x8F, append_both_as_8_bits)),
			])),

			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xD4, append_first_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::YRegister, (0xDB, append_first_as_8_bits)),
			])),

			(AddressingModeCategory::DirectPageYIndexed, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::XRegister, (0xD9, append_first_as_8_bits as TwoOperandSegmentAction)),
			])),

			(AddressingModeCategory::Address, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xC5, append_first_as_16_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::XRegister, (0xC9, append_first_as_16_bits)),
				(AddressingModeCategory::YRegister, (0xCC, append_first_as_16_bits)),
			])),

			(AddressingModeCategory::XIndexed, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xD5, append_first_as_16_bits as TwoOperandSegmentAction)),
			])),

			(AddressingModeCategory::YIndexed, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xD6, append_first_as_16_bits as TwoOperandSegmentAction)),
			])),

			(AddressingModeCategory::DirectPageXIndexedIndirect, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xC7, append_first_as_8_bits as TwoOperandSegmentAction)),
			])),

			(AddressingModeCategory::DirectPageIndirectYIndexed, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::ARegister, (0xD7, append_first_as_8_bits as TwoOperandSegmentAction)),
			])),

			(AddressingModeCategory::StackPointerRegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::XRegister, (0xBD, two_operand_nop as TwoOperandSegmentAction)),
			])),
		]));

		table.insert(Mnemonic::Adc, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0x88, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::IndirectX, (0x86, two_operand_nop)),
				(AddressingModeCategory::DirectPage, (0x84, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0x94, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0x85, append_second_as_16_bits)),
				(AddressingModeCategory::XIndexed, (0x95, append_second_as_16_bits)),
				(AddressingModeCategory::YIndexed, (0x96, append_second_as_16_bits)),
				(AddressingModeCategory::DirectPageXIndexedIndirect, (0x87, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageIndirectYIndexed, (0x97, append_second_as_8_bits)),
			])),
			(AddressingModeCategory::IndirectX, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::IndirectY, (0x99, two_operand_nop as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x89, append_both_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Immediate, (0x98, append_both_as_8_bits)),
			])),
		]));

		table.insert(Mnemonic::Sbc, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0xA8, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::IndirectX, (0xA6, two_operand_nop)),
				(AddressingModeCategory::DirectPage, (0xA4, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0xB4, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0xA5, append_second_as_16_bits)),
				(AddressingModeCategory::XIndexed, (0xB5, append_second_as_16_bits)),
				(AddressingModeCategory::YIndexed, (0xB6, append_second_as_16_bits)),
				(AddressingModeCategory::DirectPageXIndexedIndirect, (0xA7, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageIndirectYIndexed, (0xB7, append_second_as_8_bits)),
			])),
			(AddressingModeCategory::IndirectX, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::IndirectY, (0xB9, two_operand_nop as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0xA9, append_both_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Immediate, (0xB8, append_both_as_8_bits)),
			])),
		]));

		table.insert(Mnemonic::Cmp, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0x68, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::IndirectX, (0x66, two_operand_nop)),
				(AddressingModeCategory::DirectPage, (0x64, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0x74, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0x65, append_second_as_16_bits)),
				(AddressingModeCategory::XIndexed, (0x75, append_second_as_16_bits)),
				(AddressingModeCategory::YIndexed, (0x76, append_second_as_16_bits)),
				(AddressingModeCategory::DirectPageXIndexedIndirect, (0x67, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageIndirectYIndexed, (0x77, append_second_as_8_bits)),
			])),
			(AddressingModeCategory::IndirectX, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::IndirectY, (0x79, two_operand_nop as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x69, append_both_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Immediate, (0x78, append_both_as_8_bits)),
			])),
			(AddressingModeCategory::XRegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0xC8, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::DirectPage, (0x3E, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0x1E, append_second_as_16_bits)),
			])),
			(AddressingModeCategory::YRegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0xAD, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::DirectPage, (0x7E, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0x5E, append_second_as_16_bits)),
			])),
		]));

		table.insert(Mnemonic::And, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0x28, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::IndirectX, (0x26, two_operand_nop)),
				(AddressingModeCategory::DirectPage, (0x24, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0x34, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0x25, append_second_as_16_bits)),
				(AddressingModeCategory::XIndexed, (0x35, append_second_as_16_bits)),
				(AddressingModeCategory::YIndexed, (0x36, append_second_as_16_bits)),
				(AddressingModeCategory::DirectPageXIndexedIndirect, (0x27, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageIndirectYIndexed, (0x37, append_second_as_8_bits)),
			])),
			(AddressingModeCategory::IndirectX, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::IndirectY, (0x39, two_operand_nop as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x29, append_both_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Immediate, (0x38, append_both_as_8_bits)),
			])),
		]));

		table.insert(Mnemonic::Or, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0x08, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::IndirectX, (0x06, two_operand_nop)),
				(AddressingModeCategory::DirectPage, (0x04, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0x14, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0x05, append_second_as_16_bits)),
				(AddressingModeCategory::XIndexed, (0x15, append_second_as_16_bits)),
				(AddressingModeCategory::YIndexed, (0x16, append_second_as_16_bits)),
				(AddressingModeCategory::DirectPageXIndexedIndirect, (0x07, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageIndirectYIndexed, (0x17, append_second_as_8_bits)),
			])),
			(AddressingModeCategory::IndirectX, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::IndirectY, (0x19, two_operand_nop as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x09, append_both_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Immediate, (0x18, append_both_as_8_bits)),
			])),
		]));

		table.insert(Mnemonic::Eor, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::Immediate, (0x48, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::IndirectX, (0x46, two_operand_nop)),
				(AddressingModeCategory::DirectPage, (0x44, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageXIndexed, (0x54, append_second_as_8_bits)),
				(AddressingModeCategory::Address, (0x45, append_second_as_16_bits)),
				(AddressingModeCategory::XIndexed, (0x55, append_second_as_16_bits)),
				(AddressingModeCategory::YIndexed, (0x56, append_second_as_16_bits)),
				(AddressingModeCategory::DirectPageXIndexedIndirect, (0x47, append_second_as_8_bits)),
				(AddressingModeCategory::DirectPageIndirectYIndexed, (0x57, append_second_as_8_bits)),
			])),
			(AddressingModeCategory::IndirectX, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::IndirectY, (0x59, two_operand_nop as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x49, append_both_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Immediate, (0x58, append_both_as_8_bits)),
			])),
		]));

		table.insert(Mnemonic::Inc, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0xBC, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0xAB, append_one_as_8_bits)),
			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::Entry(0xBB, append_one_as_8_bits)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0xAC, append_one_as_16_bits)),
			(AddressingModeCategory::XRegister, EntryOrSecondOperandTable::Entry(0x3D, one_operand_nop)),
			(AddressingModeCategory::YRegister, EntryOrSecondOperandTable::Entry(0xFC, one_operand_nop)),
		]));

		table.insert(Mnemonic::Dec, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0x9C, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x8B, append_one_as_8_bits)),
			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::Entry(0x9B, append_one_as_8_bits)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x8C, append_one_as_16_bits)),
			(AddressingModeCategory::XRegister, EntryOrSecondOperandTable::Entry(0x1D, one_operand_nop)),
			(AddressingModeCategory::YRegister, EntryOrSecondOperandTable::Entry(0xDC, one_operand_nop)),
		]));

		table.insert(Mnemonic::Asl, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0x1C, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x0B, append_one_as_8_bits)),
			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::Entry(0x1B, append_one_as_8_bits)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0xCC, append_one_as_16_bits)),
		]));

		table.insert(Mnemonic::Lsr, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0x5C, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x4B, append_one_as_8_bits)),
			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::Entry(0x5B, append_one_as_8_bits)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x4C, append_one_as_16_bits)),
		]));

		table.insert(Mnemonic::Rol, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0x3C, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x2B, append_one_as_8_bits)),
			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::Entry(0x3B, append_one_as_8_bits)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x2C, append_one_as_16_bits)),
		]));

		table.insert(Mnemonic::Ror, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0x7C, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x6B, append_one_as_8_bits)),
			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::Entry(0x7B, append_one_as_8_bits)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x6C, append_one_as_16_bits)),
		]));

		table.insert(Mnemonic::Xcn, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0x9F, one_operand_nop as SingleOperandSegmentAction)),
		]));

		table.insert(Mnemonic::Movw, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::YARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0xBA, append_second_as_8_bits as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::YARegister, (0xDA, append_first_as_8_bits as TwoOperandSegmentAction)),
			])),
		]));

		table.insert(Mnemonic::Incw, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x3A, append_one_as_8_bits as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Decw, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x1A, append_one_as_8_bits as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Addw, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::YARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x7A, append_second_as_8_bits as TwoOperandSegmentAction)),
			])),
		]));
		table.insert(Mnemonic::Subw, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::YARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x9A, append_second_as_8_bits as TwoOperandSegmentAction)),
			])),
		]));
		table.insert(Mnemonic::Cmpw, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::YARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x5A, append_second_as_8_bits as TwoOperandSegmentAction)),
			])),
		]));

		table.insert(Mnemonic::Mul, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::YARegister, EntryOrSecondOperandTable::Entry(0xCF, one_operand_nop as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Div, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::YARegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::XRegister, (0x9E, two_operand_nop as TwoOperandSegmentAction)),
			])),
		]));

		table.insert(Mnemonic::Daa, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0xDF, one_operand_nop as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Das, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0xBE, one_operand_nop as SingleOperandSegmentAction)),
		]));

		table.insert(Mnemonic::Bra, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x2F, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x2F, append_one_relative)),
		]));
		table.insert(Mnemonic::Beq, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0xF0, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0xF0, append_one_relative)),
		]));
		table.insert(Mnemonic::Bne, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0xD0, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0xD0, append_one_relative)),
		]));
		table.insert(Mnemonic::Bcs, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0xB0, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0xB0, append_one_relative)),
		]));
		table.insert(Mnemonic::Bcc, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x90, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x90, append_one_relative)),
		]));
		table.insert(Mnemonic::Bvs, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x70, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x70, append_one_relative)),
		]));
		table.insert(Mnemonic::Bvc, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x50, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x50, append_one_relative)),
		]));
		table.insert(Mnemonic::Bmi, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x30, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x30, append_one_relative)),
		]));
		table.insert(Mnemonic::Bpl, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x10, append_one_relative as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x10, append_one_relative)),
		]));

		table.insert(Mnemonic::Bbs, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPageBit, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, TwoOperandEntry::BitEntry(0x03, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, TwoOperandEntry::BitEntry(0x03, append_both_reversed_as_8_bits)),
			])),
		]));

		table.insert(Mnemonic::Bbc, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPageBit, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, TwoOperandEntry::BitEntry(0x13, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, TwoOperandEntry::BitEntry(0x13, append_both_reversed_as_8_bits)),
			])),
		]));

		table.insert(Mnemonic::Cbne, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x2E, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, (0x2E, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::DirectPageXIndexed, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0xDE, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, (0xDE, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
			])),
		]));

		table.insert(Mnemonic::Dbnz, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0x6E, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, (0x6E, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::YRegister, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::DirectPage, (0xFE, append_second_as_8_bits as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, (0x6E, append_both_reversed_as_8_bits as TwoOperandSegmentAction)),
			])),
		]));

		table.insert(Mnemonic::Jmp, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x5F, append_one_as_16_bits as SingleOperandSegmentAction)),
			(AddressingModeCategory::DirectPageXIndexedIndirect, EntryOrSecondOperandTable::Entry(0x1F, append_one_as_16_bits)),
			(AddressingModeCategory::XIndexed, EntryOrSecondOperandTable::Entry(0x1F, append_one_as_16_bits)),
		]));

		table.insert(Mnemonic::Call, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0x3F, append_one_as_16_bits as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Pcall, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::Entry(0x4F, append_one_as_8_bits as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Tcall, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPage, EntryOrSecondOperandTable::TcallEntry(0x01)),
		]));

		table.insert(Mnemonic::Brk, EntryOrFirstOperandTable::Entry(0x0F));
		table.insert(Mnemonic::Ret, EntryOrFirstOperandTable::Entry(0x6F));
		table.insert(Mnemonic::Ret1, EntryOrFirstOperandTable::Entry(0x7F));

		table.insert(Mnemonic::Push, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0x2D, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::XRegister, EntryOrSecondOperandTable::Entry(0x4D, one_operand_nop)),
			(AddressingModeCategory::YRegister, EntryOrSecondOperandTable::Entry(0x6D, one_operand_nop)),
			(AddressingModeCategory::FlagsRegister, EntryOrSecondOperandTable::Entry(0x0D, one_operand_nop)),
		]));

		table.insert(Mnemonic::Pop, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::ARegister, EntryOrSecondOperandTable::Entry(0xAE, one_operand_nop as SingleOperandSegmentAction)),
			(AddressingModeCategory::XRegister, EntryOrSecondOperandTable::Entry(0xCE, one_operand_nop)),
			(AddressingModeCategory::YRegister, EntryOrSecondOperandTable::Entry(0xEE, one_operand_nop)),
			(AddressingModeCategory::FlagsRegister, EntryOrSecondOperandTable::Entry(0x8E, one_operand_nop)),
		]));

		table.insert(Mnemonic::Set1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPageBit, EntryOrSecondOperandTable::BitEntry(0x02, append_one_as_8_bits as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Clr1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::DirectPageBit, EntryOrSecondOperandTable::BitEntry(0x12, append_one_as_8_bits as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Tset1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::ImplicitAEntry(0x0E, append_one_as_16_bits as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Tclr1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::ImplicitAEntry(0x4E, append_one_as_16_bits as SingleOperandSegmentAction)),
		]));

		table.insert(Mnemonic::And1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::CarryFlag, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::AddressBit, TwoOperandEntry::Entry(0x4A, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, TwoOperandEntry::Entry(0x4A, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
				(AddressingModeCategory::NegatedAddressBit, TwoOperandEntry::Entry(0x6A, append_second_as_16_bits_and_bit_index)),
			])),
		]));
		table.insert(Mnemonic::Or1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::CarryFlag, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::AddressBit, TwoOperandEntry::Entry(0x0A, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, TwoOperandEntry::Entry(0x0A, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
				(AddressingModeCategory::NegatedAddressBit, TwoOperandEntry::Entry(0x2A, append_second_as_16_bits_and_bit_index)),
			])),
		]));
		table.insert(Mnemonic::Eor1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::CarryFlag, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::AddressBit, TwoOperandEntry::Entry(0x8A, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, TwoOperandEntry::Entry(0x8A, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
			])),
		]));
		table.insert(Mnemonic::Not1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::AddressBit, EntryOrSecondOperandTable::Entry(0xEA, append_one_as_16_bits_and_bit_index as SingleOperandSegmentAction)),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::Entry(0xEA, append_one_as_16_bits_and_bit_index as SingleOperandSegmentAction)),
		]));
		table.insert(Mnemonic::Mov1, EntryOrFirstOperandTable::from([
			(AddressingModeCategory::CarryFlag, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::AddressBit, TwoOperandEntry::Entry(0xAA, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
				(AddressingModeCategory::Address, TwoOperandEntry::Entry(0xAA, append_second_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::AddressBit, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::CarryFlag, TwoOperandEntry::Entry(0xCA, append_first_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
			])),
			(AddressingModeCategory::Address, EntryOrSecondOperandTable::from([
				(AddressingModeCategory::CarryFlag, TwoOperandEntry::Entry(0xCA, append_first_as_16_bits_and_bit_index as TwoOperandSegmentAction)),
			])),
		]));

		table.insert(Mnemonic::Clrc, EntryOrFirstOperandTable::Entry(0x60));
		table.insert(Mnemonic::Setc, EntryOrFirstOperandTable::Entry(0x80));
		table.insert(Mnemonic::Notc, EntryOrFirstOperandTable::Entry(0xED));
		table.insert(Mnemonic::Clrv, EntryOrFirstOperandTable::Entry(0xE0));
		table.insert(Mnemonic::Clrp, EntryOrFirstOperandTable::Entry(0x20));
		table.insert(Mnemonic::Setp, EntryOrFirstOperandTable::Entry(0x40));
		table.insert(Mnemonic::Ei, EntryOrFirstOperandTable::Entry(0xA0));
		table.insert(Mnemonic::Di, EntryOrFirstOperandTable::Entry(0xC0));

		table.insert(Mnemonic::Nop, EntryOrFirstOperandTable::Entry(0x00));
		table.insert(Mnemonic::Sleep, EntryOrFirstOperandTable::Entry(0xEF));
		table.insert(Mnemonic::Stop, EntryOrFirstOperandTable::Entry(0xFF));

		table
	};
}

fn one_operand_nop(
	_data: &mut AssembledData,
	_span: SourceSpan,
	_first: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	Ok(())
}

fn two_operand_nop(
	_data: &mut AssembledData,
	_span: SourceSpan,
	_first: &AssemblyTimeValue,
	_second: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	Ok(())
}

fn append_one_as_8_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(first.clone(), 0, None, span)
}

fn append_one_relative(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_relative_unresolved(first.clone(), span)
}

fn append_one_as_16_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_16_bits_unresolved(first.clone(), None, span)
}

fn append_one_as_16_bits_and_bit_index(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(first.clone(), 0, None, span)?;
	data.append_unresolved_with_high_byte_bit_index(first.clone(), bit_index, span)
}

fn append_first_as_8_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	_second: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(first.clone(), 0, None, span)
}

fn append_second_as_8_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	_first: &AssemblyTimeValue,
	second: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(second.clone(), 0, None, span)
}

/// This is for most normal instructions which have their assembly operands in reverse order from machine code.
fn append_both_as_8_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	second: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(second.clone(), 0, None, span)?;
	data.append_8_bits_unresolved(first.clone(), 0, None, span)
}

/// This is for only BBS/BBC which have their assembly operands in the same order as machine code.
fn append_both_reversed_as_8_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	second: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(first.clone(), 0, None, span)?;
	data.append_8_bits_unresolved(second.clone(), 0, None, span)
}

fn append_first_as_16_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	_second: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_16_bits_unresolved(first.clone(), None, span)
}

fn append_second_as_16_bits(
	data: &mut AssembledData,
	span: SourceSpan,
	_first: &AssemblyTimeValue,
	second: &AssemblyTimeValue,
	_bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_16_bits_unresolved(second.clone(), None, span)
}

fn append_second_as_16_bits_and_bit_index(
	data: &mut AssembledData,
	span: SourceSpan,
	_first: &AssemblyTimeValue,
	second: &AssemblyTimeValue,
	bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(second.clone(), 0, None, span)?;
	data.append_unresolved_with_high_byte_bit_index(second.clone(), bit_index, span)
}

fn append_first_as_16_bits_and_bit_index(
	data: &mut AssembledData,
	span: SourceSpan,
	first: &AssemblyTimeValue,
	_second: &AssemblyTimeValue,
	bit_index: u8,
) -> Result<(), Box<AssemblyError>> {
	data.append_8_bits_unresolved(first.clone(), 0, None, span)?;
	data.append_unresolved_with_high_byte_bit_index(first.clone(), bit_index, span)
}
