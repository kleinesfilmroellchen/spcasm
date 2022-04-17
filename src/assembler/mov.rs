//! Assembling the MOV instruction.
use std::rc::Rc;

use crate::assembler::AssembledData;
use crate::lexer::Register;
use crate::parser::{AddressingMode, Label};

#[allow(clippy::too_many_lines)] //heh
pub(super) fn assemble_mov(
	data: &mut AssembledData,
	target: AddressingMode,
	source: AddressingMode,
	label: Option<Rc<Label>>,
) -> Result<(), String> {
	match target {
		AddressingMode::Register(Register::A) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0xE8, value, label),
			AddressingMode::IndirectX => data.append(0xE6, label),
			AddressingMode::IndirectXAutoIncrement => data.append(0xBF, label),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xE4, page_address, label),
			AddressingMode::DirectPageXIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF4, page_base_address, label),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(0xE5, address, label),
			AddressingMode::XIndexed(address) => data.append_instruction_with_16_bit_operand(0xF5, address, label),
			AddressingMode::YIndexed(address) => data.append_instruction_with_16_bit_operand(0xF6, address, label),
			AddressingMode::DirectPageXIndexedIndirect(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xE7, page_base_address, label),
			AddressingMode::DirectPageIndirectYIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF7, page_base_address, label),
			AddressingMode::Register(Register::X) => data.append(0x7D, label),
			AddressingMode::Register(Register::Y) => data.append(0xDD, label),
			mode => return Err(format!("Unsupported `MOV A,` addressing mode {:?}", mode)),
		},
		AddressingMode::Register(Register::X) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0xCD, value, label),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF8, page_address, label),
			AddressingMode::DirectPageYIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF9, page_address, label),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(0xE9, address, label),
			AddressingMode::Register(Register::A) => data.append(0x5D, label),
			AddressingMode::Register(Register::SP) => data.append(0x9D, label),
			mode => return Err(format!("Unsupported `MOV X,` addressing mode {:?}", mode)),
		},
		AddressingMode::Register(Register::Y) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0x8D, value, label),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xEB, page_address, label),
			AddressingMode::DirectPageXIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xFB, page_address, label),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(0xEC, address, label),
			AddressingMode::Register(Register::A) => data.append(0xFD, label),
			mode => return Err(format!("Unsupported `MOV Y,` addressing mode {:?}", mode)),
		},
		AddressingMode::Register(Register::SP) =>
			if source == AddressingMode::Register(Register::X) {
				data.append(0xBD, label);
			} else {
				return Err(format!("Unsupported `MOV SP,` addressing mode {:?}", source));
			},
		AddressingMode::IndirectX => match source {
			AddressingMode::Register(Register::A) => data.append(0xC6, label),
			_ => return Err("`MOV (X),` is only supported with A".to_owned()),
		},
		AddressingMode::IndirectXAutoIncrement => match source {
			AddressingMode::Register(Register::A) => data.append(0xAF, label),
			_ => return Err("`MOV (X)+,` is only supported with A".to_owned()),
		},
		AddressingMode::DirectPage(page_address) => match source {
			AddressingMode::Register(Register::A) => data.append_instruction_with_8_bit_operand(0xC4, page_address, label),
			AddressingMode::Register(Register::X) => data.append_instruction_with_8_bit_operand(0xD8, page_address, label),
			AddressingMode::Register(Register::Y) => data.append_instruction_with_8_bit_operand(0xCB, page_address, label),
			AddressingMode::DirectPage(source_address) =>
				data.append_instruction_with_two_8_bit_operands(0xFA, page_address, source_address, label),
			AddressingMode::Immediate(literal_value) =>
				data.append_instruction_with_two_8_bit_operands(0x8F, page_address, literal_value, label),
			mode => return Err(format!("Unsupported `MOV dp` addressing mode {:?}", mode)),
		},
		AddressingMode::DirectPageXIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xD4,
				AddressingMode::Register(Register::Y) => 0xDB,
				mode => return Err(format!("Unsupported `MOV (dp+X)` addressing mode {:?}", mode)),
			},
			page_address,
			label,
		),
		AddressingMode::DirectPageYIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::X) => 0xD9,
				mode => return Err(format!("Unsupported `MOV (dp)+Y` addressing mode {:?}", mode)),
			},
			page_address,
			label,
		),
		AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xC5,
				AddressingMode::Register(Register::X) => 0xC9,
				AddressingMode::Register(Register::Y) => 0xCC,
				mode => return Err(format!("Unsupported `MOV addr` addressing mode {:?}", mode)),
			},
			address,
			label,
		),
		AddressingMode::XIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD5, address, label);
			} else {
				return Err(format!("Unsupported `MOV addr+X` addressing mode {:?}", source));
			},
		AddressingMode::YIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD6, address, label);
			} else {
				return Err(format!("Unsupported `MOV addr+Y` addressing mode {:?}", source));
			},
		AddressingMode::DirectPageXIndexedIndirect(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xC7, page_address, label);
			} else {
				return Err(format!("Unsupported `MOV (dp+X)` addressing mode {:?}", source));
			},
		AddressingMode::DirectPageIndirectYIndexed(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xD7, page_address, label);
			} else {
				return Err(format!("Unsupported `MOV (dp)+Y` addressing mode {:?}", source));
			},
		_ => unimplemented!(),
	}
	Ok(())
}
