//! Assembling the arithmetic and logic instructions.
use std::rc::Rc;

use crate::assembler::AssembledData;
use crate::lexer::Register;
use crate::parser::{AddressingMode, Label, Mnemonic};

#[allow(clippy::too_many_lines)] // ¯\_(ツ)_/¯
#[allow(clippy::needless_pass_by_value)]
pub(super) fn assemble_arithmetic_instruction(
	data: &mut AssembledData,
	mnemonic: Mnemonic,
	target: AddressingMode,
	source: AddressingMode,
	label: Option<Rc<Label>>,
) -> Result<(), String> {
	match target {
		AddressingMode::Register(Register::A) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x88,
					Mnemonic::Sbc => 0xA8,
					Mnemonic::Cmp => 0x68,
					Mnemonic::And => 0x28,
					Mnemonic::Or => 0x08,
					Mnemonic::Eor => 0x48,
					_ => unreachable!(),
				},
				value,
				label,
			),
			AddressingMode::IndirectX => data.append(
				match mnemonic {
					Mnemonic::Adc => 0x86,
					Mnemonic::Cmp => 0x66,
					Mnemonic::Sbc => 0xA6,
					Mnemonic::And => 0x26,
					Mnemonic::Or => 0x06,
					Mnemonic::Eor => 0x46,
					_ => unreachable!(),
				},
				label,
			),
			AddressingMode::DirectPage(page_address) => data.append_instruction_with_8_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x84,
					Mnemonic::Sbc => 0xA4,
					Mnemonic::Cmp => 0x64,
					Mnemonic::And => 0x24,
					Mnemonic::Or => 0x04,
					Mnemonic::Eor => 0x44,
					_ => unreachable!(),
				},
				page_address,
				label,
			),
			AddressingMode::DirectPageXIndexed(page_address) => data.append_instruction_with_8_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x94,
					Mnemonic::Sbc => 0xB4,
					Mnemonic::Cmp => 0x74,
					Mnemonic::And => 0x34,
					Mnemonic::Or => 0x14,
					Mnemonic::Eor => 0x54,
					_ => unreachable!(),
				},
				page_address,
				label,
			),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x85,
					Mnemonic::Sbc => 0xA5,
					Mnemonic::Cmp => 0x65,
					Mnemonic::And => 0x25,
					Mnemonic::Or => 0x05,
					Mnemonic::Eor => 0x45,
					_ => unreachable!(),
				},
				address,
				label,
			),
			AddressingMode::XIndexed(address) => data.append_instruction_with_16_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x95,
					Mnemonic::Sbc => 0xB5,
					Mnemonic::Cmp => 0x75,
					Mnemonic::And => 0x35,
					Mnemonic::Or => 0x15,
					Mnemonic::Eor => 0x55,
					_ => unreachable!(),
				},
				address,
				label,
			),
			AddressingMode::YIndexed(address) => data.append_instruction_with_16_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x96,
					Mnemonic::Sbc => 0xB6,
					Mnemonic::Cmp => 0x76,
					Mnemonic::And => 0x36,
					Mnemonic::Or => 0x16,
					Mnemonic::Eor => 0x56,
					_ => unreachable!(),
				},
				address,
				label,
			),
			AddressingMode::DirectPageXIndexedIndirect(page_address) => data.append_instruction_with_8_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x87,
					Mnemonic::Sbc => 0xA7,
					Mnemonic::Cmp => 0x67,
					Mnemonic::And => 0x27,
					Mnemonic::Or => 0x07,
					Mnemonic::Eor => 0x47,
					_ => unreachable!(),
				},
				page_address,
				label,
			),
			AddressingMode::DirectPageIndirectYIndexed(page_address) => data.append_instruction_with_8_bit_operand(
				match mnemonic {
					Mnemonic::Adc => 0x97,
					Mnemonic::Sbc => 0xB7,
					Mnemonic::Cmp => 0x77,
					Mnemonic::And => 0x37,
					Mnemonic::Or => 0x17,
					Mnemonic::Eor => 0x57,
					_ => unreachable!(),
				},
				page_address,
				label,
			),
			_ => return Err("unimplemented".to_owned()),
		},
		AddressingMode::IndirectX =>
			if source == AddressingMode::IndirectY {
				data.append(
					match mnemonic {
						Mnemonic::Adc => 0x99,
						Mnemonic::Sbc => 0xB9,
						Mnemonic::Cmp => 0x79,
						Mnemonic::And => 0x39,
						Mnemonic::Or => 0x19,
						Mnemonic::Eor => 0x59,
						_ => unreachable!(),
					},
					label,
				);
			} else {
				return Err(format!("Addressing mode {:?} unsupported for (X) target", source));
			},
		AddressingMode::DirectPage(page_address) => data.append_instruction_with_two_8_bit_operands(
			// TODO: different operands for two different direct page sources
			match source {
				AddressingMode::DirectPage(_) => match mnemonic {
					Mnemonic::Adc => 0x89,
					Mnemonic::Sbc => 0xA9,
					Mnemonic::Cmp => 0x69,
					Mnemonic::And => 0x29,
					Mnemonic::Or => 0x09,
					Mnemonic::Eor => 0x49,
					_ => unreachable!(),
				},
				AddressingMode::Immediate(_) => match mnemonic {
					Mnemonic::Adc => 0x98,
					Mnemonic::Sbc => 0xB8,
					Mnemonic::Cmp => 0x78,
					Mnemonic::And => 0x38,
					Mnemonic::Or => 0x18,
					Mnemonic::Eor => 0x58,
					_ => unreachable!(),
				},
				_ => return Err(format!("Addressing mode {:?} unsupported for dp target", source)),
			},
			page_address,
			match source {
				AddressingMode::DirectPage(source_data) | AddressingMode::Immediate(source_data) => source_data,
				_ => return Err(format!("Addressing mode {:?} unsupported for dp target", source)),
			},
			label,
		),
		AddressingMode::Register(register @ (Register::X | Register::Y)) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(
				match register {
					Register::X => 0xC8,
					Register::Y => 0xAD,
					_ => unreachable!(),
				},
				value,
				label,
			),
			AddressingMode::DirectPage(page_address) => data.append_instruction_with_8_bit_operand(
				match register {
					Register::X => 0x3E,
					Register::Y => 0x7E,
					_ => unreachable!(),
				},
				page_address,
				label,
			),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
				match register {
					Register::X => 0x1E,
					Register::Y => 0x5E,
					_ => unreachable!(),
				},
				address,
				label,
			),
			_ => return Err(format!("Addressing mode {:?} is invalid for `{:?} X,` or `CMP Y,`", source, mnemonic)),
		},
		_ => return Err(format!("Unsupported target addressing mode {:?} for arithmetic/logic mnemonics", target)),
	}
	Ok(())
}
