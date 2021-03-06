//! Assembling the arithmetic and logic instructions.
use crate::assembler::AssembledData;
use crate::error::AssemblyError;
use crate::instruction::{AddressingMode, Instruction, Mnemonic};
use crate::Register;

#[allow(clippy::too_many_lines)] // ¯\_(ツ)_/¯
#[allow(clippy::needless_pass_by_value)]
pub(super) fn assemble_arithmetic_instruction(
	data: &mut AssembledData,
	mnemonic: Mnemonic,
	target: AddressingMode,
	source: AddressingMode,
	instruction: &Instruction,
) -> Result<(), AssemblyError> {
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
				instruction.label.clone(),
				instruction.span,
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
				instruction.label.clone(),
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
				instruction.label.clone(),
				instruction.span,
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
				instruction.label.clone(),
				instruction.span,
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
				instruction.label.clone(),
				instruction.span,
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
				instruction.label.clone(),
				instruction.span,
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
				instruction.label.clone(),
				instruction.span,
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
				instruction.label.clone(),
				instruction.span,
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
				instruction.label.clone(),
				instruction.span,
			),
			_ =>
				return Err(AssemblyError::InvalidAddressingMode {
					mnemonic,
					is_first_operand: false,
					// TODO
					legal_modes: vec![],
					mode: source,
					location: instruction.span,
					src: data.source_code.clone(),
				}),
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
					instruction.label.clone(),
				);
			} else {
				return Err(AssemblyError::InvalidAddressingMode {
					is_first_operand: false,
					mode: source,
					mnemonic,
					src: data.source_code.clone(),
					location: instruction.span,
					// TODO
					legal_modes: vec![],
				});
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
				_ =>
					return Err(AssemblyError::InvalidAddressingMode {
						is_first_operand: false,
						mode: source,
						mnemonic,
						src: data.source_code.clone(),
						location: instruction.span,
						// TODO
						legal_modes: vec![],
					}),
			},
			page_address,
			match source {
				AddressingMode::DirectPage(source_data) | AddressingMode::Immediate(source_data) => source_data,
				_ =>
					return Err(AssemblyError::InvalidAddressingMode {
						is_first_operand: false,
						mode: source,
						mnemonic,
						src: data.source_code.clone(),
						location: instruction.span,
						// TODO
						legal_modes: vec![],
					}),
			},
			instruction.label.clone(),
			instruction.span,
		),
		AddressingMode::Register(register @ (Register::X | Register::Y)) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(
				match register {
					Register::X => 0xC8,
					Register::Y => 0xAD,
					_ => unreachable!(),
				},
				value,
				instruction.label.clone(),
				instruction.span,
			),
			AddressingMode::DirectPage(page_address) => data.append_instruction_with_8_bit_operand(
				match register {
					Register::X => 0x3E,
					Register::Y => 0x7E,
					_ => unreachable!(),
				},
				page_address,
				instruction.label.clone(),
				instruction.span,
			),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
				match register {
					Register::X => 0x1E,
					Register::Y => 0x5E,
					_ => unreachable!(),
				},
				address,
				instruction.label.clone(),
				instruction.span,
			),
			_ =>
				return Err(AssemblyError::InvalidAddressingMode {
					is_first_operand: false,
					mode: source,
					mnemonic,
					src: data.source_code.clone(),
					location: instruction.span,
					// TODO
					legal_modes: vec![],
				}),
		},
		_ =>
			return Err(AssemblyError::InvalidAddressingMode {
				is_first_operand: true,
				mode: target,
				mnemonic,
				src: data.source_code.clone(),
				location: instruction.span,
				// TODO
				legal_modes: vec![],
			}),
	}
	Ok(())
}

pub(super) fn assemble_inc_dec_instruction(
	data: &mut AssembledData,
	is_increment: bool,
	target: AddressingMode,
	instruction: &Instruction,
) -> Result<(), AssemblyError> {
	match target {
		AddressingMode::Register(register @ (Register::A | Register::X | Register::Y)) => data.append(
			match (is_increment, register) {
				(true, Register::A) => 0xBC,
				(true, Register::X) => 0x3D,
				(true, Register::Y) => 0xFC,
				(false, Register::A) => 0x9C,
				(false, Register::X) => 0x1D,
				(false, Register::Y) => 0xDC,
				_ => unreachable!(),
			},
			instruction.label.clone(),
		),
		AddressingMode::DirectPage(page_address) => data.append_instruction_with_8_bit_operand(
			if is_increment { 0xAB } else { 0x8B },
			page_address,
			instruction.label.clone(),
			instruction.span,
		),
		AddressingMode::DirectPageXIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			if is_increment { 0xBB } else { 0x9B },
			page_address,
			instruction.label.clone(),
			instruction.span,
		),
		AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
			if is_increment { 0xAC } else { 0x8C },
			address,
			instruction.label.clone(),
			instruction.span,
		),
		_ =>
			return Err(AssemblyError::InvalidAddressingMode {
				is_first_operand: true,
				mode:             target,
				mnemonic:         if is_increment { Mnemonic::Inc } else { Mnemonic::Dec },
				src:              data.source_code.clone(),
				location:         instruction.span,
				legal_modes:      vec![
					AddressingMode::Register(Register::A),
					AddressingMode::Register(Register::X),
					AddressingMode::Register(Register::Y),
					AddressingMode::DirectPageXIndexed(0.into()),
					AddressingMode::DirectPage(0.into()),
					AddressingMode::Address(0.into()),
				],
			}),
	}
	Ok(())
}

pub(super) fn assemble_shift_rotation_instruction(
	data: &mut AssembledData,
	mnemonic: Mnemonic,
	target: AddressingMode,
	instruction: &Instruction,
) -> Result<(), AssemblyError> {
	let target_copy = target.clone();
	let source_code_copy = data.source_code.clone();
	let make_xcn_err = || {
		Err(AssemblyError::InvalidAddressingMode {
			mode: target_copy,
			is_first_operand: true,
			mnemonic,
			location: instruction.span,
			legal_modes: vec![AddressingMode::Register(Register::A)],
			src: source_code_copy,
		})
	};

	match target {
		AddressingMode::Register(Register::A) => data.append(
			match mnemonic {
				Mnemonic::Asl => 0x1C,
				Mnemonic::Lsr => 0x5C,
				Mnemonic::Rol => 0x3C,
				Mnemonic::Ror => 0x7C,
				Mnemonic::Xcn => 0x9F,
				_ => unreachable!(),
			},
			instruction.label.clone(),
		),
		AddressingMode::DirectPage(page_address) => data.append_instruction_with_8_bit_operand(
			match mnemonic {
				Mnemonic::Asl => 0x0B,
				Mnemonic::Lsr => 0x4B,
				Mnemonic::Rol => 0x2B,
				Mnemonic::Ror => 0x6B,
				Mnemonic::Xcn => return make_xcn_err(),
				_ => unreachable!(),
			},
			page_address,
			instruction.label.clone(),
			instruction.span,
		),
		AddressingMode::DirectPageXIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match mnemonic {
				Mnemonic::Asl => 0x1B,
				Mnemonic::Lsr => 0x5B,
				Mnemonic::Rol => 0x3B,
				Mnemonic::Ror => 0x7B,
				Mnemonic::Xcn => return make_xcn_err(),
				_ => unreachable!(),
			},
			page_address,
			instruction.label.clone(),
			instruction.span,
		),
		AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
			match mnemonic {
				Mnemonic::Asl => 0xCC,
				Mnemonic::Lsr => 0x4C,
				Mnemonic::Rol => 0x2C,
				Mnemonic::Ror => 0x6C,
				Mnemonic::Xcn => return make_xcn_err(),
				_ => unreachable!(),
			},
			address,
			instruction.label.clone(),
			instruction.span,
		),
		_ =>
			return Err(AssemblyError::InvalidAddressingMode {
				mode: target,
				is_first_operand: true,
				mnemonic,
				location: instruction.span,
				legal_modes: vec![
					AddressingMode::Register(Register::A),
					AddressingMode::Address(0.into()),
					AddressingMode::DirectPage(0.into()),
					AddressingMode::DirectPageXIndexed(0.into()),
				],
				src: data.source_code.clone(),
			}),
	}
	Ok(())
}
