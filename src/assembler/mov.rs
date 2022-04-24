//! Assembling the MOV instruction.
use crate::assembler::AssembledData;
use crate::error::AssemblyError;
use crate::lexer::Register;
use crate::parser::{AddressingMode, Instruction, Mnemonic};

#[allow(clippy::too_many_lines)] //heh
pub(super) fn assemble_mov(
	data: &mut AssembledData,
	target: &AddressingMode,
	source: AddressingMode,
	instruction: &Instruction,
) -> Result<(), AssemblyError> {
	let source_code_copy = data.source_code.clone();
	let make_error = |mode, is_first_operand| {
		Err(AssemblyError::InvalidAddressingMode {
			is_first_operand,
			location: instruction.span,
			src: source_code_copy,
			mode,
			mnemonic: Mnemonic::Mov,
			// TODO
			legal_modes: vec![],
		})
	};

	match target {
		AddressingMode::Register(Register::A) => match source {
			AddressingMode::Immediate(value) =>
				data.append_instruction_with_8_bit_operand(0xE8, value, instruction.label.clone()),
			AddressingMode::IndirectX => data.append(0xE6, instruction.label.clone()),
			AddressingMode::IndirectXAutoIncrement => data.append(0xBF, instruction.label.clone()),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xE4, page_address, instruction.label.clone()),
			AddressingMode::DirectPageXIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF4, page_base_address, instruction.label.clone()),
			AddressingMode::Address(address) =>
				data.append_instruction_with_16_bit_operand(0xE5, address, instruction.label.clone()),
			AddressingMode::XIndexed(address) =>
				data.append_instruction_with_16_bit_operand(0xF5, address, instruction.label.clone()),
			AddressingMode::YIndexed(address) =>
				data.append_instruction_with_16_bit_operand(0xF6, address, instruction.label.clone()),
			AddressingMode::DirectPageXIndexedIndirect(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xE7, page_base_address, instruction.label.clone()),
			AddressingMode::DirectPageIndirectYIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF7, page_base_address, instruction.label.clone()),
			AddressingMode::Register(Register::X) => data.append(0x7D, instruction.label.clone()),
			AddressingMode::Register(Register::Y) => data.append(0xDD, instruction.label.clone()),
			mode => return make_error(mode, false),
		},
		AddressingMode::Register(Register::X) => match source {
			AddressingMode::Immediate(value) =>
				data.append_instruction_with_8_bit_operand(0xCD, value, instruction.label.clone()),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF8, page_address, instruction.label.clone()),
			AddressingMode::DirectPageYIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF9, page_address, instruction.label.clone()),
			AddressingMode::Address(address) =>
				data.append_instruction_with_16_bit_operand(0xE9, address, instruction.label.clone()),
			AddressingMode::Register(Register::A) => data.append(0x5D, instruction.label.clone()),
			AddressingMode::Register(Register::SP) => data.append(0x9D, instruction.label.clone()),
			mode => return make_error(mode, false),
		},
		AddressingMode::Register(Register::Y) => match source {
			AddressingMode::Immediate(value) =>
				data.append_instruction_with_8_bit_operand(0x8D, value, instruction.label.clone()),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xEB, page_address, instruction.label.clone()),
			AddressingMode::DirectPageXIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xFB, page_address, instruction.label.clone()),
			AddressingMode::Address(address) =>
				data.append_instruction_with_16_bit_operand(0xEC, address, instruction.label.clone()),
			AddressingMode::Register(Register::A) => data.append(0xFD, instruction.label.clone()),
			mode => return make_error(mode, false),
		},
		AddressingMode::Register(Register::SP) =>
			if source == AddressingMode::Register(Register::X) {
				data.append(0xBD, instruction.label.clone());
			} else {
				return Err(AssemblyError::InvalidAddressingMode {
					is_first_operand: false,
					mode:             source,
					mnemonic:         Mnemonic::Mov,
					src:              data.source_code.clone(),
					location:         instruction.span,
					legal_modes:      vec![AddressingMode::Register(Register::X)],
				});
			},
		AddressingMode::IndirectX => match source {
			AddressingMode::Register(Register::A) => data.append(0xC6, instruction.label.clone()),
			mode => return make_error(mode,false),
		},
		AddressingMode::IndirectXAutoIncrement => match source {
			AddressingMode::Register(Register::A) => data.append(0xAF, instruction.label.clone()),
			mode => return make_error(mode,false),
		},
		AddressingMode::DirectPage(page_address) => match source {
			AddressingMode::Register(Register::A) =>
				data.append_instruction_with_8_bit_operand(0xC4, page_address.clone(), instruction.label.clone()),
			AddressingMode::Register(Register::X) =>
				data.append_instruction_with_8_bit_operand(0xD8, page_address.clone(), instruction.label.clone()),
			AddressingMode::Register(Register::Y) =>
				data.append_instruction_with_8_bit_operand(0xCB, page_address.clone(), instruction.label.clone()),
			AddressingMode::DirectPage(source_address) => data.append_instruction_with_two_8_bit_operands(
				0xFA,
				page_address.clone(),
				source_address,
				instruction.label.clone(),
			),
			AddressingMode::Immediate(literal_value) => data.append_instruction_with_two_8_bit_operands(
				0x8F,
				page_address.clone(),
				literal_value,
				instruction.label.clone(),
			),
			mode => return make_error(mode, false),
		},
		AddressingMode::DirectPageXIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xD4,
				AddressingMode::Register(Register::Y) => 0xDB,
				mode => return make_error(mode, false),
			},
			page_address.clone(),
			instruction.label.clone(),
		),
		AddressingMode::DirectPageYIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::X) => 0xD9,
				mode => return make_error(mode, false),
			},
			page_address.clone(),
			instruction.label.clone(),
		),
		AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xC5,
				AddressingMode::Register(Register::X) => 0xC9,
				AddressingMode::Register(Register::Y) => 0xCC,
				mode => return make_error(mode, false),
			},
			address.clone(),
			instruction.label.clone(),
		),
		AddressingMode::XIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD5, address.clone(), instruction.label.clone());
			} else {
				return make_error(source, false);
			},
		AddressingMode::YIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD6, address.clone(), instruction.label.clone());
			} else {
				return make_error(source, false);
			},
		AddressingMode::DirectPageXIndexedIndirect(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xC7, page_address.clone(), instruction.label.clone());
			} else {
				return make_error(source, false);
			},
		AddressingMode::DirectPageIndirectYIndexed(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xD7, page_address.clone(), instruction.label.clone());
			} else {
				return make_error(source, false);
			},
		mode => return make_error(mode.clone(), true),
	}
	Ok(())
}

pub(super) fn assemble_push_pop(
	data: &mut AssembledData,
	is_push: bool,
	target: Register,
	instruction: &Instruction,
) -> Result<(), AssemblyError> {
	let source_code_copy = data.source_code.clone();
	let make_error = || {
		Err(AssemblyError::InvalidAddressingMode {
			is_first_operand: true,
			location:         instruction.span,
			src:              source_code_copy,
			mode:             AddressingMode::Register(target),
			mnemonic:         Mnemonic::Mov,
			// TODO
			legal_modes:      vec![],
		})
	};
	data.append_8_bits(
		if is_push {
			match target {
				Register::A => 0x2D,
				Register::X => 0x4D,
				Register::Y => 0x6D,
				Register::PSW => 0x0D,
				_ => return make_error(),
			}
		} else {
			match target {
				Register::A => 0xAE,
				Register::X => 0xCE,
				Register::Y => 0xEE,
				Register::PSW => 0x8E,
				_ => return make_error(),
			}
		},
		instruction.label.clone(),
	);
	Ok(())
}
