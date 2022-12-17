//! Assembling the MOV instruction.
use super::*;

#[allow(clippy::too_many_lines)] //heh
pub(super) fn assemble_mov(
	data: &mut AssembledData,
	target: &AddressingMode,
	source: AddressingMode,
	instruction: &mut Instruction,
) -> Result<(), Box<AssemblyError>> {
	let source_code_copy = data.source_code.clone();
	let make_error = |mode: AddressingMode, is_first_operand| {
		Err(AssemblyError::InvalidAddressingMode {
			is_first_operand,
			location: instruction.span,
			src: source_code_copy,
			mode: mode.to_string(),
			mnemonic: Mnemonic::Mov,
			legal_modes: if is_first_operand {
				vec![
					AddressingMode::Register(Register::A).to_string(),
					AddressingMode::Register(Register::X).to_string(),
					AddressingMode::Register(Register::Y).to_string(),
					AddressingMode::Register(Register::SP).to_string(),
					AddressingMode::IndirectX.to_string(),
					AddressingMode::IndirectXAutoIncrement.to_string(),
					AddressingMode::DirectPage(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::DirectPageXIndexed(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::DirectPageYIndexed(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::Address(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::XIndexed(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::YIndexed(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::DirectPageXIndexedIndirect(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::DirectPageIndirectYIndexed(AssemblyTimeValue::Literal(0)).to_string(),
				]
			} else {
				vec![
					AddressingMode::Register(Register::A).to_string(),
					AddressingMode::Register(Register::X).to_string(),
					AddressingMode::Register(Register::Y).to_string(),
					AddressingMode::Register(Register::SP).to_string(),
					AddressingMode::Immediate(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::IndirectX.to_string(),
					AddressingMode::IndirectXAutoIncrement.to_string(),
					AddressingMode::DirectPage(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::DirectPageXIndexed(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::Address(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::XIndexed(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::YIndexed(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::DirectPageXIndexedIndirect(AssemblyTimeValue::Literal(0)).to_string(),
					AddressingMode::DirectPageIndirectYIndexed(AssemblyTimeValue::Literal(0)).to_string(),
				]
			},
		}
		.into())
	};

	match target {
		AddressingMode::Register(Register::A) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0xE8, value, instruction)?,
			AddressingMode::IndirectX => data.append_instruction(0xE6, instruction),
			AddressingMode::IndirectXAutoIncrement => data.append_instruction(0xBF, instruction),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xE4, page_address, instruction)?,
			AddressingMode::DirectPageXIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF4, page_base_address, instruction)?,
			AddressingMode::Address(address) =>
				data.append_instruction_with_16_bit_operand(0xE5, address, instruction)?,
			AddressingMode::XIndexed(address) =>
				data.append_instruction_with_16_bit_operand(0xF5, address, instruction)?,
			AddressingMode::YIndexed(address) =>
				data.append_instruction_with_16_bit_operand(0xF6, address, instruction)?,
			AddressingMode::DirectPageXIndexedIndirect(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xE7, page_base_address, instruction)?,
			AddressingMode::DirectPageIndirectYIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF7, page_base_address, instruction)?,
			AddressingMode::Register(Register::X) => data.append_instruction(0x7D, instruction),
			AddressingMode::Register(Register::Y) => data.append_instruction(0xDD, instruction),
			mode => return make_error(mode, false),
		},
		AddressingMode::Register(Register::X) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0xCD, value, instruction)?,
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF8, page_address, instruction)?,
			AddressingMode::DirectPageYIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF9, page_address, instruction)?,
			AddressingMode::Address(address) =>
				data.append_instruction_with_16_bit_operand(0xE9, address, instruction)?,
			AddressingMode::Register(Register::A) => data.append_instruction(0x5D, instruction),
			AddressingMode::Register(Register::SP) => data.append_instruction(0x9D, instruction),
			mode => return make_error(mode, false),
		},
		AddressingMode::Register(Register::Y) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0x8D, value, instruction)?,
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xEB, page_address, instruction)?,
			AddressingMode::DirectPageXIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xFB, page_address, instruction)?,
			AddressingMode::Address(address) =>
				data.append_instruction_with_16_bit_operand(0xEC, address, instruction)?,
			AddressingMode::Register(Register::A) => data.append_instruction(0xFD, instruction),
			mode => return make_error(mode, false),
		},
		AddressingMode::Register(Register::SP) =>
			if source == AddressingMode::Register(Register::X) {
				data.append_instruction(0xBD, instruction);
			} else {
				return Err(AssemblyError::InvalidAddressingModeCombination {
					first_mode:  target.to_string(),
					mnemonic:    Mnemonic::Mov,
					src:         data.source_code.clone(),
					location:    instruction.span,
					second_mode: source.to_string(),
				}
				.into());
			},
		AddressingMode::IndirectX => match source {
			AddressingMode::Register(Register::A) => data.append_instruction(0xC6, instruction),
			mode => return make_error(mode, false),
		},
		AddressingMode::IndirectXAutoIncrement => match source {
			AddressingMode::Register(Register::A) => data.append_instruction(0xAF, instruction),
			mode => return make_error(mode, false),
		},
		AddressingMode::DirectPage(page_address) => match source {
			AddressingMode::Register(Register::A) =>
				data.append_instruction_with_8_bit_operand(0xC4, page_address.clone(), instruction)?,
			AddressingMode::Register(Register::X) =>
				data.append_instruction_with_8_bit_operand(0xD8, page_address.clone(), instruction)?,
			AddressingMode::Register(Register::Y) =>
				data.append_instruction_with_8_bit_operand(0xCB, page_address.clone(), instruction)?,
			AddressingMode::DirectPage(source_address) => data.append_instruction_with_two_8_bit_operands(
				0xFA,
				page_address.clone(),
				source_address,
				instruction,
			)?,
			AddressingMode::Immediate(literal_value) =>
				data.append_instruction_with_two_8_bit_operands(0x8F, page_address.clone(), literal_value, instruction)?,
			mode => return make_error(mode, false),
		},
		AddressingMode::DirectPageXIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xD4,
				AddressingMode::Register(Register::Y) => 0xDB,
				mode => return make_error(mode, false),
			},
			page_address.clone(),
			instruction,
		)?,
		AddressingMode::DirectPageYIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::X) => 0xD9,
				mode => return make_error(mode, false),
			},
			page_address.clone(),
			instruction,
		)?,
		AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xC5,
				AddressingMode::Register(Register::X) => 0xC9,
				AddressingMode::Register(Register::Y) => 0xCC,
				mode => return make_error(mode, false),
			},
			address.clone(),
			instruction,
		)?,
		AddressingMode::XIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD5, address.clone(), instruction)?;
			} else {
				return make_error(source, false);
			},
		AddressingMode::YIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD6, address.clone(), instruction)?;
			} else {
				return make_error(source, false);
			},
		AddressingMode::DirectPageXIndexedIndirect(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xC7, page_address.clone(), instruction)?;
			} else {
				return make_error(source, false);
			},
		AddressingMode::DirectPageIndirectYIndexed(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xD7, page_address.clone(), instruction)?;
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
	instruction: &mut Instruction,
) -> Result<(), Box<AssemblyError>> {
	let source_code_copy = data.source_code.clone();
	let make_error = || {
		Err(AssemblyError::InvalidAddressingMode {
			is_first_operand: true,
			location:         instruction.span,
			src:              source_code_copy,
			mode:             AddressingMode::Register(target).to_string(),
			mnemonic:         Mnemonic::Mov,
			// TODO
			legal_modes:      vec![],
		}
		.into())
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
		instruction.span,
	);
	Ok(())
}
