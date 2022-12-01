//! Assembling the branching instructions.
use super::*;

#[allow(clippy::too_many_lines)] // bruh
#[allow(clippy::needless_pass_by_value)]
pub(super) fn assemble_branching_instruction(
	data: &mut AssembledData,
	mnemonic: Mnemonic,
	target: AddressingMode,
	source: Option<AddressingMode>,
	instruction: &mut Instruction,
) -> Result<(), Box<AssemblyError>> {
	let target_copy = target.clone();
	let source_code_copy = data.source_code.clone();
	let make_target_error = |legal_modes| {
		Err(AssemblyError::InvalidAddressingMode {
			mode: target_copy.to_string(),
			is_first_operand: true,
			mnemonic,
			location: instruction.span,
			legal_modes,
			src: source_code_copy,
		}
		.into())
	};

	match &target {
		// Note that a direct page address in the target is also interpreted as a relative offset for some branch
		// instructions. For the other relative branches, it's in the source. JMP is absolute and uses full addresses.
		AddressingMode::DirectPage(page_address_or_relative) | AddressingMode::Address(page_address_or_relative @ Number::Reference(_)) if mnemonic != Mnemonic::Jmp =>
			match mnemonic {
				Mnemonic::Bra => data.append_instruction_with_relative_reference(0x2F, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Beq => data.append_instruction_with_relative_reference(0xF0, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Bne => data.append_instruction_with_relative_reference(0xD0, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Bcs => data.append_instruction_with_relative_reference(0xB0, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Bcc => data.append_instruction_with_relative_reference(0x90, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Bvs => data.append_instruction_with_relative_reference(0x70, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Bvc => data.append_instruction_with_relative_reference(0x50, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Bmi => data.append_instruction_with_relative_reference(0x30, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Bpl => data.append_instruction_with_relative_reference(0x10, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Pcall => data.append_instruction_with_8_bit_operand(0x4F, page_address_or_relative.clone(), instruction)?,
				Mnemonic::Tcall => data.append_instruction(0x01 | match page_address_or_relative {
						Number::Literal(value) => (value & 0x0F) as u8,
						_ => return Err(AssemblyError::InvalidAddressingMode {
							is_first_operand: true,
							mode: target.to_string(),
							mnemonic, legal_modes: vec![AddressingMode::Address(0.into()).to_string()],
							src: data.source_code.clone(),
							location: instruction.span
						}.into()),
					} << 4,
				instruction),
				// CBNE and DBNZ have the relative jump target in the source operand, and the target is the direct page
				// address that's checked and/or decremented.
				Mnemonic::Cbne | Mnemonic::Dbnz =>
					if let Some(AddressingMode::DirectPage(relative_source) | AddressingMode::Address(relative_source)) = source.clone() {
						#[cfg(test)]
						{
							instruction.assembled_size = Some(2);
						}

						data.append_instruction(
							if mnemonic == Mnemonic::Cbne {
								0x2E
							} else {
								0x6E
							},
						instruction,
						);

						// First argument is the checked direct page address
						match page_address_or_relative {
							Number::Literal(page_address) => data.append_8_bits(*page_address, None, instruction.span)?,
							value => data.append_8_bits_unresolved(value.clone(), 0, None, instruction.span)?,
						}
						// Second argument is the relative jump target.
						// The relative unresolved reference needs a negative offset of 2, because we're the second operand.
						match relative_source {
							Number::Literal(relative_offset) => data.append_8_bits(relative_offset, None, instruction.span)?,
							value => data.append_relative_unresolved(value, instruction.span)?,
						}
					} else {
						return if let Some(source) = source { Err(AssemblyError::InvalidAddressingModeCombination {
							second_mode: source.to_string(),
							first_mode: target.to_string(),
							src: data.source_code.clone(),
							location: instruction.span,mnemonic
						}.into())} else {
							make_target_error(vec![])
						};
					},
				_ => return make_target_error(vec![]),
			},
		AddressingMode::DirectPageXIndexed(page_address) =>
			if let Some(AddressingMode::DirectPage(relative_source) | AddressingMode::Address(relative_source)) = source.clone() && mnemonic == Mnemonic::Cbne {
				data.append_instruction(0xDE, instruction);
				// First argument is the checked direct page address
				match page_address {
					Number::Literal(page_address) => data.append_8_bits(*page_address, None, instruction.span)?,
					value => data.append_8_bits_unresolved(value.clone(), 0, None, instruction.span)?,
				}
				// Second argument is the relative jump target.
				// The relative unresolved reference needs a negative offset of 2, because we're the second operand.
				match relative_source {
					Number::Literal(relative_offset) => data.append_8_bits(relative_offset, None, instruction.span)?,
					value => data.append_relative_unresolved(value, instruction.span)?,
				}
			} else {
				return make_target_error(vec![]);
			},
		AddressingMode::Register(Register::Y) =>
			if let Some(AddressingMode::DirectPage(relative_jump_target)) = source && mnemonic == Mnemonic::Dbnz {
				data.append_instruction_with_relative_reference(0xFE, relative_jump_target, instruction);
			} else {
				return make_target_error(vec![]);
			},
		AddressingMode::Address(jump_target) => {
			data.append_instruction_with_16_bit_operand(match mnemonic {
				Mnemonic::Jmp => 0x5F,
				Mnemonic::Call => 0x3F,
				_ => return make_target_error(vec![]),
			}, jump_target.clone(), instruction
		);},
		AddressingMode::XIndexed(address) | AddressingMode::DirectPageXIndexedIndirect(address) if mnemonic == Mnemonic::Jmp => {
			data.append_instruction_with_16_bit_operand(0x1F, address.clone(), instruction);
		},
		AddressingMode::DirectPageBit(page_address, bit) | AddressingMode::AddressBit(page_address, bit) => {
			let is_bbs = mnemonic == Mnemonic::Bbs;
			if !is_bbs && mnemonic != Mnemonic::Bbc {
				return make_target_error(vec![AddressingMode::DirectPageBit(0.into(),0).to_string()]);
			}
			let jump_target = if let Some(AddressingMode::DirectPage(jump_target) | AddressingMode::Address(jump_target)) = source {
				jump_target
			} else {
				return Err(AssemblyError::InvalidAddressingMode {
					is_first_operand: false,
					mnemonic,
					mode: source.unwrap_or_else(|| AddressingMode::Register(Register::A)).to_string(),
					legal_modes: vec![AddressingMode::DirectPage(0.into()).to_string()],
					location: instruction.span,
					src: data.source_code.clone(),
				}.into())
			};
			// CAUTION! BBS and BBC have their operands in the SAME order in machine code and assembly, which is NOT THE DEFAULT! 
			data.append_instruction_with_two_8_bit_operands(
				if is_bbs { 0x03 } else { 0x13 } | (bit << 5),
				jump_target,
				page_address.clone(), instruction
			);
		},
		_ => return make_target_error(vec![]),
	}

	Ok(())
}
