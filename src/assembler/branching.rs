//! Assembling the branching instructions.

use crate::assembler::AssembledData;
use crate::lexer::Register;
use crate::parser::{AddressingMode, Mnemonic, Number, Instruction};
use crate::error::AssemblyError;

#[allow(clippy::needless_pass_by_value)]
pub(super) fn assemble_branching_instruction(
	data: &mut AssembledData,
	mnemonic: Mnemonic,
	target: AddressingMode,
	source: Option<AddressingMode>,
	instruction: &Instruction,
) -> Result<(), AssemblyError> {

	let target_copy = target.clone();
	let make_target_error = |legal_modes| {
		Err(AssemblyError::InvalidAddressingMode {
			mode: target_copy,
			is_first_operand: true,
			mnemonic,
			location: instruction.span,
			legal_modes,
			src: data.source_code.clone(),
		})
	};

	match &target {
		// Note that a direct page address in the target is also interpreted as a relative offset for some branch
		// instructions. For the other relative branches, it's in the source. JMP is absolute and uses full addresses.
		AddressingMode::DirectPage(page_address_or_relative) | AddressingMode::Address(page_address_or_relative @ Number::Label(_)) => 
			match mnemonic {
				Mnemonic::Bra => data.append_instruction_with_relative_label(0x2F, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Beq => data.append_instruction_with_relative_label(0xF0, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Bne => data.append_instruction_with_relative_label(0xD0, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Bcs => data.append_instruction_with_relative_label(0xB0, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Bcc => data.append_instruction_with_relative_label(0x90, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Bvs => data.append_instruction_with_relative_label(0x70, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Bvc => data.append_instruction_with_relative_label(0x50, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Bmi => data.append_instruction_with_relative_label(0x30, page_address_or_relative.clone(), instruction.label.clone()),
				Mnemonic::Bpl => data.append_instruction_with_relative_label(0x10, page_address_or_relative.clone(), instruction.label.clone()),
				// CBNE and DBNZ have the relative jump target in the source operand, and the target is the direct page
				// address that's checked and/or decremented.
				Mnemonic::Cbne | Mnemonic::Dbnz =>
					if let Some(AddressingMode::DirectPage(relative_source) | AddressingMode::Address(relative_source)) = source.clone() {
						data.append(
								if mnemonic == Mnemonic::Cbne {
									0x2E
								} else {
									0x6E
								},
							instruction.label.clone(),
						);
						// First argument is the checked direct page address
						match page_address_or_relative {
							Number::Literal(page_address) => data.append_8_bits(*page_address, None),
							Number::Label(page_label) => data.append_unresolved(page_label.clone(), false, None),
						}
						// Second argument is the relative jump target.
						// The relative unresolved label needs a negative offset of 2, because we're the second operand.
						match relative_source {
							Number::Literal(relative_offset) => data.append_8_bits(relative_offset, None),
							Number::Label(relative_source_label) => data.append_relative_unresolved(relative_source_label, 2),
						}
					} else {
						return if let Some(source) =  source { Err(AssemblyError::InvalidAddressingModeCombination {
							second_mode: source, first_mode: target,
							src: data.source_code.clone(), location: instruction.span,mnemonic
						})} else {
							// TODO
							make_target_error(vec![])
						};
					},
				_ => return make_target_error(vec![]),
			},
		AddressingMode::DirectPageXIndexed(page_address) =>
			if let Some(AddressingMode::DirectPage(relative_source) | AddressingMode::Address(relative_source)) = source.clone() && mnemonic == Mnemonic::Cbne {
				data.append(0xDE, instruction.label.clone());
				// First argument is the checked direct page address
				match page_address {
					Number::Literal(page_address) => data.append_8_bits(*page_address, None),
					Number::Label(page_label) => data.append_unresolved(page_label.clone(), false, None),
				}
				// Second argument is the relative jump target.
				// The relative unresolved label needs a negative offset of 2, because we're the second operand.
				match relative_source {
					Number::Literal(relative_offset) => data.append_8_bits(relative_offset, None),
					Number::Label(relative_source_label) => data.append_relative_unresolved(relative_source_label, 2),
				}
			} else {
				return make_target_error(vec![]);
			},
		AddressingMode::Register(Register::Y) =>
			if let Some(AddressingMode::DirectPage(relative_jump_target)) = source && mnemonic == Mnemonic::Dbnz {
				data.append_instruction_with_relative_label(0xFE, relative_jump_target, instruction.label.clone());
			} else {
				// TODO
				return make_target_error(vec![]);
			},
		AddressingMode::Address(jump_target) => {
			if mnemonic != Mnemonic::Jmp {
				return make_target_error(vec![]);
			}
			data.append_instruction_with_16_bit_operand(0x5F, jump_target.clone(), instruction.label.clone());
		},
		AddressingMode::XIndexed(address) => data.append_instruction_with_16_bit_operand(0x1F, address.clone(), instruction.label.clone()),
		_ => return make_target_error(vec![]),
	}

	Ok(())
}
