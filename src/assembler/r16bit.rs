//! Assembling the 16-bit (word) instructions.
use super::*;

#[repr(u8)]
pub(super) enum MovDirection {
	IntoYA,
	FromYA,
}

pub(super) fn assemble_incw_decw_instruction(
	data: &mut AssembledData,
	is_increment: bool,
	target_address: Number,
	instruction: &mut Instruction,
) {
	data.append_instruction_with_8_bit_operand(if is_increment { 0x3A } else { 0x1A }, target_address, instruction);
}

pub(super) fn assemble_add_sub_cmp_wide_instruction(
	data: &mut AssembledData,
	mnemonic: Mnemonic,
	target_address: Number,
	instruction: &mut Instruction,
) {
	data.append_instruction_with_8_bit_operand(
		match mnemonic {
			Mnemonic::Addw => 0x7A,
			Mnemonic::Subw => 0x9A,
			Mnemonic::Cmpw => 0x5A,
			_ => unreachable!(),
		},
		target_address,
		instruction,
	);
}

pub(super) fn assemble_mov_wide_instruction(
	data: &mut AssembledData,
	page_address: Number,
	direction: &MovDirection,
	instruction: &mut Instruction,
) {
	data.append_instruction_with_8_bit_operand(
		match direction {
			MovDirection::IntoYA => 0xBA,
			MovDirection::FromYA => 0xDA,
		},
		page_address,
		instruction,
	);
}
