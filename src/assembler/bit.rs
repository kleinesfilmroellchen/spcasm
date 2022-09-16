//! Bitwise instructions.
use super::*;

#[allow(clippy::too_many_lines)] // This is getting old
pub(super) fn assemble_bit_instructions(
	data: &mut AssembledData,
	mnemonic: Mnemonic,
	target: AddressingMode,
	source: &Option<AddressingMode>,
	instruction: &mut Instruction,
) -> Result<(), AssemblyError> {
	let target_copy = target.clone();
	let source_copy = source.clone();
	let source_code_copy = data.source_code.clone();
	let make_error = |is_first_operand| {
		Err(AssemblyError::InvalidAddressingMode {
			mode: if is_first_operand { target_copy.to_string() } else { source_copy.unwrap().to_string() },
			is_first_operand,
			mnemonic,
			location: instruction.span,
			// TODO
			legal_modes: vec![],
			src: source_code_copy,
		})
	};

	match target {
		AddressingMode::DirectPageBit(page_address, bit) => {
			if mnemonic != Mnemonic::Set1 && mnemonic != Mnemonic::Clr1 {
				return make_error(true);
			} else if source.is_some() {
				return Err(AssemblyError::TwoOperandsNotAllowed {
					location: instruction.span,
					src: data.source_code.clone(),
					mnemonic,
				});
			}
			data.append_instruction_with_8_bit_operand(
				if mnemonic == Mnemonic::Set1 { 0x02 } else { 0x12 } | (bit << 5),
				page_address,
				instruction,
			);
		},
		AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
			match mnemonic {
				Mnemonic::Tset1 => 0x0E,
				Mnemonic::Tclr1 => 0x4E,
				_ => return make_error(true),
			},
			address,
			instruction,
		),
		AddressingMode::CarryFlag => match &source {
			Some(
				addressing_mode @ (AddressingMode::AddressBit(address, bit)
				| AddressingMode::NegatedAddressBit(address, bit)),
			) => {
				let is_negated = matches!(addressing_mode, AddressingMode::NegatedAddressBit(..));
				data.append_instruction_with_16_bit_operand_and_bit_index(
					match mnemonic {
						Mnemonic::And1 =>
							if is_negated {
								0x6A
							} else {
								0x4A
							},
						Mnemonic::Or1 =>
							if is_negated {
								0x2A
							} else {
								0x0A
							},
						Mnemonic::Eor1 =>
							if is_negated {
								return make_error(false);
							} else {
								0x8A
							},
						Mnemonic::Mov1 => 0xAA,
						_ => return make_error(false),
					},
					address.clone(),
					*bit,
					instruction,
				);
			},
			_ => return make_error(true),
		},
		AddressingMode::AddressBit(address, bit) => match mnemonic {
			Mnemonic::Not1 =>
				if source.is_some() {
					return Err(AssemblyError::TwoOperandsNotAllowed {
						mnemonic,
						location: instruction.span,
						src: data.source_code.clone(),
					});
				} else {
					data.append_instruction_with_16_bit_operand_and_bit_index(0xEA, address, bit, instruction);
				},
			Mnemonic::Mov1 =>
				if matches!(source, Some(AddressingMode::CarryFlag)) {
					data.append_instruction_with_16_bit_operand_and_bit_index(0xCA, address, bit, instruction);
				} else {
					return make_error(true);
				},
			_ => return make_error(true),
		},
		_ => return make_error(true),
	};
	Ok(())
}
