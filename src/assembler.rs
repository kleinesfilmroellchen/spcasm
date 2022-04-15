//! Assembler/codegen

use crate::lexer::Register;
use crate::parser::{AddressingMode, Environment, Instruction, Mnemonic, Opcode};

/// Assembles the instructions into a byte sequence.
/// TODO: label resolution
/// # Errors
/// Unencodeable instructions will cause errors.
pub fn assemble(_environment: &Environment, instructions: Vec<Instruction>) -> Result<Vec<u8>, String> {
	// Closure doesn't work because they don't have lifetimes
	#[inline]
	#[allow(clippy::cast_sign_loss)]
	fn append_16_bits(data: &mut Vec<u8>, value: i64) {
		// lsb first (little endian?)
		data.push((value & 0xFF) as u8);
		data.push(((value & 0xFF00) >> 8) as u8);
	}
	#[inline]
	#[allow(clippy::cast_sign_loss)]
	fn append_8_bits(data: &mut Vec<u8>, value: i64) {
		data.push((value & 0xFF) as u8);
	}

	let mut data = Vec::new();

	for instruction in instructions {
		match instruction.opcode {
			Opcode { mnemonic: Mnemonic::Mov, first_operand: target, second_operand: source } => match target {
				Some(AddressingMode::Register(Register::A)) => match source {
					Some(AddressingMode::Immediate(value)) => {
						data.push(0xE8);
						append_8_bits(&mut data, value.value());
					},
					Some(AddressingMode::IndirectX) => data.push(0xE6),
					Some(AddressingMode::IndirectXAutoIncrement) => data.push(0xBF),
					Some(AddressingMode::DirectPage(page_address)) => {
						data.push(0xE4);
						append_8_bits(&mut data, page_address.value());
					},
					Some(AddressingMode::DirectPageXIndexed(page_base_address)) => {
						data.push(0xF4);
						append_8_bits(&mut data, page_base_address.value());
					},
					Some(AddressingMode::Address(address)) => {
						data.push(0xE5);
						append_16_bits(&mut data, address.value());
					},
					Some(AddressingMode::XIndexed(address)) => {
						data.push(0xF5);
						append_16_bits(&mut data, address.value());
					},
					Some(AddressingMode::YIndexed(address)) => {
						data.push(0xF6);
						append_16_bits(&mut data, address.value());
					},
					Some(AddressingMode::DirectPageXIndexedIndirect(page_base_address)) => {
						data.push(0xE7);
						append_8_bits(&mut data, page_base_address.value());
					},
					Some(AddressingMode::DirectPageIndirectYIndexed(page_base_address)) => {
						data.push(0xF7);
						append_8_bits(&mut data, page_base_address.value());
					},
					Some(mode) => return Err(format!("Unsupported `MOV A,` addressing mode {:?}", mode)),
					None => return Err("MOV must have a source operand".to_owned()),
				},
				_ => unimplemented!(),
			},
		}
	}
	Ok(data)
}
