//! Assembler/codegen

use std::collections::BTreeMap;

use crate::lexer::Register;
use crate::parser::{AddressingMode, Environment, Instruction, MemoryAddress, Mnemonic, Opcode};

/// Assembles the instructions into a byte sequence.
/// TODO: label resolution
/// # Errors
/// Unencodeable instructions will cause errors.
pub fn assemble(_environment: &Environment, instructions: Vec<Instruction>) -> Result<Vec<u8>, String> {
	let mut data = AssembledData::new();

	data.new_segment(0);

	for instruction in instructions {
		match instruction.opcode {
			Opcode { mnemonic: Mnemonic::Mov, first_operand: target, second_operand: source } => match target {
				Some(AddressingMode::Register(Register::A)) => match source {
					Some(AddressingMode::Immediate(value)) =>
						data.append_instruction_with_8_bit_operand(0xE8, value.value()),
					Some(AddressingMode::IndirectX) => data.append(0xE6),
					Some(AddressingMode::IndirectXAutoIncrement) => data.append(0xBF),
					Some(AddressingMode::DirectPage(page_address)) =>
						data.append_instruction_with_8_bit_operand(0xE4, page_address.value()),
					Some(AddressingMode::DirectPageXIndexed(page_base_address)) =>
						data.append_instruction_with_8_bit_operand(0xF4, page_base_address.value()),
					Some(AddressingMode::Address(address)) =>
						data.append_instruction_with_16_bit_operand(0xE5, address.value()),
					Some(AddressingMode::XIndexed(address)) =>
						data.append_instruction_with_16_bit_operand(0xF5, address.value()),
					Some(AddressingMode::YIndexed(address)) =>
						data.append_instruction_with_16_bit_operand(0xF6, address.value()),
					Some(AddressingMode::DirectPageXIndexedIndirect(page_base_address)) =>
						data.append_instruction_with_8_bit_operand(0xE7, page_base_address.value()),
					Some(AddressingMode::DirectPageIndirectYIndexed(page_base_address)) =>
						data.append_instruction_with_8_bit_operand(0xF7, page_base_address.value()),
					Some(mode) => return Err(format!("Unsupported `MOV A,` addressing mode {:?}", mode)),
					None => return Err("MOV must have a source operand".to_owned()),
				},
				_ => unimplemented!(),
			},
		}
	}
	data.combine_segments()
}

/// The assembled data, which consists of multiple sections.
#[derive(Clone, Debug, Default)]
pub struct AssembledData {
	/// The data segments. These are checked later when being combined into one.
	pub segments:              BTreeMap<MemoryAddress, Vec<u8>>,
	/// The starting address of the current segment. This is the key to the segments map.
	pub current_segment_start: Option<MemoryAddress>,
}

impl AssembledData {
	/// Combine the segments into one binary stream. The result has correct memory addresses, so the first byte is memory
	/// address 0 etc.
	/// # Errors
	/// If the segments contain overlapping data, errors are returned.
	#[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
	pub fn combine_segments(&self) -> Result<Vec<u8>, String> {
		let mut all_data = Vec::new();
		// The iteration is sorted
		for (starting_address, segment_data) in &self.segments {
			if *starting_address < all_data.len() as i64 {
				return Err(format!(
					"Section at {:04x} starts after the end of the previous one, which is {:04x}",
					starting_address,
					all_data.len()
				));
			}
			all_data.resize(*starting_address as usize, 0);
			all_data.extend_from_slice(segment_data);
		}

		Ok(all_data)
	}

	/// Creates new assembled data
	#[must_use]
	#[inline]
	pub fn new() -> Self {
		Self::default()
	}

	/// Starts a new segment at the given memory address and set it as the current segment.
	/// <strong>Warning: This replaces any segment that currently starts at this memory address!</strong>
	#[inline]
	pub fn new_segment(&mut self, segment_start: MemoryAddress) -> &mut Self {
		self.segments.insert(segment_start, Vec::new());
		self.current_segment_start = Some(segment_start);
		self
	}

	/// Returns an immutable reference to the data of the current segment.
	#[must_use]
	#[inline]
	pub fn current_segment(&self) -> &Vec<u8> {
		&self.segments[&self.current_segment_start.expect("didn't start a segment yet")]
	}

	/// Returns a mutable reference to the data of the current segment.
	#[allow(clippy::missing_panics_doc)]
	#[must_use]
	#[inline]
	pub fn current_segment_mut(&mut self) -> &mut Vec<u8> {
		self.segments.get_mut(&self.current_segment_start.expect("didn't start a segment yet")).unwrap()
	}

	/// Appends a slice of data to the current segment.
	#[inline]
	pub fn append_slice<'a, 'b>(&'a mut self, data: &'b [u8]) -> &'a mut Self {
		self.current_segment_mut().extend_from_slice(data);
		self
	}

	/// Appends a little endian (LSB first) 16-bit value to the current segment. The given number is truncated to 16
	/// bits.
	#[inline]
	#[allow(clippy::cast_sign_loss)]
	pub fn append_16_bits(&mut self, value: MemoryAddress) {
		self.current_segment_mut().push((value & 0xFF) as u8);
		self.current_segment_mut().push(((value & 0xFF00) >> 8) as u8);
	}

	/// Appends an 8-bit value to the current segment. The given number is truncated to 8 bits.
	#[inline]
	#[allow(clippy::cast_sign_loss)]
	pub fn append_8_bits(&mut self, value: MemoryAddress) {
		self.current_segment_mut().push((value & 0xFF) as u8);
	}

	/// Appends an 8-bit value to the current segment.
	#[inline]
	pub fn append(&mut self, value: u8) {
		self.current_segment_mut().push(value);
	}

	/// Appends an instruction with an 8-bit operand.
	#[inline]
	pub fn append_instruction_with_8_bit_operand(&mut self, opcode: u8, operand: MemoryAddress) {
		self.append(opcode);
		self.append_8_bits(operand);
	}

	/// Appends an instruction with an 16-bit operand.
	#[inline]
	pub fn append_instruction_with_16_bit_operand(&mut self, opcode: u8, operand: MemoryAddress) {
		self.append(opcode);
		self.append_16_bits(operand);
	}
}
