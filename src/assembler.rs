//! Assembler/codegen

#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::lexer::Register;
use crate::parser::{AddressingMode, Environment, Instruction, Label, MemoryAddress, Mnemonic, Number, Opcode};

/// Maximum number of resolution passes executed so that no endless resolution loops are hit.
pub const MAX_PASSES: usize = 10;

/// Assembles the instructions into a byte sequence.
/// # Errors
/// Unencodeable instructions will cause errors.
pub fn assemble(_environment: &Environment, instructions: Vec<Instruction>) -> Result<Vec<u8>, String> {
	let mut data = AssembledData::new();

	data.new_segment(0);

	for instruction in instructions {
		match instruction.opcode {
			Opcode { mnemonic: Mnemonic::Mov, first_operand: Some(target), second_operand: Some(source) } =>
				assemble_mov(&mut data, target, source, instruction.label)?,
			Opcode {
				mnemonic: mnemonic @ (Mnemonic::Adc | Mnemonic::Sbc | Mnemonic::And | Mnemonic::Or | Mnemonic::Eor),
				first_operand: Some(target),
				second_operand: Some(source),
			} => assemble_arithmetic_instruction(&mut data, mnemonic, target, source, instruction.label)?,
			opcode => return Err(format!("Unsupported combination of opcode and addressing modes: {:?}", opcode)),
		}
	}
	let mut pass_count = 0;
	while data.execute_label_resolution_pass() && pass_count < MAX_PASSES {
		pass_count += 1;
	}
	data.combine_segments()
}

#[allow(clippy::too_many_lines)] //heh
fn assemble_mov(
	data: &mut AssembledData,
	target: AddressingMode,
	source: AddressingMode,
	label: Option<Rc<Label>>,
) -> Result<(), String> {
	match target {
		AddressingMode::Register(Register::A) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0xE8, value, label),
			AddressingMode::IndirectX => data.append(0xE6, label),
			AddressingMode::IndirectXAutoIncrement => data.append(0xBF, label),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xE4, page_address, label),
			AddressingMode::DirectPageXIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF4, page_base_address, label),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(0xE5, address, label),
			AddressingMode::XIndexed(address) => data.append_instruction_with_16_bit_operand(0xF5, address, label),
			AddressingMode::YIndexed(address) => data.append_instruction_with_16_bit_operand(0xF6, address, label),
			AddressingMode::DirectPageXIndexedIndirect(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xE7, page_base_address, label),
			AddressingMode::DirectPageIndirectYIndexed(page_base_address) =>
				data.append_instruction_with_8_bit_operand(0xF7, page_base_address, label),
			AddressingMode::Register(Register::X) => data.append(0x7D, label),
			AddressingMode::Register(Register::Y) => data.append(0xDD, label),
			mode => return Err(format!("Unsupported `MOV A,` addressing mode {:?}", mode)),
		},
		AddressingMode::Register(Register::X) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0xCD, value, label),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF8, page_address, label),
			AddressingMode::DirectPageYIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xF9, page_address, label),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(0xE9, address, label),
			AddressingMode::Register(Register::A) => data.append(0x5D, label),
			AddressingMode::Register(Register::SP) => data.append(0x9D, label),
			mode => return Err(format!("Unsupported `MOV X,` addressing mode {:?}", mode)),
		},
		AddressingMode::Register(Register::Y) => match source {
			AddressingMode::Immediate(value) => data.append_instruction_with_8_bit_operand(0x8D, value, label),
			AddressingMode::DirectPage(page_address) =>
				data.append_instruction_with_8_bit_operand(0xEB, page_address, label),
			AddressingMode::DirectPageXIndexed(page_address) =>
				data.append_instruction_with_8_bit_operand(0xFB, page_address, label),
			AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(0xEC, address, label),
			AddressingMode::Register(Register::A) => data.append(0xFD, label),
			mode => return Err(format!("Unsupported `MOV Y,` addressing mode {:?}", mode)),
		},
		AddressingMode::Register(Register::SP) =>
			if source == AddressingMode::Register(Register::X) {
				data.append(0xBD, label);
			} else {
				return Err(format!("Unsupported `MOV SP,` addressing mode {:?}", source));
			},
		AddressingMode::IndirectX => match source {
			AddressingMode::Register(Register::A) => data.append(0xC6, label),
			_ => return Err("`MOV (X),` is only supported with A".to_owned()),
		},
		AddressingMode::IndirectXAutoIncrement => match source {
			AddressingMode::Register(Register::A) => data.append(0xAF, label),
			_ => return Err("`MOV (X)+,` is only supported with A".to_owned()),
		},
		AddressingMode::DirectPage(page_address) => match source {
			AddressingMode::Register(Register::A) => data.append_instruction_with_8_bit_operand(0xC4, page_address, label),
			AddressingMode::Register(Register::X) => data.append_instruction_with_8_bit_operand(0xD8, page_address, label),
			AddressingMode::Register(Register::Y) => data.append_instruction_with_8_bit_operand(0xCB, page_address, label),
			AddressingMode::DirectPage(source_address) =>
				data.append_instruction_with_two_8_bit_operands(0xFA, page_address, source_address, label),
			AddressingMode::Immediate(literal_value) =>
				data.append_instruction_with_two_8_bit_operands(0x8F, page_address, literal_value, label),
			mode => return Err(format!("Unsupported `MOV dp` addressing mode {:?}", mode)),
		},
		AddressingMode::DirectPageXIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xD4,
				AddressingMode::Register(Register::Y) => 0xDB,
				mode => return Err(format!("Unsupported `MOV (dp+X)` addressing mode {:?}", mode)),
			},
			page_address,
			label,
		),
		AddressingMode::DirectPageYIndexed(page_address) => data.append_instruction_with_8_bit_operand(
			match source {
				AddressingMode::Register(Register::X) => 0xD9,
				mode => return Err(format!("Unsupported `MOV (dp)+Y` addressing mode {:?}", mode)),
			},
			page_address,
			label,
		),
		AddressingMode::Address(address) => data.append_instruction_with_16_bit_operand(
			match source {
				AddressingMode::Register(Register::A) => 0xC5,
				AddressingMode::Register(Register::X) => 0xC9,
				AddressingMode::Register(Register::Y) => 0xCC,
				mode => return Err(format!("Unsupported `MOV addr` addressing mode {:?}", mode)),
			},
			address,
			label,
		),
		AddressingMode::XIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD5, address, label);
			} else {
				return Err(format!("Unsupported `MOV addr+X` addressing mode {:?}", source));
			},
		AddressingMode::YIndexed(address) =>
			if AddressingMode::Register(Register::A) == source {
				data.append_instruction_with_16_bit_operand(0xD6, address, label);
			} else {
				return Err(format!("Unsupported `MOV addr+Y` addressing mode {:?}", source));
			},
		AddressingMode::DirectPageXIndexedIndirect(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xC7, page_address, label);
			} else {
				return Err(format!("Unsupported `MOV (dp+X)` addressing mode {:?}", source));
			},
		AddressingMode::DirectPageIndirectYIndexed(page_address) =>
			if source == AddressingMode::Register(Register::A) {
				data.append_instruction_with_8_bit_operand(0xD7, page_address, label);
			} else {
				return Err(format!("Unsupported `MOV (dp)+Y` addressing mode {:?}", source));
			},
		_ => unimplemented!(),
	}
	Ok(())
}

#[allow(clippy::needless_pass_by_value)]
fn assemble_arithmetic_instruction(
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
					Mnemonic::Sbc => 0xA6,
					Mnemonic::And => 0x26,
					Mnemonic::Or => 0x06,
					Mnemonic::Eor => 0x46,
					_ => unreachable!(),
				},
				label,
			),
			_ => return Err("unimplemented".to_owned()),
		},
		_ => unimplemented!(),
	}
	Ok(())
}

/// Data in memory while we still need to resolve labels.
/// This data may have an attached label.
#[derive(Clone, Debug)]
pub struct LabeledMemoryValue {
	/// The label of this memory value.
	pub label: Option<Rc<Label>>,
	/// The actual memory value, which might or might not be resolved.
	pub value: MemoryValue,
}

impl LabeledMemoryValue {
	/// Try to resolve this memory value if it has a label. This always does nothing if the data is already resolved.
	#[inline]
	#[must_use]
	pub fn try_resolve(&mut self) -> bool {
		if let MemoryValue::Resolved(_) = self.value {
			false
		} else {
			// FIXME: I can't figure out how to do this without copying first.
			let value_copy = self.value.clone();
			self.value = value_copy.try_resolve();
			true
		}
	}

	/// Return the resolved memory value.
	/// # Panics
	/// If the memory value isn't resolved yet.
	#[inline]
	#[must_use]
	pub fn as_resolved_or_panic(&self) -> u8 {
		self.value.as_resolved_or_panic()
	}
}

/// The internal data held in a byte in memory, which may not be resolved.
#[derive(Clone, Debug)]
pub enum MemoryValue {
	/// Resolved data.
	Resolved(u8),
	/// High byte of an (unresolved) label.
	LabelHighByte(Rc<Label>),
	/// Low byte of an (unresolved) label.
	LabelLowByte(Rc<Label>),
}

impl MemoryValue {
	#[allow(clippy::match_wildcard_for_single_variants)]
	fn try_resolve(self) -> Self {
		match self {
			Self::Resolved(_) => self,
			Self::LabelHighByte(ref label) | Self::LabelLowByte(ref label) => match **label {
				Label { location: Some(memory_location), .. } => {
					let resolved_data = match self {
						Self::LabelHighByte(_) => (memory_location & 0xFF00) >> 8,
						Self::LabelLowByte(_) => memory_location & 0xFF,
						_ => unreachable!(),
					} as u8;
					Self::Resolved(resolved_data)
				},
				_ => self,
			},
		}
	}

	fn as_resolved_or_panic(&self) -> u8 {
		match self {
			Self::Resolved(value) => *value,
			_ => panic!("Unresolved memory value {:?}", self),
		}
	}
}

/// The assembled data, which consists of multiple sections.
#[derive(Clone, Debug, Default)]
pub struct AssembledData {
	/// The data segments. These are checked later when being combined into one.
	pub segments:              BTreeMap<MemoryAddress, Vec<LabeledMemoryValue>>,
	/// The starting address of the current segment. This is the key to the segments map.
	pub current_segment_start: Option<MemoryAddress>,
}

impl AssembledData {
	/// Combine the segments into one binary stream. The result has correct memory addresses, so the first byte is memory
	/// address 0 etc.
	/// # Errors
	/// If the segments contain overlapping data, errors are returned.
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
			let resolved_segment_data =
				segment_data.iter().map(LabeledMemoryValue::as_resolved_or_panic).collect::<Vec<u8>>();
			all_data.resize(*starting_address as usize, 0);
			all_data.extend_from_slice(&resolved_segment_data);
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
	pub fn current_segment(&self) -> &Vec<LabeledMemoryValue> {
		&self.segments[&self.current_segment_start.expect("didn't start a segment yet")]
	}

	/// Returns a mutable reference to the data of the current segment.
	#[allow(clippy::missing_panics_doc)]
	#[must_use]
	#[inline]
	pub fn current_segment_mut(&mut self) -> &mut Vec<LabeledMemoryValue> {
		self.segments.get_mut(&self.current_segment_start.expect("didn't start a segment yet")).unwrap()
	}

	/// Appends a little endian (LSB first) 16-bit value to the current segment. The given number is truncated to 16
	/// bits.
	#[inline]
	pub fn append_16_bits(&mut self, value: MemoryAddress, label: Option<Rc<Label>>) {
		self.append((value & 0xFF) as u8, label);
		self.append(((value & 0xFF00) >> 8) as u8, None);
	}

	/// Appends an 8-bit value to the current segment. The given number is truncated to 8 bits.
	#[inline]
	pub fn append_8_bits(&mut self, value: MemoryAddress, label: Option<Rc<Label>>) {
		self.append((value & 0xFF) as u8, label);
	}

	/// Appends an 8-bit value to the current segment.
	#[inline]
	pub fn append(&mut self, value: u8, label: Option<Rc<Label>>) {
		self.current_segment_mut().push(LabeledMemoryValue { value: MemoryValue::Resolved(value), label });
	}

	/// Appends an unresolved value that points to a label to the current segment. The `use_high_byte` parameter decides
	/// whether the high byte (true) or low byte (false) will be used in this memory address when the label is resolved.
	pub fn append_unresolved(&mut self, value: Rc<Label>, use_high_byte: bool, label: Option<Rc<Label>>) {
		self.current_segment_mut().push(LabeledMemoryValue {
			value: if use_high_byte { MemoryValue::LabelHighByte(value) } else { MemoryValue::LabelLowByte(value) },
			label,
		});
	}

	/// Appends an instruction with an 8-bit operand.
	#[inline]
	pub fn append_instruction_with_8_bit_operand(&mut self, opcode: u8, operand: Number, label: Option<Rc<Label>>) {
		self.append(opcode, label);
		match operand {
			Number::Literal(value) => self.append_8_bits(value, None),
			Number::Label(value) => self.append_unresolved(value, false, None),
		}
	}

	/// Appends an instruction with two 8-bit operands
	#[inline]
	pub fn append_instruction_with_two_8_bit_operands(
		&mut self,
		opcode: u8,
		first_operand: Number,
		second_operand: Number,
		label: Option<Rc<Label>>,
	) {
		self.append_instruction_with_8_bit_operand(opcode, first_operand, label);
		match second_operand {
			Number::Literal(value) => self.append_8_bits(value, None),
			Number::Label(value) => self.append_unresolved(value, false, None),
		}
	}

	/// Appends an instruction with an 16-bit operand.
	#[inline]
	pub fn append_instruction_with_16_bit_operand(&mut self, opcode: u8, operand: Number, label: Option<Rc<Label>>) {
		self.append(opcode, label);
		match operand {
			Number::Literal(value) => self.append_16_bits(value, None),
			Number::Label(value) => {
				// low byte first because little endian
				self.append_unresolved(value.clone(), false, None);
				self.append_unresolved(value, true, None);
			},
		}
	}

	/// Executes a label resolution pass. This means the following:
	/// * All data in all segments is traversed. The current memory location is kept track of during traversal.
	/// * All data with a label has that label assigned the current memory location.
	/// * All data that references a label has a resolution attempted, which succeeds if the label has "gained" an actual
	///   memory location. The label reference is then gone.
	/// This means that data which references labels declared later needs one additional resolution pass.
	/// # Returns
	/// Whether any modifications were actually done during the resolution pass.
	#[must_use]
	#[allow(clippy::missing_panics_doc)]
	pub fn execute_label_resolution_pass(&mut self) -> bool {
		let mut had_modifications = true;
		for (segment_start, segment_data) in &mut self.segments {
			for (offset, datum) in segment_data.iter_mut().enumerate() {
				let memory_address = segment_start + offset as i64;
				if datum.label.is_some_and(|existing_label| !existing_label.is_resolved()) {
					// Modifying Rc-contained data is dangerous in general, but safe for labels if we don't modify the name.
					unsafe { Rc::get_mut_unchecked(datum.label.as_mut().unwrap()) }.resolve_to(memory_address);
					had_modifications |= true;
				}
				had_modifications |= datum.try_resolve();
			}
		}
		had_modifications
	}
}
