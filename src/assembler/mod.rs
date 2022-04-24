//! Assembler/codegen

#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]

use std::collections::BTreeMap;
use std::sync::Arc;

use r16bit::MovDirection;

use super::pretty_hex;
use crate::error::{AssemblyCode, AssemblyError};
use crate::instruction::{AddressingMode, Instruction, Label, MemoryAddress, Mnemonic, Number, Opcode};
use crate::parser::Environment;
use crate::Register;

mod arithmetic_logic;
mod branching;
mod mov;
mod r16bit;

/// Maximum number of resolution passes executed so that no endless resolution loops are hit.
pub const MAX_PASSES: usize = 10;

/// Assembles the instructions into a byte sequence.
/// # Errors
/// Unencodeable instructions will cause errors.
#[allow(clippy::too_many_lines)] // ¯\_(ツ)_/¯
pub fn assemble(environment: &Environment, instructions: Vec<Instruction>) -> Result<Vec<u8>, AssemblyError> {
	let mut data = AssembledData::new(environment.source_code.clone());

	data.new_segment(0);

	for instruction in instructions {
		match &instruction.opcode {
			Opcode { mnemonic: Mnemonic::Mov, first_operand: Some(target), second_operand: Some(source) } =>
				mov::assemble_mov(&mut data, target, source.clone(), &instruction)?,
			Opcode {
				mnemonic:
					mnemonic @ (Mnemonic::Adc | Mnemonic::Sbc | Mnemonic::And | Mnemonic::Or | Mnemonic::Eor | Mnemonic::Cmp),
				first_operand: Some(target),
				second_operand: Some(source),
			} => arithmetic_logic::assemble_arithmetic_instruction(
				&mut data,
				*mnemonic,
				target.clone(),
				source.clone(),
				&instruction,
			)?,
			Opcode {
				mnemonic: mnemonic @ (Mnemonic::Inc | Mnemonic::Dec),
				first_operand: Some(target),
				second_operand: None,
			} => {
				let is_increment = *mnemonic == Mnemonic::Inc;
				arithmetic_logic::assemble_inc_dec_instruction(&mut data, is_increment, target.clone(), &instruction)?;
			},
			Opcode {
				mnemonic: mnemonic @ (Mnemonic::Asl | Mnemonic::Lsr | Mnemonic::Rol | Mnemonic::Ror | Mnemonic::Xcn),
				first_operand: Some(target),
				second_operand: None,
			} => arithmetic_logic::assemble_shift_rotation_instruction(&mut data, *mnemonic, target.clone(), &instruction)?,
			Opcode {
				mnemonic: mnemonic @ (Mnemonic::Incw | Mnemonic::Decw),
				first_operand: Some(AddressingMode::DirectPage(target)),
				second_operand: None,
			} => {
				let is_increment = *mnemonic == Mnemonic::Incw;
				r16bit::assemble_incw_decw_instruction(&mut data, is_increment, target.clone(), instruction.label);
			},
			Opcode {
				mnemonic: mnemonic @ (Mnemonic::Addw | Mnemonic::Subw | Mnemonic::Cmpw),
				first_operand: Some(AddressingMode::Register(Register::YA)),
				second_operand: Some(AddressingMode::DirectPage(target)),
			} => r16bit::assemble_add_sub_cmp_wide_instruction(&mut data, *mnemonic, target.clone(), instruction.label),
			Opcode {
				mnemonic: Mnemonic::Movw,
				first_operand: Some(target @ (AddressingMode::DirectPage(_) | AddressingMode::Register(Register::YA))),
				second_operand: Some(source @ (AddressingMode::DirectPage(_) | AddressingMode::Register(Register::YA))),
			} => {
				let make_movw_error = || {
					Err(AssemblyError::InvalidAddressingModeCombination {
						first_mode:  target.clone(),
						second_mode: source.clone(),
						src:         data.source_code.clone(),
						location:    instruction.span,
						mnemonic:    Mnemonic::Movw,
					})
				};
				let (direction, page_address) = if *target == AddressingMode::Register(Register::YA) {
					(MovDirection::IntoYA, match source {
						AddressingMode::DirectPage(page_address) => page_address,
						_ => return make_movw_error(),
					})
				} else {
					(MovDirection::FromYA, match target {
						AddressingMode::DirectPage(page_address) => page_address,
						_ => return make_movw_error(),
					})
				};
				r16bit::assemble_mov_wide_instruction(&mut data, page_address.clone(), &direction, instruction.label);
			},
			Opcode {
				mnemonic: Mnemonic::Mul,
				first_operand: Some(AddressingMode::Register(Register::YA)),
				second_operand: None,
			} => data.append(0xCF, instruction.label),
			Opcode {
				mnemonic: Mnemonic::Div,
				first_operand: Some(AddressingMode::Register(Register::YA)),
				second_operand: Some(AddressingMode::Register(Register::X)),
			} => data.append(0x9E, instruction.label),
			Opcode {
				mnemonic: mnemonic @ (Mnemonic::Daa | Mnemonic::Das),
				first_operand: Some(AddressingMode::Register(Register::A)),
				second_operand: None,
			} => data.append(if *mnemonic == Mnemonic::Daa { 0xDF } else { 0xBE }, instruction.label),
			Opcode {
				mnemonic:
					mnemonic @ (Mnemonic::Bra
					| Mnemonic::Beq
					| Mnemonic::Bne
					| Mnemonic::Bcs
					| Mnemonic::Bcc
					| Mnemonic::Bvs
					| Mnemonic::Bvc
					| Mnemonic::Bmi
					| Mnemonic::Bpl
					| Mnemonic::Bbs
					| Mnemonic::Bbc
					| Mnemonic::Cbne
					| Mnemonic::Dbnz
					| Mnemonic::Jmp),
				first_operand: Some(target),
				second_operand: source,
			} => branching::assemble_branching_instruction(
				&mut data,
				*mnemonic,
				target.clone(),
				source.clone(),
				&instruction,
			)?,
			Opcode {
				mnemonic:
					mnemonic @ (Mnemonic::Brk
					| Mnemonic::Ret
					| Mnemonic::Ret1
					| Mnemonic::Clrc
					| Mnemonic::Setc
					| Mnemonic::Notc
					| Mnemonic::Clrv
					| Mnemonic::Clrp
					| Mnemonic::Setp
					| Mnemonic::Ei
					| Mnemonic::Di
					| Mnemonic::Nop
					| Mnemonic::Sleep
					| Mnemonic::Stop),
				first_operand: None,
				second_operand: None,
			} => assemble_operandless_instruction(&mut data, *mnemonic, instruction.label),
			Opcode {
				mnemonic: mnemonic @ (Mnemonic::Push | Mnemonic::Pop),
				first_operand: Some(AddressingMode::Register(target)),
				second_operand: None,
			} => mov::assemble_push_pop(&mut data, *mnemonic == Mnemonic::Push, *target, &instruction)?,
			Opcode { mnemonic, first_operand: Some(_), second_operand: Some(_) } =>
				return Err(AssemblyError::TwoOperandsNotAllowed {
					mnemonic: *mnemonic,
					src:      data.source_code,
					location: instruction.span,
				}),
			Opcode { mnemonic, first_operand: Some(_), .. } =>
				return Err(AssemblyError::OperandNotAllowed {
					mnemonic: *mnemonic,
					src:      data.source_code,
					location: instruction.span,
				}),
			_ => unreachable!(),
		}
	}
	let mut pass_count = 0;
	while data.execute_label_resolution_pass() && pass_count < MAX_PASSES {
		pass_count += 1;
	}
	data.combine_segments()
}

fn assemble_operandless_instruction(data: &mut AssembledData, mnemonic: Mnemonic, label: Option<Arc<Label>>) {
	data.append(
		match mnemonic {
			Mnemonic::Brk => 0x0F,
			Mnemonic::Ret => 0x6F,
			Mnemonic::Ret1 => 0x7F,
			Mnemonic::Clrc => 0x60,
			Mnemonic::Setc => 0x80,
			Mnemonic::Notc => 0xED,
			Mnemonic::Clrv => 0xE0,
			Mnemonic::Clrp => 0x20,
			Mnemonic::Setp => 0x40,
			Mnemonic::Ei => 0xA0,
			Mnemonic::Di => 0xC0,
			Mnemonic::Nop => 0x00,
			Mnemonic::Sleep => 0xEF,
			Mnemonic::Stop => 0xFF,
			_ => unreachable!(),
		},
		label,
	);
}

/// Data in memory while we still need to resolve labels.
/// This data may have an attached label.
#[derive(Clone, Debug)]
pub struct LabeledMemoryValue {
	/// The label of this memory value.
	pub label: Option<Arc<Label>>,
	/// The actual memory value, which might or might not be resolved.
	pub value: MemoryValue,
}

impl LabeledMemoryValue {
	/// Try to resolve this memory value if it has a label. This always does nothing if the data is already resolved.
	/// * `own_memory_address`: The actual location in memory that this value is at. Some resolution strategies need
	///   this.
	#[inline]
	#[must_use]
	pub fn try_resolve(&mut self, own_memory_address: MemoryAddress) -> bool {
		if let MemoryValue::Resolved(_) = self.value {
			false
		} else {
			// FIXME: I can't figure out how to do this without copying first.
			let value_copy = self.value.clone();
			self.value = value_copy.try_resolve(own_memory_address);
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
	LabelHighByte(Arc<Label>),
	/// Low byte of an (unresolved) label.
	LabelLowByte(Arc<Label>),
	/// An (unresolved) label. The resolved memory value will be the difference between this memory value's location and
	/// the label's location. The second parameter is the negative offset that's added to the relative offset after
	/// computation. This is necessary because the actual instruction that has this relative label stored doesn't start
	/// at this memory address, but one or two bytes before. The PSW is still at the beginning of the instruction when
	/// we jump, so the relative jump starts at that instruction, not at this memory address.
	LabelRelative(Arc<Label>, u8),
}

impl MemoryValue {
	#[allow(clippy::match_wildcard_for_single_variants)]
	fn try_resolve(self, own_memory_address: MemoryAddress) -> Self {
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
			Self::LabelRelative(ref label, negative_offset) => match **label {
				Label { location: Some(label_memory_address), .. } => {
					let resolved_data = (label_memory_address - own_memory_address) as u8 + negative_offset;
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
#[derive(Debug)]
pub struct AssembledData {
	/// The data segments. These are checked later when being combined into one.
	pub segments:              BTreeMap<MemoryAddress, Vec<LabeledMemoryValue>>,
	/// The starting address of the current segment. This is the key to the segments map.
	pub current_segment_start: Option<MemoryAddress>,
	/// The source code behind this assembled data
	pub source_code:           Arc<AssemblyCode>,
}

impl AssembledData {
	/// Combine the segments into one binary stream. The result has correct memory addresses, so the first byte is memory
	/// address 0 etc.
	/// # Errors
	/// If the segments contain overlapping data, errors are returned.
	pub fn combine_segments(&self) -> Result<Vec<u8>, AssemblyError> {
		let mut all_data = Vec::new();
		// The iteration is sorted
		for (starting_address, segment_data) in &self.segments {
			if *starting_address < all_data.len() as i64 {
				return Err(AssemblyError::SectionMismatch {
					src:           Arc::new(AssemblyCode {
						text: pretty_hex(&all_data),
						name: self.source_code.name.clone(),
					}),
					// TODO: This location is wrong.
					location:      (*starting_address as usize, 1).into(),
					section_start: *starting_address,
					section_end:   all_data.len() as MemoryAddress,
				});
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
	pub fn new(source_code: Arc<AssemblyCode>) -> Self {
		Self { segments: BTreeMap::default(), current_segment_start: Option::default(), source_code }
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
	pub fn append_16_bits(&mut self, value: MemoryAddress, label: Option<Arc<Label>>) {
		self.append((value & 0xFF) as u8, label);
		self.append(((value & 0xFF00) >> 8) as u8, None);
	}

	/// Appends an 8-bit value to the current segment. The given number is truncated to 8 bits.
	#[inline]
	pub fn append_8_bits(&mut self, value: MemoryAddress, label: Option<Arc<Label>>) {
		self.append((value & 0xFF) as u8, label);
	}

	/// Appends an 8-bit value to the current segment.
	#[inline]
	pub fn append(&mut self, value: u8, label: Option<Arc<Label>>) {
		self.current_segment_mut().push(LabeledMemoryValue { value: MemoryValue::Resolved(value), label });
	}

	/// Appends an unresolved value that points to a label to the current segment. The `use_high_byte` parameter decides
	/// whether the high byte (true) or low byte (false) will be used in this memory address when the label is resolved.
	pub fn append_unresolved(&mut self, value: Arc<Label>, use_high_byte: bool, label: Option<Arc<Label>>) {
		self.current_segment_mut().push(LabeledMemoryValue {
			value: if use_high_byte { MemoryValue::LabelHighByte(value) } else { MemoryValue::LabelLowByte(value) },
			label,
		});
	}

	/// Appends an unresolved value that points to a label to the current segment. The label will be resolved to a
	/// relative offset, like various branch instructions need it.
	pub fn append_relative_unresolved(&mut self, value: Arc<Label>, negative_offset: u8) {
		self
			.current_segment_mut()
			.push(LabeledMemoryValue { value: MemoryValue::LabelRelative(value, negative_offset), label: None });
	}

	/// Appends an instruction with an 8-bit operand.
	#[inline]
	pub fn append_instruction_with_8_bit_operand(&mut self, opcode: u8, operand: Number, label: Option<Arc<Label>>) {
		self.append(opcode, label);
		match operand {
			Number::Literal(value) => self.append_8_bits(value, None),
			Number::Label(value) => self.append_unresolved(value, false, None),
		}
	}

	/// Appends an instruction with two 8-bit operands.
	#[inline]
	pub fn append_instruction_with_two_8_bit_operands(
		&mut self,
		opcode: u8,
		first_operand: Number,
		second_operand: Number,
		label: Option<Arc<Label>>,
	) {
		self.append_instruction_with_8_bit_operand(opcode, first_operand, label);
		match second_operand {
			Number::Literal(value) => self.append_8_bits(value, None),
			Number::Label(value) => self.append_unresolved(value, false, None),
		}
	}

	/// Appends an instruction with an 16-bit operand.
	#[inline]
	pub fn append_instruction_with_16_bit_operand(&mut self, opcode: u8, operand: Number, label: Option<Arc<Label>>) {
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

	/// Appends an instruction with an 8-bit operand. If this is a label, it's stored as a relative unresolved label.
	pub fn append_instruction_with_relative_label(&mut self, opcode: u8, operand: Number, label: Option<Arc<Label>>) {
		self.append(opcode, label);
		match operand {
			Number::Literal(value) => self.append_8_bits(value, None),
			Number::Label(label) => self.append_relative_unresolved(label, 1),
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
					unsafe { Arc::get_mut_unchecked(datum.label.as_mut().unwrap()) }.resolve_to(memory_address);
					had_modifications |= true;
				}
				had_modifications |= datum.try_resolve(memory_address);
			}
		}
		had_modifications
	}
}
