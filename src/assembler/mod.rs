//! Assembler/codegen

#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]

use std::collections::BTreeMap;
use std::sync::Arc;

use miette::{Result, SourceSpan};
use r16bit::MovDirection;

use super::r#macro::MacroValue;
use super::{pretty_hex, Macro, ProgramElement};
use crate::error::{AssemblyCode, AssemblyError};
use crate::instruction::{AddressingMode, Instruction, MemoryAddress, Mnemonic, Number, Opcode};
use crate::label::{Label, Resolvable};
use crate::parser::Environment;
use crate::Register;

mod arithmetic_logic;
mod bit;
mod branching;
mod mov;
mod r16bit;

/// Maximum number of resolution passes executed so that no endless resolution loops are hit.
pub const MAX_PASSES: usize = 10;

/// Assembles the instructions into a byte sequence.
/// # Errors
/// Unencodeable instructions will cause errors.
pub fn assemble(environment: &Environment, instructions: &mut Vec<ProgramElement>) -> Result<Vec<u8>, AssemblyError> {
	let mut data = AssembledData::new(environment.source_code.clone());

	data.new_segment(0);

	for program_element in instructions {
		match program_element {
			ProgramElement::Instruction(instruction) => assemble_instruction(&mut data, instruction)?,
			ProgramElement::Macro(r#macro) => assemble_macro(&mut data, r#macro.clone())?,
		}
	}
	let mut pass_count = 0;
	while data.execute_label_resolution_pass() && pass_count < MAX_PASSES {
		pass_count += 1;
	}
	data.combine_segments()
}

#[allow(clippy::too_many_lines)] // ¯\_(ツ)_/¯
fn assemble_instruction(data: &mut AssembledData, instruction: &mut Instruction) -> Result<(), AssemblyError> {
	match instruction.opcode.clone() {
		Opcode { mnemonic: Mnemonic::Mov, first_operand: Some(target), second_operand: Some(source) } =>
			mov::assemble_mov(data, &target, source, instruction)?,
		Opcode {
			mnemonic:
				mnemonic @ (Mnemonic::Adc | Mnemonic::Sbc | Mnemonic::And | Mnemonic::Or | Mnemonic::Eor | Mnemonic::Cmp),
			first_operand: Some(target),
			second_operand: Some(source),
		} => arithmetic_logic::assemble_arithmetic_instruction(data, mnemonic, target, source, instruction)?,
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Inc | Mnemonic::Dec),
			first_operand: Some(target),
			second_operand: None,
		} => {
			let is_increment = mnemonic == Mnemonic::Inc;
			arithmetic_logic::assemble_inc_dec_instruction(data, is_increment, target, instruction)?;
		},
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Asl | Mnemonic::Lsr | Mnemonic::Rol | Mnemonic::Ror | Mnemonic::Xcn),
			first_operand: Some(target),
			second_operand: None,
		} => arithmetic_logic::assemble_shift_rotation_instruction(data, mnemonic, target, instruction)?,
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Incw | Mnemonic::Decw),
			first_operand: Some(AddressingMode::DirectPage(target)),
			second_operand: None,
		} => {
			let is_increment = mnemonic == Mnemonic::Incw;
			r16bit::assemble_incw_decw_instruction(data, is_increment, target, instruction);
		},
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Addw | Mnemonic::Subw | Mnemonic::Cmpw),
			first_operand: Some(AddressingMode::Register(Register::YA)),
			second_operand: Some(AddressingMode::DirectPage(target)),
		} => r16bit::assemble_add_sub_cmp_wide_instruction(data, mnemonic, target, instruction),
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
			let (direction, page_address) = if target == AddressingMode::Register(Register::YA) {
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
			r16bit::assemble_mov_wide_instruction(data, page_address, &direction, instruction);
		},
		Opcode {
			mnemonic: Mnemonic::Mul,
			first_operand: Some(AddressingMode::Register(Register::YA)),
			second_operand: None,
		} => data.append_instruction(0xCF, instruction),
		Opcode {
			mnemonic: Mnemonic::Div,
			first_operand: Some(AddressingMode::Register(Register::YA)),
			second_operand: Some(AddressingMode::Register(Register::X)),
		} => data.append_instruction(0x9E, instruction),
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Daa | Mnemonic::Das),
			first_operand: Some(AddressingMode::Register(Register::A)),
			second_operand: None,
		} => data.append_instruction(if mnemonic == Mnemonic::Daa { 0xDF } else { 0xBE }, instruction),
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
				| Mnemonic::Call
				| Mnemonic::Tcall
				| Mnemonic::Pcall
				| Mnemonic::Jmp),
			first_operand: Some(target),
			second_operand: source,
		} => branching::assemble_branching_instruction(data, mnemonic, target, source, instruction)?,
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
		} => assemble_operandless_instruction(data, mnemonic, instruction),
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Push | Mnemonic::Pop),
			first_operand: Some(AddressingMode::Register(target)),
			second_operand: None,
		} => mov::assemble_push_pop(data, mnemonic == Mnemonic::Push, target, instruction)?,
		Opcode {
			mnemonic:
				mnemonic @ (Mnemonic::Set1
				| Mnemonic::Clr1
				| Mnemonic::Tset1
				| Mnemonic::Tclr1
				| Mnemonic::And1
				| Mnemonic::Or1
				| Mnemonic::Eor1
				| Mnemonic::Not1
				| Mnemonic::Mov1),
			first_operand: Some(target),
			second_operand: source,
		} => bit::assemble_bit_instructions(data, mnemonic, target, &source, instruction)?,
		Opcode { mnemonic, first_operand: Some(_), second_operand: Some(_) } =>
			return Err(AssemblyError::TwoOperandsNotAllowed {
				mnemonic,
				src: data.source_code.clone(),
				location: instruction.span,
			}),
		Opcode { mnemonic, first_operand: Some(_), .. } =>
			return Err(AssemblyError::OperandNotAllowed {
				mnemonic,
				src: data.source_code.clone(),
				location: instruction.span,
			}),
		_ => unreachable!(),
	}
	Ok(())
}

#[allow(clippy::unnecessary_wraps)]
fn assemble_macro(data: &mut AssembledData, mcro: Macro) -> Result<(), AssemblyError> {
	match mcro.value {
		MacroValue::Org(address) => {
			data.new_segment(address);
		},
	}
	Ok(())
}

fn assemble_operandless_instruction(data: &mut AssembledData, mnemonic: Mnemonic, instruction: &mut Instruction) {
	data.append_instruction(
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
		instruction,
	);

	#[cfg(test)]
	{
		instruction.assembled_size = Some(1);
	}
}

/// Data in memory while we still need to resolve labels.
/// This data may have an attached label.
#[derive(Clone, Debug)]
pub struct LabeledMemoryValue {
	/// The label of this memory value.
	pub label:                Option<Label>,
	/// The actual memory value, which might or might not be resolved.
	pub value:                MemoryValue,
	/// The source span of the instruction or macro that was compiled to this memory value.
	pub instruction_location: SourceSpan,
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
	/// # Errors
	/// If the memory value is not resolved, a nice "unresolved label" error is returned.
	#[inline]
	pub fn try_as_resolved(&self, src: &Arc<AssemblyCode>) -> Result<u8, AssemblyError> {
		self.value.try_resolved().map_err(|number| {
			let first_label =
				number.first_label().expect("Number resolution failure was not caused by label; this is a bug!");
			AssemblyError::UnresolvedLabel {
				label:          first_label.clone(),
				label_location: first_label.source_span(),
				usage_location: self.instruction_location,
				src:            src.clone(),
			}
		})
	}
}

/// The internal data held in a byte in memory, which may not be resolved.
#[derive(Clone, Debug)]
pub enum MemoryValue {
	/// Resolved data.
	Resolved(u8),
	/// High byte of an (unresolved) number.
	NumberHighByte(Number),
	/// Low byte of an (unresolved) number.
	NumberLowByte(Number),
	/// An (unresolved) number. The resolved memory value will be the difference between this memory value's location
	/// plus one and the number's location.
	NumberRelative(Number),
	/// An (unresolved) number. The upper three bits are used for the bit index value which can range from 0 to 7. This
	/// is used for most absolute bit addressing modes.
	NumberHighByteWithContainedBitIndex(Number, u8),
}

impl MemoryValue {
	#[allow(clippy::match_wildcard_for_single_variants)]
	fn try_resolve(self, own_memory_address: MemoryAddress) -> Self {
		match self {
			Self::Resolved(_) => self,
			Self::NumberHighByte(number) => match number.try_resolve() {
				Number::Literal(memory_location) => Self::Resolved(((memory_location & 0xFF00) >> 8) as u8),
				resolved => Self::NumberHighByte(resolved),
			},
			Self::NumberLowByte(number) => match number.try_resolve() {
				Number::Literal(memory_location) => Self::Resolved((memory_location & 0xFF) as u8),
				resolved => Self::NumberLowByte(resolved),
			},
			Self::NumberRelative(number) => match number.try_resolve() {
				Number::Literal(label_memory_address) => {
					let resolved_data = (label_memory_address - (own_memory_address + 1)) as u8;
					Self::Resolved(resolved_data)
				},
				resolved => Self::NumberRelative(resolved),
			},
			Self::NumberHighByteWithContainedBitIndex(number, bit_index) => match number.try_resolve() {
				Number::Literal(label_memory_address) => {
					let resolved_data = ((label_memory_address & 0x1F00) >> 8) as u8 | (bit_index << 5);
					Self::Resolved(resolved_data)
				},
				resolved => Self::NumberHighByteWithContainedBitIndex(resolved, bit_index),
			},
		}
	}

	fn try_resolved(&self) -> Result<u8, Number> {
		match self {
			Self::Resolved(value) => Ok(*value),
			Self::NumberHighByte(label)
			| Self::NumberLowByte(label)
			| Self::NumberHighByteWithContainedBitIndex(label, ..)
			| Self::NumberRelative(label) => Err(label.clone()),
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
	/// Combine the segments into one binary stream. The result has correct memory addresses, so the first byte is
	/// memory address 0 etc.
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
			let try_resolve = |lmv: &LabeledMemoryValue| lmv.try_as_resolved(&self.source_code);
			let resolved_segment_data = segment_data.iter().map(try_resolve).try_collect::<Vec<u8>>()?;
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

	/// Returns the current memory location where data is written to.
	/// # Panics
	/// If this assembly data doesn't have a started segment yet.
	#[must_use]
	#[inline]
	pub fn current_location(&self) -> MemoryAddress {
		self.segments[&self.current_segment_start.expect("didn't start a segment yet")].len() as MemoryAddress
			+ self.current_segment_start.unwrap()
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
	pub fn append_16_bits(&mut self, value: MemoryAddress, label: Option<Label>, span: SourceSpan) {
		if (value & 0xFFFF) != value {
			println!(
				"{:?}",
				miette::Report::new(AssemblyError::ValueTooLarge {
					value,
					location: span,
					src: self.source_code.clone(),
					size: 16,
				})
			);
		}
		self.append((value & 0xFF) as u8, label, span);
		self.append(((value & 0xFF00) >> 8) as u8, None, span);
	}

	/// Appends an 8-bit value to the current segment. The given number is truncated to 8 bits.
	#[inline]
	pub fn append_8_bits(&mut self, value: MemoryAddress, label: Option<Label>, span: SourceSpan) {
		if (value & 0xFF) != value {
			println!(
				"{:?}",
				miette::Report::new(AssemblyError::ValueTooLarge {
					value,
					location: span,
					src: self.source_code.clone(),
					size: 8,
				})
			);
		}
		self.append((value & 0xFF) as u8, label, span);
	}

	/// Appends the opcode of an instruction that doesn't take any operands.
	#[inline]
	pub fn append_instruction(&mut self, opcode: u8, instruction: &mut Instruction) {
		self.append(opcode, instruction.label.clone(), instruction.span);

		#[cfg(test)]
		{
			instruction.assembled_size = Some(1);
		}
	}

	/// Appends an 8-bit value to the current segment.
	#[inline]
	fn append(&mut self, value: u8, label: Option<Label>, span: SourceSpan) {
		self.current_segment_mut().push(LabeledMemoryValue {
			value: MemoryValue::Resolved(value),
			label,
			instruction_location: span,
		});
	}

	/// Appends an unresolved value that points to a label to the current segment. The `use_high_byte` parameter decides
	/// whether the high byte (true) or low byte (false) will be used in this memory address when the label is resolved.
	pub fn append_unresolved(&mut self, value: Number, use_high_byte: bool, label: Option<Label>, span: SourceSpan) {
		self.current_segment_mut().push(LabeledMemoryValue {
			value: if use_high_byte { MemoryValue::NumberHighByte(value) } else { MemoryValue::NumberLowByte(value) },
			label,
			instruction_location: span,
		});
	}

	/// Appends an unresolved value that points to a label to the current segment. The label will be resolved to a
	/// relative offset, like various branch instructions need it.
	pub fn append_relative_unresolved(&mut self, value: Number, span: SourceSpan) {
		self.current_segment_mut().push(LabeledMemoryValue {
			value:                MemoryValue::NumberRelative(value),
			label:                None,
			instruction_location: span,
		});
	}

	/// Appends an unresolved value with a bit index that will be placed into the upper three bits after label
	/// resolution.
	pub fn append_unresolved_with_bit_index(&mut self, value: Number, bit_index: u8, span: SourceSpan) {
		self.current_segment_mut().push(LabeledMemoryValue {
			value:                MemoryValue::NumberHighByteWithContainedBitIndex(value, bit_index),
			label:                None,
			instruction_location: span,
		});
	}

	/// Appends an instruction with an 8-bit operand.
	#[inline]
	pub fn append_instruction_with_8_bit_operand(
		&mut self,
		opcode: u8,
		operand: Number,
		instruction: &mut Instruction,
	) {
		self.append(opcode, instruction.label.clone(), instruction.span);
		match operand.try_resolve() {
			Number::Literal(value) => self.append_8_bits(value, None, instruction.span),
			value => self.append_unresolved(value, false, None, instruction.span),
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(2);
		}
	}

	/// Appends an instruction with two 8-bit operands.
	/// Note that the second machine operand is given first, as most *assembly code* mnemonics have the second *machine
	/// code* operand first. There are exceptions, like BBS and BBC, but standard MOV/ADD/... have target, source while
	/// their machine code has source, target.
	#[inline]
	pub fn append_instruction_with_two_8_bit_operands(
		&mut self,
		opcode: u8,
		second_machine_operand: Number,
		first_machine_operand: Number,
		instruction: &mut Instruction,
	) {
		// The operands are flipped in machine code from what the assembly does. It's not target, source; it's source,
		// target.
		self.append_instruction_with_8_bit_operand(opcode, first_machine_operand, instruction);
		match second_machine_operand.try_resolve() {
			Number::Literal(value) => self.append_8_bits(value, None, instruction.span),
			value => self.append_unresolved(value, false, None, instruction.span),
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(3);
		}
	}

	/// Appends an instruction with an 16-bit operand.
	#[inline]
	pub fn append_instruction_with_16_bit_operand(
		&mut self,
		opcode: u8,
		operand: Number,
		instruction: &mut Instruction,
	) {
		self.append(opcode, instruction.label.clone(), instruction.span);
		match operand.try_resolve() {
			Number::Literal(value) => self.append_16_bits(value, None, instruction.span),
			value => {
				// low byte first because little endian
				self.append_unresolved(value.clone(), false, None, instruction.span);
				self.append_unresolved(value, true, None, instruction.span);
			},
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(3);
		}
	}

	/// Appends an instruction with a 16-bit operand. The upper three bits of it are replaced by the bit index, either
	/// now (if the operand is a resolved number) or later (if the operand is a label).
	#[inline]
	pub fn append_instruction_with_16_bit_operand_and_bit_index(
		&mut self,
		opcode: u8,
		operand: Number,
		bit_index: u8,
		instruction: &mut Instruction,
	) {
		self.append(opcode, instruction.label.clone(), instruction.span);

		match operand.try_resolve() {
			Number::Literal(value) =>
				self.append_16_bits(value | (MemoryAddress::from(bit_index) << 13), None, instruction.span),
			value => {
				self.append_unresolved(value.clone(), false, None, instruction.span);
				self.append_unresolved_with_bit_index(value, bit_index, instruction.span);
			},
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(3);
		}
	}

	/// Appends an instruction with an 8-bit operand. If this is a label, it's stored as a relative unresolved label.
	pub fn append_instruction_with_relative_label(
		&mut self,
		opcode: u8,
		operand: Number,
		instruction: &mut Instruction,
	) {
		self.append(opcode, instruction.label.clone(), instruction.span);
		match operand.try_resolve() {
			Number::Literal(value) => self.append_8_bits(value, None, instruction.span),
			value => self.append_relative_unresolved(value, instruction.span),
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(2);
		}
	}

	/// Executes a label resolution pass. This means the following:
	/// * All data in all segments is traversed. The current memory location is kept track of during traversal.
	/// * All data with a label has that label assigned the current memory location.
	/// * All data that references a label has a resolution attempted, which succeeds if the label has "gained" an
	///   actual memory location. The label reference is then gone.
	/// This means that data which references labels declared later needs one additional resolution pass.
	/// # Returns
	/// Whether any modifications were actually done during the resolution pass.
	#[must_use]
	#[allow(clippy::missing_panics_doc)]
	pub fn execute_label_resolution_pass(&mut self) -> bool {
		let mut had_modifications = true;
		for (segment_start, segment_data) in &mut self.segments {
			let mut current_global_label = None;
			for (offset, datum) in segment_data.iter_mut().enumerate() {
				let memory_address = segment_start + offset as i64;
				current_global_label =
					datum.label.clone().filter(|label| matches!(label, Label::Global(..))).or(current_global_label);
				// Resolve the actual label definition; i.e. if the below code executes, we're at the memory location
				// which is labeled.
				datum.label.as_mut().filter(|existing_label| !existing_label.is_resolved()).map(|resolved_label| {
					match *resolved_label {
						Label::Global(ref mut global) => {
							// Modifying Rc-contained data is dangerous in general, but safe for labels if we don't
							// modify the name.
							unsafe { Arc::get_mut_unchecked(global) }
								.resolve_to(memory_address, self.source_code.clone());
							had_modifications |= true;
							resolved_label
						},
						Label::Local(ref mut local) => {
							local.resolve_to(memory_address, self.source_code.clone());
							let mut parent = local.strong_parent();
							unsafe { Arc::get_mut_unchecked(&mut parent) }
								.locals
								.insert(local.name.clone(), local.clone());
							resolved_label
						},
					}
				});
				// Resolve a label used as a memory address, e.g. in an instruction operand like a jump target.
				had_modifications |= datum.try_resolve(memory_address);
			}
		}
		had_modifications
	}
}
