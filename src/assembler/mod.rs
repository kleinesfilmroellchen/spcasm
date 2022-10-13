//! Assembler/codegen
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss, clippy::wildcard_imports)]

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::sync::Arc;

use miette::{Result, SourceSpan};
use r16bit::MovDirection;

use crate::brr::{self, wav};
use crate::cli::ErrorOptions;
use crate::error::{AssemblyCode, AssemblyError};
use crate::mcro::MacroValue;
use crate::parser::instruction::{AddressingMode, Instruction, MemoryAddress, Mnemonic, Number, Opcode};
use crate::parser::label::{Label, Resolvable};
use crate::parser::{AssemblyFile, ProgramElement, Register};
use crate::{pretty_hex, Macro};

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
#[allow(clippy::trivially_copy_pass_by_ref)]
pub(crate) fn assemble(
	main_file: &Arc<RefCell<AssemblyFile>>,
	options: &ErrorOptions,
) -> Result<Vec<u8>, Box<AssemblyError>> {
	let mut main_file = main_file.borrow_mut();
	let mut data = AssembledData::new(main_file.source_code.clone());
	#[cfg(feature = "binaries")]
	data.set_error_options(options.clone());

	data.new_segment(0);

	for program_element in &mut main_file.content {
		match program_element {
			ProgramElement::Instruction(instruction) => assemble_instruction(&mut data, instruction)?,
			ProgramElement::Macro(r#macro) => assemble_macro(&mut data, r#macro)?,
			ProgramElement::IncludeSource { .. } =>
				unreachable!("there should not be any remaining unincluded source code at assembly time"),
			ProgramElement::UserDefinedMacroCall { .. } =>
				unreachable!("there should not be unexpanded user macros at assembly time"),
		}
		if data.should_stop {
			break;
		}
	}
	let mut pass_count = 0;
	while data.execute_label_resolution_pass()? && pass_count < MAX_PASSES {
		pass_count += 1;
	}
	data.combine_segments()
}

#[allow(clippy::too_many_lines)] // ¯\_(ツ)_/¯
fn assemble_instruction(data: &mut AssembledData, instruction: &mut Instruction) -> Result<(), Box<AssemblyError>> {
	match instruction.opcode.clone() {
		Opcode { mnemonic: Mnemonic::Mov, first_operand: Some(target), second_operand: Some(source), .. } =>
			mov::assemble_mov(data, &target, source, instruction)?,
		Opcode {
			mnemonic:
				mnemonic @ (Mnemonic::Adc | Mnemonic::Sbc | Mnemonic::And | Mnemonic::Or | Mnemonic::Eor | Mnemonic::Cmp),
			first_operand: Some(target),
			second_operand: Some(source),
			..
		} => arithmetic_logic::assemble_arithmetic_instruction(data, mnemonic, target, source, instruction)?,
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Inc | Mnemonic::Dec),
			first_operand: Some(target),
			second_operand: None,
			..
		} => {
			let is_increment = mnemonic == Mnemonic::Inc;
			arithmetic_logic::assemble_inc_dec_instruction(data, is_increment, target, instruction)?;
		},
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Asl | Mnemonic::Lsr | Mnemonic::Rol | Mnemonic::Ror | Mnemonic::Xcn),
			first_operand: Some(target),
			second_operand: None,
			..
		} => arithmetic_logic::assemble_shift_rotation_instruction(data, mnemonic, target, instruction)?,
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Incw | Mnemonic::Decw),
			first_operand: Some(AddressingMode::DirectPage(target)),
			second_operand: None,
			..
		} => {
			let is_increment = mnemonic == Mnemonic::Incw;
			r16bit::assemble_incw_decw_instruction(data, is_increment, target, instruction);
		},
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Addw | Mnemonic::Subw | Mnemonic::Cmpw),
			first_operand: Some(AddressingMode::Register(Register::YA)),
			second_operand: Some(AddressingMode::DirectPage(target)),
			..
		} => r16bit::assemble_add_sub_cmp_wide_instruction(data, mnemonic, target, instruction),
		Opcode {
			mnemonic: Mnemonic::Movw,
			first_operand: Some(target @ (AddressingMode::DirectPage(_) | AddressingMode::Register(Register::YA))),
			second_operand: Some(source @ (AddressingMode::DirectPage(_) | AddressingMode::Register(Register::YA))),
			..
		} => {
			let make_movw_error = || {
				Err(AssemblyError::InvalidAddressingModeCombination {
					first_mode:  target.to_string(),
					second_mode: source.to_string(),
					src:         data.source_code.clone(),
					location:    instruction.span,
					mnemonic:    Mnemonic::Movw,
				}
				.into())
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
			..
		} => data.append_instruction(0xCF, instruction),
		Opcode {
			mnemonic: Mnemonic::Div,
			first_operand: Some(AddressingMode::Register(Register::YA)),
			second_operand: Some(AddressingMode::Register(Register::X)),
			..
		} => data.append_instruction(0x9E, instruction),
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Daa | Mnemonic::Das),
			first_operand: Some(AddressingMode::Register(Register::A)),
			second_operand: None,
			..
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
			..
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
			..
		} => assemble_operandless_instruction(data, mnemonic, instruction),
		Opcode {
			mnemonic: mnemonic @ (Mnemonic::Push | Mnemonic::Pop),
			first_operand: Some(AddressingMode::Register(target)),
			second_operand: None,
			..
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
			..
		} => bit::assemble_bit_instructions(data, mnemonic, target, &source, instruction)?,
		Opcode { mnemonic, first_operand: Some(_), second_operand: Some(_), .. } =>
			return Err(AssemblyError::TwoOperandsNotAllowed {
				mnemonic,
				src: data.source_code.clone(),
				location: instruction.span,
			}
			.into()),
		Opcode { mnemonic, first_operand: Some(_), .. } =>
			return Err(AssemblyError::OperandNotAllowed {
				mnemonic,
				src: data.source_code.clone(),
				location: instruction.span,
			}
			.into()),
		Opcode { mnemonic, .. } =>
			return Err(AssemblyError::MissingOperand {
				mnemonic,
				src: data.source_code.clone(),
				location: instruction.span,
			}
			.into()),
	}
	Ok(())
}

#[allow(clippy::unnecessary_wraps)]
fn assemble_macro(data: &mut AssembledData, mcro: &mut Macro) -> Result<(), Box<AssemblyError>> {
	match mcro.value {
		MacroValue::Org(address) => {
			data.new_segment(address);
		},
		MacroValue::Table { entry_size, ref values } => {
			let mut label = mcro.label.clone();
			for value in values {
				match entry_size {
					1 => data.append_8_bits_unresolved(value.clone(), 0, label, mcro.span)?,
					2 => data.append_16_bits_unresolved(value.clone(), label, mcro.span)?,
					3 | 4 => unimplemented!(),
					_ => unreachable!(),
				}
				label = None;
			}
		},
		MacroValue::Brr(ref file_name) => {
			// Resolve the audio file's path relative to the source file.
			let actual_path = resolve_file(&data.source_code, mcro.span, file_name)?;
			let file = File::open(actual_path).map_err(|os_error| AssemblyError::FileNotFound {
				os_error,
				file_name: file_name.clone(),
				src: data.source_code.clone(),
				location: mcro.span,
			})?;
			let mut sample_data =
				wav::read_wav_for_brr(file).map_err(|error_text| AssemblyError::AudioProcessingError {
					error_text,
					file_name: file_name.clone(),
					src: data.source_code.clone(),
					location: mcro.span,
				})?;
			let encoded = brr::encode_to_brr(&mut sample_data, false, brr::CompressionLevel::Max);

			data.append_bytes(encoded, &mcro.label, mcro.span);
		},
		MacroValue::String { ref text, has_null_terminator } => {
			let mut is_first = true;
			for chr in text {
				data.append(*chr, if is_first { mcro.label.clone() } else { None }, mcro.span);
				is_first = false;
			}
			if has_null_terminator {
				data.append(0, if is_first { mcro.label.clone() } else { None }, mcro.span);
			}
		},
		MacroValue::AssignLabel { ref mut label, ref value } => match label {
			Label::Local(label) => {
				label.borrow_mut().location = Some(Box::new(value.clone().try_resolve()));
			},
			Label::Global(ref mut global) => {
				global.borrow_mut().location = Some(value.clone().try_resolve());
			},
			Label::MacroArgument { span, name, .. } =>
				return Err(AssemblyError::AssigningToMacroArgument {
					name:     (*name).to_string(),
					src:      data.source_code.clone(),
					location: *span,
				}
				.into()),
		},
		MacroValue::Include { ref file, range } => {
			let binary_file = resolve_file(&data.source_code, mcro.span, file)?;
			let mut binary_data = std::fs::read(binary_file).map_err(|os_error| AssemblyError::FileNotFound {
				os_error,
				file_name: file.clone(),
				src: data.source_code.clone(),
				location: mcro.span,
			})?;
			if let Some(range) = range {
				let max_number_of_bytes = binary_data.len().saturating_sub(range.offset());
				binary_data = binary_data
					.get(range.offset() .. range.offset().saturating_add(range.len()).min(max_number_of_bytes))
					.ok_or(AssemblyError::RangeOutOfBounds {
						start:    range.offset(),
						end:      range.offset() + range.len(),
						file:     file.clone(),
						file_len: binary_data.len(),
						src:      data.source_code.clone(),
						location: mcro.span,
					})?
					.to_vec();
			}

			data.append_bytes(binary_data, &mcro.label, mcro.span);
		},
		MacroValue::End => {
			data.should_stop = true;
		},
		MacroValue::PopSection => data
			.pop_segment()
			.map_err(|_| AssemblyError::NoSegmentOnStack { location: mcro.span, src: data.source_code.clone() })?,
		MacroValue::PushSection => data
			.push_segment()
			.map_err(|_| AssemblyError::MissingSegment { location: mcro.span, src: data.source_code.clone() })?,
		MacroValue::UserDefinedMacro { .. } => {},
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

pub(crate) fn resolve_file(
	source_code: &Arc<AssemblyCode>,
	span: SourceSpan,
	target_file: &str,
) -> Result<PathBuf, Box<AssemblyError>> {
	source_code.name.clone().parent().map(|directory| directory.to_owned().join(target_file)).ok_or_else(|| {
		{
			AssemblyError::FileNotFound {
				os_error:  std::io::Error::new(ErrorKind::NotFound, "no parent directory for source file"),
				file_name: source_code.file_name(),
				src:       source_code.clone(),
				location:  span,
			}
		}
		.into()
	})
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
	pub fn try_as_resolved(&self, src: &Arc<AssemblyCode>) -> Result<u8, Box<AssemblyError>> {
		self.value.try_resolved(self.instruction_location, src).map_err(|number| {
			{
				let first_label =
					number.first_label().expect("Number resolution failure was not caused by label; this is a bug!");
				AssemblyError::UnresolvedLabel {
					label:          first_label.to_string(),
					label_location: first_label.source_span(),
					usage_location: self.instruction_location,
					src:            src.clone(),
				}
			}
			.into()
		})
	}
}

/// The internal data held in a byte in memory, which may not be resolved.
#[derive(Clone, Debug)]
pub enum MemoryValue {
	/// Resolved data.
	Resolved(u8),
	/// Some byte of an (unresolved) number. The u8 is the byte index, where 0 means the lowest byte, 1 means the
	/// second-lowest byte etc.
	Number(Number, u8),
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
			Self::Number(number, byte) => match number.try_resolve() {
				Number::Literal(memory_location) =>
					Self::Resolved(((memory_location & (0xFF << (byte * 8))) >> (byte * 8)) as u8),
				resolved => Self::Number(resolved, byte),
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

	#[allow(clippy::missing_const_for_fn)]
	fn try_resolved(&self, location: SourceSpan, source_code: &Arc<AssemblyCode>) -> Result<u8, Number> {
		match self {
			Self::Resolved(value) => Ok(*value),
			Self::Number(number, ..)
			| Self::NumberHighByteWithContainedBitIndex(number, ..)
			| Self::NumberRelative(number) => Err(number.clone()),
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
	/// The stack of saved segments, manipulated with pushpc/pullpc.
	pub segment_stack:         Vec<MemoryAddress>,
	/// The source code behind this assembled data
	pub source_code:           Arc<AssemblyCode>,
	/// Assembler subroutines use this as a flag to signal an end of assembly as soon as possible.
	should_stop:               bool,
	/// Options that command line received; used for determining what to do with warnings.
	options:                   ErrorOptions,
}

impl AssembledData {
	/// Combine the segments into one binary stream. The result has correct memory addresses, so the first byte is
	/// memory address 0 etc.
	/// # Errors
	/// If the segments contain overlapping data, errors are returned.
	pub fn combine_segments(&self) -> Result<Vec<u8>, Box<AssemblyError>> {
		let mut all_data = Vec::new();
		// The iteration is sorted
		for (starting_address, segment_data) in &self.segments {
			if *starting_address < all_data.len() as i64 {
				return Err(AssemblyError::SegmentMismatch {
					src:           Arc::new(AssemblyCode {
						text:         pretty_hex(&all_data),
						name:         self.source_code.name.clone(),
						include_path: Vec::new(),
					}),
					// TODO: This location is wrong, it ignores newlines.
					location:      (*starting_address as usize * 3 + 1, 2).into(),
					segment_start: *starting_address,
					segment_end:   all_data.len() as MemoryAddress,
				}
				.into());
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
		Self {
			segments: BTreeMap::default(),
			current_segment_start: Option::default(),
			source_code,
			should_stop: false,
			options: ErrorOptions::default(),
			segment_stack: Vec::new(),
		}
	}

	/// Change the error options for assembler warning and error reporting.
	#[cfg(feature = "binaries")]
	pub fn set_error_options(&mut self, options: ErrorOptions) -> &mut Self {
		self.options = options;
		self
	}

	/// Report or throw an error depending on what command-line options this assembly data object knows about. If error
	/// options are not available (on non-clap builds, e.g. tests), this always reports the error.
	/// # Errors
	/// The provided error is re-thrown if the error options specify to do so. On non-clap builds, this function never
	/// errors.
	pub fn report_or_throw(&self, error: AssemblyError) -> Result<(), Box<AssemblyError>> {
		#[cfg(feature = "binaries")]
		{
			error.report_or_throw(&self.options)
		}
		#[cfg(not(feature = "binaries"))]
		{
			println!("{:?}", miette::Report::new(error));
			Ok(())
		}
	}

	/// Push the current segment onto the segment stack, leaving the current segment vacant.
	///
	/// # Errors
	/// If there is no current segment.
	#[allow(clippy::result_unit_err)]
	pub fn push_segment(&mut self) -> Result<(), ()> {
		self.segment_stack.push(self.current_segment_start.ok_or(())?);
		self.current_segment_start = None;
		Ok(())
	}

	/// Pop the current segment off the stack, re-enabling it.
	///
	/// # Errors
	/// If there is no segment on the stack.
	#[allow(clippy::result_unit_err)]
	pub fn pop_segment(&mut self) -> Result<(), ()> {
		if self.segment_stack.is_empty() {
			return Err(());
		}
		self.current_segment_start = self.segment_stack.pop();
		Ok(())
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
	/// # Errors
	/// If this assembly data doesn't have a started segment yet.
	#[inline]
	#[allow(clippy::result_unit_err)]
	pub fn current_segment(&self) -> Result<&Vec<LabeledMemoryValue>, ()> {
		Ok(&self.segments[&self.current_segment_start.ok_or(())?])
	}

	/// Returns the current memory location where data is written to.
	/// # Errors
	/// If this assembly data doesn't have a started segment yet.
	#[inline]
	#[allow(clippy::result_unit_err, clippy::missing_panics_doc)]
	pub fn current_location(&self) -> Result<MemoryAddress, ()> {
		Ok(self.segments[&self.current_segment_start.ok_or(())?].len() as MemoryAddress
			+ self.current_segment_start.unwrap())
	}

	/// Returns a mutable reference to the data of the current segment.
	/// # Errors
	/// If this assembly data doesn't have a started segment yet.
	#[inline]
	#[allow(clippy::result_unit_err, clippy::missing_panics_doc)]
	pub fn current_segment_mut(&mut self) -> Result<&mut Vec<LabeledMemoryValue>, ()> {
		Ok(self.segments.get_mut(&self.current_segment_start.ok_or(())?).unwrap())
	}

	/// Appends a little endian (LSB first) 16-bit value to the current segment. The given number is truncated to 16
	/// bits.
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	#[inline]
	pub fn append_16_bits(
		&mut self,
		value: MemoryAddress,
		label: Option<Label>,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		if (value & 0xFFFF) != value {
			self.report_or_throw(AssemblyError::ValueTooLarge {
				value,
				location: span,
				src: self.source_code.clone(),
				size: 16,
			})?;
		}
		self.append((value & 0xFF) as u8, label, span);
		self.append(((value & 0xFF00) >> 8) as u8, None, span);
		Ok(())
	}

	/// Appends an 8-bit value to the current segment. The given number is truncated to 8 bits.
	///
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	#[inline]
	pub fn append_8_bits(
		&mut self,
		value: MemoryAddress,
		label: Option<Label>,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		if (value & 0xFF) != value {
			self.report_or_throw(AssemblyError::ValueTooLarge {
				value,
				location: span,
				src: self.source_code.clone(),
				size: 8,
			})?;
		}
		self.append((value & 0xFF) as u8, label, span);
		Ok(())
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
	fn append(&mut self, value: u8, label: Option<Label>, span: SourceSpan) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.current_segment_mut()
			.map_err(|_| AssemblyError::MissingSegment { location: span, src })?
			.push(LabeledMemoryValue { value: MemoryValue::Resolved(value), label, instruction_location: span });
		Ok(())
	}

	fn append_bytes(
		&mut self,
		values: Vec<u8>,
		label: &Option<Label>,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let mut is_first = true;
		for value in values {
			self.append(value, if is_first { label.clone() } else { None }, span)?;
			is_first = false;
		}
		Ok(())
	}

	/// Appends an unresolved value to the current segment. The `byte` parameter decides
	/// which byte will be used in this memory address when the label is resolved.
	///
	/// # Errors
	/// If there is no segment currently.
	pub fn append_8_bits_unresolved(
		&mut self,
		value: Number,
		byte: u8,
		label: Option<Label>,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.current_segment_mut()
			.map_err(|_| AssemblyError::MissingSegment { location: span, src })?
			.push(LabeledMemoryValue { value: MemoryValue::Number(value, byte), label, instruction_location: span });
		Ok(())
	}

	/// Appends an unresolved value that occupies 16 bits (LSB first) to the current segment.
	///
	/// # Errors
	/// If there is no segment currently.
	pub fn append_16_bits_unresolved(
		&mut self,
		value: Number,
		label: Option<Label>,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		self.append_8_bits_unresolved(value.clone(), 0, label, span)?;
		self.append_8_bits_unresolved(value, 1, None, span)
	}

	/// Appends an unresolved value to the current segment. The label will be resolved to a
	/// relative offset, like various branch instructions need it.
	///
	/// # Errors
	/// If there is no segment currently.
	pub fn append_relative_unresolved(&mut self, value: Number, span: SourceSpan) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.current_segment_mut().map_err(|_| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				label:                None,
				value:                MemoryValue::NumberRelative(value),
				instruction_location: span,
			},
		);
		Ok(())
	}

	/// Appends an unresolved value with a bit index that will be placed into the upper three bits after label
	/// resolution.
	///
	/// # Errors
	/// If there is no segment currently.
	pub fn append_unresolved_with_bit_index(
		&mut self,
		value: Number,
		bit_index: u8,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.current_segment_mut().map_err(|_| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				value:                MemoryValue::NumberHighByteWithContainedBitIndex(value, bit_index),
				label:                None,
				instruction_location: span,
			},
		);
		Ok(())
	}

	/// Appends an instruction with an 8-bit operand.
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	#[inline]
	pub fn append_instruction_with_8_bit_operand(
		&mut self,
		opcode: u8,
		operand: Number,
		instruction: &mut Instruction,
	) -> Result<(), Box<AssemblyError>> {
		self.append(opcode, instruction.label.clone(), instruction.span);
		match operand.try_resolve() {
			Number::Literal(value) => self.append_8_bits(value, None, instruction.span)?,
			value => self.append_8_bits_unresolved(value, 0, None, instruction.span)?,
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(2);
		}
		Ok(())
	}

	/// Appends an instruction with two 8-bit operands.
	/// Note that the second machine operand is given first, as most *assembly code* mnemonics have the second *machine
	/// code* operand first. There are exceptions, like BBS and BBC, but standard MOV/ADD/... have target, source while
	/// their machine code has source, target.
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	#[inline]
	pub fn append_instruction_with_two_8_bit_operands(
		&mut self,
		opcode: u8,
		second_machine_operand: Number,
		first_machine_operand: Number,
		instruction: &mut Instruction,
	) -> Result<(), Box<AssemblyError>> {
		// The operands are flipped in machine code from what the assembly does. It's not target, source; it's source,
		// target.
		self.append_instruction_with_8_bit_operand(opcode, first_machine_operand, instruction);
		match second_machine_operand.try_resolve() {
			Number::Literal(value) => self.append_8_bits(value, None, instruction.span)?,
			value => self.append_8_bits_unresolved(value, 0, None, instruction.span)?,
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(3);
		}
		Ok(())
	}

	/// Appends an instruction with an 16-bit operand.
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	#[inline]
	pub fn append_instruction_with_16_bit_operand(
		&mut self,
		opcode: u8,
		operand: Number,
		instruction: &mut Instruction,
	) -> Result<(), Box<AssemblyError>> {
		self.append(opcode, instruction.label.clone(), instruction.span);
		match operand.try_resolve() {
			Number::Literal(value) => self.append_16_bits(value, None, instruction.span)?,
			value => self.append_16_bits_unresolved(value, None, instruction.span)?,
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(3);
		}

		Ok(())
	}

	/// Appends an instruction with a 16-bit operand. The upper three bits of it are replaced by the bit index, either
	/// now (if the operand is a resolved number) or later (if the operand is a label).
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	#[inline]
	pub fn append_instruction_with_16_bit_operand_and_bit_index(
		&mut self,
		opcode: u8,
		operand: Number,
		bit_index: u8,
		instruction: &mut Instruction,
	) -> Result<(), Box<AssemblyError>> {
		self.append(opcode, instruction.label.clone(), instruction.span);

		match operand.try_resolve() {
			Number::Literal(value) =>
				self.append_16_bits(value | (MemoryAddress::from(bit_index) << 13), None, instruction.span)?,
			value => {
				self.append_8_bits_unresolved(value.clone(), 0, None, instruction.span);
				self.append_unresolved_with_bit_index(value, bit_index, instruction.span);
			},
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(3);
		}
		Ok(())
	}

	/// Appends an instruction with an 8-bit operand. If this is a label, it's stored as a relative unresolved label.
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	pub fn append_instruction_with_relative_label(
		&mut self,
		opcode: u8,
		operand: Number,
		instruction: &mut Instruction,
	) -> Result<(), Box<AssemblyError>> {
		self.append(opcode, instruction.label.clone(), instruction.span);
		match operand.try_resolve() {
			Number::Literal(value) => self.append_8_bits(value, None, instruction.span)?,
			value => self.append_relative_unresolved(value, instruction.span)?,
		}

		#[cfg(test)]
		{
			instruction.assembled_size = Some(2);
		}
		Ok(())
	}

	/// Executes a label resolution pass. This means the following:
	/// * All data in all segments is traversed. The current memory location is kept track of during traversal.
	/// * All data with a label has that label assigned the current memory location.
	/// * All data that references a label has a resolution attempted, which succeeds if the label has "gained" an
	///   actual memory location. The label reference is then gone.
	/// This means that data which references labels declared later needs one additional resolution pass.
	/// # Returns
	/// Whether any modifications were actually done during the resolution pass.
	/// # Errors
	/// Any warnings and warning-promoted errors from resolution are passed on.
	#[allow(clippy::missing_panics_doc)]
	pub fn execute_label_resolution_pass(&mut self) -> Result<bool, Box<AssemblyError>> {
		let mut had_modifications = true;
		for (segment_start, segment_data) in &mut self.segments {
			let mut current_global_label = None;
			for (offset, datum) in segment_data.iter_mut().enumerate() {
				let memory_address = segment_start + offset as i64;
				current_global_label =
					datum.label.clone().filter(|label| matches!(label, Label::Global(..))).or(current_global_label);
				// Resolve the actual label definition; i.e. if the below code executes, we're at the memory location
				// which is labeled.
				datum
					.label
					.as_mut()
					.filter(|existing_label| !existing_label.is_resolved())
					.and_then(|resolved_label| {
						had_modifications |= true;
						match *resolved_label {
							Label::Global(ref mut global) => global.borrow_mut().resolve_to(
								memory_address,
								datum.instruction_location,
								self.source_code.clone(),
							),
							Label::Local(ref mut local) => local.borrow_mut().resolve_to(
								memory_address,
								datum.instruction_location,
								self.source_code.clone(),
							),
							Label::MacroArgument { value: Some(_), .. } => Ok(()),
							Label::MacroArgument { value: None, span, .. } => Err(AssemblyError::UnresolvedLabel {
								label:          resolved_label.to_string(),
								label_location: span,
								usage_location: span,
								src:            self.source_code.clone(),
							}
							.into()),
						}
						.err()
					})
					.map_or_else(|| Ok(()), |err| err.report_or_throw(&self.options))?;
				// Resolve a label used as a memory address, e.g. in an instruction operand like a jump target.
				had_modifications |= datum.try_resolve(memory_address);
			}
		}
		Ok(had_modifications)
	}
}
