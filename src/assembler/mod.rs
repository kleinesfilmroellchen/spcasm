//! Assembler/codegen
#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss, clippy::wildcard_imports)]

use std::path::PathBuf;
use std::sync::Arc;

use miette::{Result, SourceSpan};
#[allow(unused)]
use smartstring::alias::String;

use crate::cli::{default_backend_options, Frontend};
use crate::error::AssemblyError;
use crate::sema::instruction::{AddressingMode, Instruction, MemoryAddress, Opcode};
use crate::sema::reference::{Reference, Resolvable};
use crate::sema::value::{BinaryOperator, Size, SizedAssemblyTimeValue};
use crate::sema::{AssemblyTimeValue, ProgramElement, Register};
use crate::{pretty_hex, AssemblyCode, Segments};

mod directive;
pub mod memory;
pub mod sample_table;
mod table;

pub use table::assembly_table;
use table::{EntryOrFirstOperandTable, EntryOrSecondOperandTable, TwoOperandEntry};

use self::memory::{LabeledMemoryValue, MemoryValue};

/// Assembles the instructions into a byte sequence. This function receives already-separated sections as input, so it
/// does not do section splitting itself. It might modify the input segments as well during optimization.
///
/// # Errors
/// Unencodeable instructions will cause errors.
#[allow(clippy::trivially_copy_pass_by_ref)]
pub(crate) fn assemble_from_segments(
	segments: &mut Segments<ProgramElement>,
	source_code: &Arc<AssemblyCode>,
	options: Arc<dyn Frontend>,
) -> Result<Vec<u8>, Box<AssemblyError>> {
	assemble_to_data(segments, source_code, options)?.combine_segments()
}

/// Runs the assembler, but does not combine data from different segments afterwards. Therefore, the assembler
/// effectively only runs inside segments. This function might modify the given segments during optimization, and it
/// returns the assembled segments.
///
/// # Errors
/// Unencodeable instructions will cause errors.
#[allow(clippy::trivially_copy_pass_by_ref)]
pub fn assemble_inside_segments(
	segments: &mut Segments<ProgramElement>,
	source_code: &Arc<AssemblyCode>,
	options: Arc<dyn Frontend>,
) -> Result<Segments<u8>, Box<AssemblyError>> {
	assemble_to_data(segments, source_code, options)?.resolve_segments()
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn assemble_to_data(
	segments: &mut Segments<ProgramElement>,
	source_code: &Arc<AssemblyCode>,
	options: Arc<dyn Frontend>,
) -> Result<AssembledData, Box<AssemblyError>> {
	let mut data = AssembledData::new(source_code.clone());
	let maximum_reference_resolution_passes = options.maximum_reference_resolution_passes();
	data.set_error_options(options);
	data.segments.sample_table = segments.sample_table.clone();

	for (segment_start, segment_content) in &mut segments.segments {
		data.segments.new_segment(*segment_start);
		let mut current_labels = Vec::default();
		for program_element in segment_content {
			match program_element {
				ProgramElement::Label(label) => {
					current_labels.push(label.clone());
					continue;
				},
				ProgramElement::Instruction(instruction) => data.assemble_instruction(instruction, &current_labels)?,
				ProgramElement::Directive(directive) => data.assemble_directive(directive, &current_labels)?,
				ProgramElement::IncludeSource { .. } =>
					unreachable!("there should not be any remaining unincluded source code at assembly time"),
				ProgramElement::UserDefinedMacroCall { .. } =>
					unreachable!("there should not be unexpanded user macros at assembly time"),
			}
			if data.should_stop {
				break;
			}
			current_labels.clear();
		}
		if data.should_stop {
			break;
		}
	}

	let mut pass_count = 0;
	while pass_count < maximum_reference_resolution_passes && data.execute_reference_resolution_pass()? {
		pass_count += 1;
	}
	Ok(data)
}

pub(crate) fn resolve_file(source_code: &Arc<AssemblyCode>, target_file: &str) -> PathBuf {
	source_code
		.name
		.clone()
		.parent()
		.map(|directory| directory.to_owned().join(target_file))
		.expect("file path was root, this makes no sense")
}

/// The assembled data, which consists of multiple sections.
#[derive(Debug)]
pub struct AssembledData {
	/// The segment data.
	pub segments:    Segments<LabeledMemoryValue>,
	/// The source code behind this assembled data
	pub source_code: Arc<AssemblyCode>,
	/// Assembler subroutines use this as a flag to signal an end of assembly as soon as possible.
	should_stop:     bool,
	/// Options that command line received; used for determining what to do with warnings.
	options:         Arc<dyn Frontend>,
}

impl AssembledData {
	const DEFAULT_VEC: &'static Vec<Reference> = &Vec::new();

	/// Combine the segments into one binary stream. The result has correct memory addresses, so the first byte is
	/// memory address 0 etc.
	/// # Errors
	/// If the segments contain overlapping data, errors are returned.
	pub fn combine_segments(&self) -> Result<Vec<u8>, Box<AssemblyError>> {
		let mut all_data = Vec::new();
		let segments = self.resolve_segments()?;

		// The iteration is sorted
		for (starting_address, segment_data) in segments.segments {
			if starting_address < all_data.len() as i64 {
				return Err(AssemblyError::SegmentMismatch {
					src:           Arc::new(AssemblyCode {
						text:         pretty_hex(&all_data, Some(starting_address as usize)),
						name:         self.source_code.name.clone(),
						include_path: Vec::new(),
					}),
					// TODO: This location is wrong, it ignores newlines.
					location:      (starting_address as usize * 3 + 1, 2).into(),
					segment_start: starting_address,
					segment_end:   all_data.len() as MemoryAddress,
				}
				.into());
			}
			all_data.resize(starting_address as usize, 0);
			all_data.extend_from_slice(&segment_data);
		}

		Ok(all_data)
	}

	/// Resolve the assembled data's segments by resolving individual memory values. This yields the final data segments
	/// containing raw bytes.
	///
	/// # Errors
	/// If any memory location cannot be resolved to a value.
	#[allow(clippy::missing_panics_doc)]
	pub fn resolve_segments(&self) -> Result<Segments<u8>, Box<AssemblyError>> {
		self.segments.clone().try_map_segments(|start_address, elements| {
			elements
				.iter()
				.enumerate()
				.map(|(address, lmv)| {
					lmv.try_as_resolved(MemoryAddress::try_from(address).unwrap() + start_address, &self.source_code)
				})
				.try_collect::<Vec<u8>>()
		})
	}

	/// Creates new assembled data
	#[must_use]
	#[inline]
	pub fn new(source_code: Arc<AssemblyCode>) -> Self {
		Self { segments: Segments::default(), source_code, should_stop: false, options: default_backend_options() }
	}

	/// Change the error options for assembler warning and error reporting.
	pub fn set_error_options(&mut self, options: Arc<dyn Frontend>) -> &mut Self {
		self.options = options;
		self
	}

	/// Report or throw an error depending on what command-line options this assembly data object knows about. If error
	/// options are not available (on non-clap builds, e.g. tests), this always reports the error.
	/// # Errors
	/// The provided error is re-thrown if the error options specify to do so. On non-clap builds, this function never
	/// errors.
	pub fn report_or_throw(&self, error: AssemblyError) -> Result<(), Box<AssemblyError>> {
		error.report_or_throw(&*self.options)
	}

	/// Assemble a single instruction. This function uses the codegen table `table::assembly_table`.
	#[allow(clippy::unnecessary_wraps, clippy::too_many_lines)]
	fn assemble_instruction(
		&mut self,
		instruction: &mut Instruction,
		current_labels: &[Reference],
	) -> Result<(), Box<AssemblyError>> {
		// Because the actions always expect to get a value, we need a fallback dummy value if there is none in the
		// addressing mode. This is fine, since we control the codegen table and we can make sure that we never use a
		// value where there is none in the operand.
		const dummy_value: AssemblyTimeValue = AssemblyTimeValue::Literal(0);

		let Instruction { opcode: Opcode { first_operand, mnemonic, second_operand, .. }, span, .. } = instruction;

		// Retrieve the table entry for the mnemonic.
		let mnemonic_entry = assembly_table
			.get(mnemonic)
			.unwrap_or_else(|| panic!("No codegen entries for mnemonic {}, this is a bug", mnemonic));

		match mnemonic_entry {
			EntryOrFirstOperandTable::Entry(opcode) =>
				if first_operand.is_some() || second_operand.is_some() {
					Err(AssemblyError::OperandNotAllowed {
						mnemonic: *mnemonic,
						location: *span,
						src:      self.source_code.clone(),
					}
					.into())
				} else {
					self.append_8_bits(MemoryAddress::from(*opcode), current_labels, *span)
				},
			EntryOrFirstOperandTable::Table(first_operand_table) => {
				let mut legal_modes: Vec<_> = first_operand_table.keys().map(|f| f.to_string().into()).collect();
				legal_modes.sort();
				let first_operand = first_operand.as_ref().ok_or_else(|| AssemblyError::MissingOperand {
					mnemonic:    *mnemonic,
					legal_modes: legal_modes.clone(),
					location:    *span,
					src:         self.source_code.clone(),
				})?;

				// Retrieve the table entry for the first operand.
				let first_operand_entry = first_operand_table.get(&first_operand.into()).ok_or_else(|| {
					AssemblyError::InvalidFirstAddressingMode {
						mode: first_operand.to_string().into(),
						mnemonic: *mnemonic,
						legal_modes,
						src: self.source_code.clone(),
						location: *span,
					}
				})?;
				// At this point, we have fully checked the first operand's correctness and can assume that the table
				// doesn't do nonsensical things with its properties, such as the bit index or the value.

				// Check that there is no second operand if we don't need one.
				// However, we don't need to error out if we are missing a second operand, because that will be checked
				// later.
				if !matches!(
					(&second_operand, first_operand_entry),
					(
						None,
						EntryOrSecondOperandTable::Entry(..)
							| EntryOrSecondOperandTable::ImplicitAEntry(..)
							| EntryOrSecondOperandTable::BitEntry(..)
							| EntryOrSecondOperandTable::TcallEntry(..)
					) | (Some(AddressingMode::Register(Register::A)), EntryOrSecondOperandTable::ImplicitAEntry(..))
						| (_, EntryOrSecondOperandTable::Table(..))
				) {
					return Err(AssemblyError::TwoOperandsNotAllowed {
						mnemonic: *mnemonic,
						src:      self.source_code.clone(),
						location: *span,
					}
					.into());
				}

				match first_operand_entry {
					EntryOrSecondOperandTable::Entry(opcode, action)
					| EntryOrSecondOperandTable::ImplicitAEntry(opcode, action) => {
						self.append_8_bits(MemoryAddress::from(*opcode), current_labels, *span)?;
						action(
							self,
							*span,
							first_operand.number_ref().unwrap_or(&dummy_value),
							first_operand.bit_index().or_else(|| mnemonic.bit_index()).unwrap_or(1),
						)
					},
					EntryOrSecondOperandTable::BitEntry(opcode, action) => {
						self.append_unresolved_opcode_with_bit_index(
							AssemblyTimeValue::Literal(MemoryAddress::from(*opcode)),
							first_operand.bit_index().or_else(|| mnemonic.bit_index()).unwrap_or(1),
							current_labels,
							*span,
						)?;
						action(
							self,
							*span,
							first_operand.number_ref().unwrap_or(&dummy_value),
							first_operand.bit_index().or_else(|| mnemonic.bit_index()).unwrap_or(1),
						)
					},
					EntryOrSecondOperandTable::TcallEntry(opcode) => self.append_8_bits_unresolved(
						// Synthesize the operation `opcode | ((operand & 0x0F) << 4)` which is exactly how TCALL
						// works.
						AssemblyTimeValue::BinaryOperation(
							AssemblyTimeValue::Literal(MemoryAddress::from(*opcode)).into(),
							AssemblyTimeValue::BinaryOperation(
								AssemblyTimeValue::BinaryOperation(
									first_operand.number().unwrap_or_else(|| dummy_value.clone()).into(),
									AssemblyTimeValue::Literal(0x0F).into(),
									BinaryOperator::And,
								)
								.into(),
								AssemblyTimeValue::Literal(4).into(),
								BinaryOperator::LeftShift,
							)
							.into(),
							BinaryOperator::Or,
						),
						0,
						current_labels,
						*span,
					),
					EntryOrSecondOperandTable::Table(second_operand_table) => {
						let mut legal_modes: Vec<_> =
							second_operand_table.keys().map(|f| f.to_string().into()).collect();
						legal_modes.sort();
						let second_operand =
							second_operand.as_ref().ok_or_else(|| AssemblyError::MissingSecondOperand {
								mnemonic:    *mnemonic,
								location:    *span,
								legal_modes: legal_modes.clone(),
								src:         self.source_code.clone(),
							})?;

						let second_operand_entry =
							second_operand_table.get(&second_operand.into()).ok_or_else(|| {
								AssemblyError::InvalidSecondAddressingMode {
									mode: second_operand.to_string().into(),
									mnemonic: *mnemonic,
									first_mode: first_operand.to_string().into(),
									legal_modes,
									src: self.source_code.clone(),
									location: *span,
								}
							})?;

						let bit_index = first_operand
							.bit_index()
							.or_else(|| second_operand.bit_index())
							.or_else(|| mnemonic.bit_index())
							.unwrap_or(1);

						match second_operand_entry {
							TwoOperandEntry::Entry(opcode, action) => {
								self.append(*opcode, current_labels, *span)?;
								action(
									self,
									*span,
									first_operand.number_ref().unwrap_or(&dummy_value),
									second_operand.number_ref().unwrap_or(&dummy_value),
									bit_index,
								)
							},
							TwoOperandEntry::BitEntry(opcode, action) => {
								self.append_unresolved_opcode_with_bit_index(
									AssemblyTimeValue::Literal(MemoryAddress::from(*opcode)),
									bit_index,
									current_labels,
									*span,
								)?;
								action(
									self,
									*span,
									first_operand.number_ref().unwrap_or(&dummy_value),
									second_operand.number_ref().unwrap_or(&dummy_value),
									bit_index,
								)
							},
						}
					},
				}
			},
		}
	}

	/// Appends an 8-bit value to the current segment. The given number is truncated to 8 bits.
	///
	/// # Errors
	/// If the given value is too large for the memory address and the related warning is promoted to an error via
	/// command-line arguments, this error will be returned.
	#[inline]
	fn append_8_bits(
		&mut self,
		value: MemoryAddress,
		labels: &[Reference],
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
		self.append((value & 0xFF) as u8, labels, span)
	}

	/// Appends an 8-bit value to the current segment.
	#[inline]
	fn append(&mut self, value: u8, labels: &[Reference], span: SourceSpan) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.segments.current_segment_mut().map_err(|_| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				value:                MemoryValue::Resolved(value),
				labels:               labels.to_owned(),
				instruction_location: span,
			},
		);
		Ok(())
	}

	fn append_bytes(
		&mut self,
		values: Vec<u8>,
		labels: &[Reference],
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let mut is_first = true;
		for value in values {
			self.append(value, if is_first { labels } else { Self::DEFAULT_VEC }, span)?;
			is_first = false;
		}
		Ok(())
	}

	/// Appends an unresolved value to the current segment. The `byte` parameter decides
	/// which byte will be used in this memory address when the reference is resolved.
	///
	/// # Errors
	/// If there is no segment currently.
	fn append_8_bits_unresolved(
		&mut self,
		value: AssemblyTimeValue,
		byte: u8,
		labels: &[Reference],
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.segments.current_segment_mut().map_err(|_| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				value:                MemoryValue::Number(value, byte),
				labels:               labels.to_owned(),
				instruction_location: span,
			},
		);
		Ok(())
	}

	/// Appends an unresolved value that occupies 16 bits (LSB first) to the current segment.
	///
	/// # Errors
	/// If there is no segment currently.
	fn append_16_bits_unresolved(
		&mut self,
		value: AssemblyTimeValue,
		labels: &[Reference],
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		self.append_8_bits_unresolved(value.clone(), 0, labels, span)?;
		self.append_8_bits_unresolved(value, 1, &Vec::default(), span)
	}

	/// Appends an unresolved value to the current segment. The value is sized, which determines how many bytes are
	/// appended.
	///
	/// # Errors
	/// If there is no segment currently.
	fn append_sized_unresolved(
		&mut self,
		value: SizedAssemblyTimeValue,
		labels: &[Reference],
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		match value.size {
			Size::Byte => self.append_8_bits_unresolved(value.value, 0, labels, span),
			Size::Word => self.append_16_bits_unresolved(value.value, labels, span),
			Size::Long => {
				self.append_16_bits_unresolved(value.value.clone(), labels, span)?;
				self.append_8_bits_unresolved(value.value, 2, &Vec::default(), span)
			},
			Size::DWord => {
				self.append_16_bits_unresolved(value.value.clone(), labels, span)?;
				self.append_8_bits_unresolved(value.value.clone(), 2, &Vec::default(), span)?;
				self.append_8_bits_unresolved(value.value, 3, &Vec::default(), span)
			},
		}
	}

	/// Appends an unresolved value to the current segment. The reference will be resolved to a
	/// relative offset, like various branch instructions need it.
	///
	/// # Errors
	/// If there is no segment currently.
	fn append_relative_unresolved(
		&mut self,
		value: AssemblyTimeValue,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.segments.current_segment_mut().map_err(|_| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				labels:               Vec::default(),
				value:                MemoryValue::NumberRelative(value),
				instruction_location: span,
			},
		);
		Ok(())
	}

	/// Appends an unresolved value with a bit index that will be placed into the upper four bits of the value's high
	/// byte after reference resolution. This is intended for the upper half of all mem.bit - like instructions which
	/// encode the bit index in this way.
	///
	/// # Errors
	/// If there is no segment currently.
	fn append_unresolved_with_high_byte_bit_index(
		&mut self,
		value: AssemblyTimeValue,
		bit_index: u8,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.segments.current_segment_mut().map_err(|_| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				value:                MemoryValue::NumberHighByteWithContainedBitIndex(value, bit_index),
				labels:               Vec::default(),
				instruction_location: span,
			},
		);
		Ok(())
	}

	/// Appends an unresolved value with a bit index that will be placed into the upper three bits of the value.
	///
	/// # Errors
	/// If there is no segment currently.
	fn append_unresolved_opcode_with_bit_index(
		&mut self,
		value: AssemblyTimeValue,
		bit_index: u8,
		labels: &[Reference],
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.segments.current_segment_mut().map_err(|_| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				// Synthesize the (bit_index << 5) | value which is needed for bit indices in opcodes.
				value:                MemoryValue::Number(
					AssemblyTimeValue::BinaryOperation(
						value.into(),
						AssemblyTimeValue::Literal(MemoryAddress::from(bit_index) << 5).into(),
						BinaryOperator::Or,
					),
					0,
				),
				labels:               labels.to_owned(),
				instruction_location: span,
			},
		);
		Ok(())
	}

	/// Executes a reference resolution pass. This means the following:
	/// * All data in all segments is traversed. The current memory location is kept track of during traversal.
	/// * All data with a reference has that reference assigned the current memory location.
	/// * All data that references a reference has a resolution attempted, which succeeds if the reference has "gained"
	///   an actual memory location. The reference reference is then gone.
	/// This means that data which uses references declared later needs one additional resolution pass.
	/// # Returns
	/// Whether any modifications were actually done during the resolution pass.
	/// # Errors
	/// Any warnings and warning-promoted errors from resolution are passed on.
	#[allow(clippy::missing_panics_doc)]
	fn execute_reference_resolution_pass(&mut self) -> Result<bool, Box<AssemblyError>> {
		let mut had_modifications = true;
		for (segment_start, segment_data) in &mut self.segments.segments {
			let mut current_global_label = None;
			for (offset, datum) in segment_data.iter_mut().enumerate() {
				let memory_address = segment_start + offset as i64;
				current_global_label = datum
					.labels
					.last()
					.filter(|label| matches!(label, Reference::Label(..)))
					.cloned()
					.or(current_global_label);
				// Resolve the actual reference definition; i.e. if the below code executes, we're at the memory
				// location which is labeled.
				datum
					.labels
					.iter_mut()
					.filter(|existing_reference| !existing_reference.is_resolved())
					.filter_map(|resolved_reference| {
						had_modifications |= true;
						match *resolved_reference {
							Reference::Label(..) | Reference::Relative { .. } => resolved_reference.resolve_to(
								memory_address,
								datum.instruction_location,
								self.source_code.clone(),
							),
							Reference::MacroArgument { value: Some(_), .. } => Ok(()),
							Reference::MacroArgument { value: None, span, .. }
							| Reference::UnresolvedLocalLabel { span, .. } => Err(AssemblyError::UnresolvedReference {
								reference:          resolved_reference.to_string().into(),
								reference_location: None,
								usage_location:     span,
								src:                self.source_code.clone(),
							}
							.into()),
							Reference::MacroGlobal { span } => Err(AssemblyError::UnresolvedMacroGlobal {
								usage_location: span,
								src:            self.source_code.clone(),
							}
							.into()),
						}
						.err()
					})
					.find_map(|err| err.report_or_throw(&*self.options).err())
					.map_or_else(|| Ok(()), Err)?;
				// Resolve a reference used as a memory address, e.g. in an instruction operand like a jump target.
				had_modifications |= datum.try_resolve(memory_address);
			}
		}
		Ok(had_modifications)
	}
}
