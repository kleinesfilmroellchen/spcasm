//! Assembler/codegen
#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss, clippy::wildcard_imports)]

use std::path::PathBuf;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use miette::{Result, SourceSpan};

use crate::change::Change;
use crate::cli::{Frontend, default_backend_options};
use crate::error::AssemblyError;
use crate::sema::instruction::{Instruction, MemoryAddress, Opcode};
use crate::sema::reference::{Reference, Resolvable};
use crate::sema::value::{BinaryOperator, Size, SizedAssemblyTimeValue};
use crate::sema::{AddressingMode, AssemblyTimeValue, ProgramElement, Register};
use crate::{AssemblyCode, Segments, pretty_hex};

mod directive;
mod memory;
pub(crate) mod sample_table;
mod table;

pub use table::ASSEMBLY_TABLE;
use table::{EntryOrFirstOperandTable, EntryOrSecondOperandTable, TwoOperandEntry};

use self::memory::{LabeledMemoryValue, MemoryValue};

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
enum ClearLabels {
	#[default]
	Yes,
	No,
}

impl From<bool> for ClearLabels {
	fn from(value: bool) -> Self {
		if value { Self::Yes } else { Self::No }
	}
}

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

/// Runs the assembler, but does not combine data from different segments afterwards.
///
/// Therefore, the assembler effectively only runs inside segments. This function might modify the given segments during
/// optimization, and it returns the assembled segments.
///
/// # Errors
/// Unencodeable instructions will cause errors.
#[allow(clippy::trivially_copy_pass_by_ref)]
pub fn assemble_inside_segments(
	segments: &mut Segments<ProgramElement>,
	source_code: &Arc<AssemblyCode>,
	options: Arc<dyn Frontend>,
) -> Result<(Segments<u8>, EntryPoint), Box<AssemblyError>> {
	let data = assemble_to_data(segments, source_code, options)?;
	Ok((data.resolve_segments()?, data.entry_point))
}

/// Assembles a [`ProgramElement`] inside a loop.
#[macro_export]
macro_rules! assemble_element {
	($data:ident, $program_element:ident, $current_labels:ident) => {{
		let result: Result<(), Box<AssemblyError>> = try {
			match $program_element {
				$crate::sema::ProgramElement::Label(label) => {
					$current_labels.push(label.clone());
					continue;
				},
				$crate::sema::ProgramElement::Instruction(instruction) =>
					$data.assemble_instruction(instruction, &$current_labels)?,
				$crate::sema::ProgramElement::Directive(directive) => {
					let clear_labels = $data.assemble_directive(directive, &$current_labels)?;
					if clear_labels == ClearLabels::No {
						continue;
					}
				},
				$crate::sema::ProgramElement::IncludeSource { .. } =>
					unreachable!("there should not be any remaining unincluded source code at assembly time"),
				$crate::sema::ProgramElement::UserDefinedMacroCall { .. } =>
					unreachable!("there should not be unexpanded user macros at assembly time"),
			}
		};
		result
	}};
}

pub use assemble_element;

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
		data.assemble_all_from_list(segment_content)?;
		if data.should_stop {
			break;
		}
	}

	let mut pass_count = 0;
	while pass_count < maximum_reference_resolution_passes
		&& data.execute_reference_resolution_pass() == Change::Modified
	{
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

/// Entry point specification.
pub type EntryPoint = Option<MemoryAddress>;

/// The assembled data, which consists of multiple sections.
#[derive(Debug)]
pub struct AssembledData {
	/// The segment data.
	pub segments:    Segments<LabeledMemoryValue>,
	/// The source code behind this assembled data
	pub source_code: Arc<AssemblyCode>,
	/// Assembler subroutines use this as a flag to signal an end of assembly as soon as possible.
	should_stop:     bool,
	/// Execution entry point of the code after being loaded.
	pub entry_point: EntryPoint,
	/// Options that command line received; used for determining what to do with warnings.
	options:         Arc<dyn Frontend>,
}

impl AssembledData {
	const DEFAULT_VEC: &'static Vec<Reference> = &Vec::new();

	/// Combine the segments into one binary stream. The result has correct memory addresses, so the first byte is
	/// memory address 0 etc.
	/// # Errors
	/// If the segments contain overlapping data, errors are returned.
	#[allow(clippy::cast_possible_wrap)]
	pub fn combine_segments(&self) -> Result<Vec<u8>, Box<AssemblyError>> {
		let mut all_data = Vec::new();
		let segments = self.resolve_segments()?;

		// The iteration is sorted
		for (starting_address, segment_data) in segments.segments {
			if starting_address < all_data.len() as MemoryAddress {
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
					lmv.try_as_resolved(
						MemoryAddress::try_from(address).unwrap() + start_address,
						&self.source_code,
						&*self.options,
					)
				})
				.try_collect::<Vec<u8>>()
		})
	}

	/// Creates new assembled data
	#[must_use]
	#[inline]
	pub fn new(source_code: Arc<AssemblyCode>) -> Self {
		Self {
			segments: Segments::default(),
			source_code,
			should_stop: false,
			entry_point: None,
			options: default_backend_options(),
		}
	}

	/// Change the error options for assembler warning and error reporting.
	pub fn set_error_options(&mut self, options: Arc<dyn Frontend>) -> &mut Self {
		self.options = options;
		self
	}

	pub(super) fn assemble_all_from_list(&mut self, list: &mut Vec<ProgramElement>) -> Result<(), Box<AssemblyError>> {
		let mut current_labels = Vec::default();
		for program_element in list {
			assemble_element!(self, program_element, current_labels)?;
			if self.should_stop {
				break;
			}
			current_labels.clear();
		}
		Ok(())
	}

	/// Assemble a single instruction. This function uses the codegen table
	/// [`table::ASSEMBLY_TABLE`].
	#[allow(clippy::unnecessary_wraps, clippy::too_many_lines)]
	fn assemble_instruction(
		&mut self,
		instruction: &mut Instruction,
		current_labels: &[Reference],
	) -> Result<(), Box<AssemblyError>> {
		// Because the actions always expect to get a value, we need a fallback dummy value if there is none in the
		// addressing mode. This is fine, since we control the codegen table and we can make sure that we never use a
		// value where there is none in the operand.
		// FIXME: Still not constable.
		let dummy_value: AssemblyTimeValue = AssemblyTimeValue::Literal(0, SourceSpan::new(0usize.into(), 0));

		let Instruction { opcode: Opcode { first_operand, mnemonic, second_operand, .. }, span, .. } = instruction;

		// Retrieve the table entry for the mnemonic.
		let mnemonic_entry = ASSEMBLY_TABLE
			.get(mnemonic)
			.unwrap_or_else(|| panic!("No codegen entries for mnemonic {mnemonic}, this is a bug"));

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
				let get_legal_modes = || {
					let mut legal_modes: Vec<_> = first_operand_table.keys().map(|f| f.to_string().into()).collect();
					legal_modes.sort();
					legal_modes
				};
				let first_operand = first_operand.as_ref().ok_or_else(|| AssemblyError::MissingOperand {
					mnemonic:    *mnemonic,
					legal_modes: get_legal_modes(),
					location:    *span,
					src:         self.source_code.clone(),
				})?;

				// Retrieve the table entry for the first operand.
				let first_operand_entry = first_operand_table.get(&first_operand.into()).ok_or_else(|| {
					AssemblyError::InvalidFirstAddressingMode {
						mode:        first_operand.to_string().into(),
						mnemonic:    *mnemonic,
						legal_modes: get_legal_modes(),
						src:         self.source_code.clone(),
						location:    *span,
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

				let first_bit_index = first_operand.bit_index().or_else(|| mnemonic.bit_index());
				// Don't report an error yet if we have a second entry. The implicit bit index will be caught later in
				// this case.
				if mnemonic.uses_any_bit_index()
					&& !matches!(first_operand_entry, EntryOrSecondOperandTable::Table(..))
					&& first_bit_index.is_none()
				{
					self.options.report_diagnostic(AssemblyError::ImplicitBitIndex {
						mnemonic: *mnemonic,
						location: *span,
						src:      self.source_code.clone(),
					});
				}
				let first_bit_index = first_bit_index.unwrap_or(1);

				match first_operand_entry {
					EntryOrSecondOperandTable::Entry(opcode, action)
					| EntryOrSecondOperandTable::ImplicitAEntry(opcode, action) => {
						self.append_8_bits(MemoryAddress::from(*opcode), current_labels, *span)?;
						action(self, *span, first_operand.number_ref().unwrap_or(&dummy_value), first_bit_index)
					},
					EntryOrSecondOperandTable::BitEntry(opcode, action) => {
						self.append_unresolved_opcode_with_bit_index(
							AssemblyTimeValue::Literal(MemoryAddress::from(*opcode), *span),
							first_bit_index,
							current_labels,
							*span,
						)?;
						action(self, *span, first_operand.number_ref().unwrap_or(&dummy_value), first_bit_index)
					},
					EntryOrSecondOperandTable::TcallEntry(opcode) => self.append_8_bits_unresolved(
						// Synthesize the operation `opcode | ((operand & 0x0F) << 4)` which is exactly how TCALL
						// works.
						AssemblyTimeValue::BinaryOperation {
							lhs:      AssemblyTimeValue::Literal(MemoryAddress::from(*opcode), *span).into(),
							rhs:      AssemblyTimeValue::BinaryOperation {
								lhs:      AssemblyTimeValue::BinaryOperation {
									lhs:      first_operand.number().unwrap_or_else(|| dummy_value.clone()).into(),
									rhs:      AssemblyTimeValue::Literal(0x0F, *span).into(),
									operator: BinaryOperator::And,
									span:     *span,
								}
								.into(),
								rhs:      AssemblyTimeValue::Literal(4, *span).into(),
								operator: BinaryOperator::LeftShift,
								span:     *span,
							}
							.into(),
							operator: BinaryOperator::Or,
							span:     *span,
						},
						0,
						true,
						current_labels,
						*span,
					),
					EntryOrSecondOperandTable::Table(second_operand_table) => {
						let get_legal_modes = || {
							let mut legal_modes: Vec<_> =
								second_operand_table.keys().map(|f| f.to_string().into()).collect();
							legal_modes.sort();
							legal_modes
						};
						let second_operand =
							second_operand.as_ref().ok_or_else(|| AssemblyError::MissingSecondOperand {
								mnemonic:    *mnemonic,
								location:    *span,
								legal_modes: get_legal_modes(),
								src:         self.source_code.clone(),
							})?;

						let second_operand_entry =
							second_operand_table.get(&second_operand.into()).ok_or_else(|| {
								AssemblyError::InvalidSecondAddressingMode {
									mode:        second_operand.to_string().into(),
									mnemonic:    *mnemonic,
									first_mode:  first_operand.to_string().into(),
									legal_modes: get_legal_modes(),
									src:         self.source_code.clone(),
									location:    *span,
								}
							})?;

						let bit_index = first_operand
							.bit_index()
							.or_else(|| second_operand.bit_index())
							.or_else(|| mnemonic.bit_index());
						if mnemonic.uses_any_bit_index() && bit_index.is_none() {
							self.options.report_diagnostic(AssemblyError::ImplicitBitIndex {
								mnemonic: *mnemonic,
								location: *span,
								src:      self.source_code.clone(),
							});
						}
						let bit_index = bit_index.unwrap_or(1);

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
									AssemblyTimeValue::Literal(MemoryAddress::from(*opcode), *span),
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
		if <MemoryAddress as TryInto<u8>>::try_into(value).is_err() {
			self.options.report_diagnostic(AssemblyError::ValueTooLarge {
				value,
				location: span,
				src: self.source_code.clone(),
				size: 8,
			});
		}
		self.append((value & 0xFF) as u8, labels, span)
	}

	/// Appends an 8-bit value to the current segment.
	#[inline]
	fn append(&mut self, value: u8, labels: &[Reference], span: SourceSpan) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.segments.current_segment_mut().map_err(|()| AssemblyError::MissingSegment { location: span, src })?.push(
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
		is_highest_byte: bool,
		labels: &[Reference],
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let src = self.source_code.clone();
		self.segments.current_segment_mut().map_err(|()| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				value:                MemoryValue::Number { value, byte_index: byte, is_highest_byte },
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
		self.append_8_bits_unresolved(value.clone(), 0, false, labels, span)?;
		self.append_8_bits_unresolved(value, 1, true, &Vec::default(), span)
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
			Size::Byte => self.append_8_bits_unresolved(value.value, 0, false, labels, span),
			Size::Word => self.append_16_bits_unresolved(value.value, labels, span),
			Size::Long => {
				self.append_8_bits_unresolved(value.value.clone(), 0, false, labels, span)?;
				self.append_8_bits_unresolved(value.value.clone(), 1, false, &Vec::default(), span)?;
				self.append_8_bits_unresolved(value.value, 2, true, &Vec::default(), span)
			},
			Size::DWord => {
				self.append_8_bits_unresolved(value.value.clone(), 0, false, labels, span)?;
				self.append_8_bits_unresolved(value.value.clone(), 1, false, &Vec::default(), span)?;
				self.append_8_bits_unresolved(value.value.clone(), 2, false, &Vec::default(), span)?;
				self.append_8_bits_unresolved(value.value, 3, true, &Vec::default(), span)
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
		self.segments.current_segment_mut().map_err(|()| AssemblyError::MissingSegment { location: span, src })?.push(
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
		self.segments.current_segment_mut().map_err(|()| AssemblyError::MissingSegment { location: span, src })?.push(
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
		self.segments.current_segment_mut().map_err(|()| AssemblyError::MissingSegment { location: span, src })?.push(
			LabeledMemoryValue {
				// Synthesize the (bit_index << 5) | value which is needed for bit indices in opcodes.
				value:                MemoryValue::Number {
					value:           AssemblyTimeValue::BinaryOperation {
						lhs: value.into(),
						rhs: AssemblyTimeValue::Literal(MemoryAddress::from(bit_index) << 5, span).into(),
						operator: BinaryOperator::Or,
						span,
					},
					byte_index:      0,
					is_highest_byte: true,
				},
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
	///
	/// This means that data which uses references declared later needs one additional resolution pass.
	/// # Returns
	/// Whether any modifications were actually done during the resolution pass.
	/// # Errors
	/// Any warnings and warning-promoted errors from resolution are passed on.
	#[allow(clippy::missing_panics_doc, clippy::cast_possible_wrap)]
	fn execute_reference_resolution_pass(&mut self) -> Change {
		let mut had_modifications = Change::Unmodified;
		for (segment_start, segment_data) in &mut self.segments.segments {
			let mut current_global_label = None;
			for (offset, datum) in segment_data.iter_mut().enumerate() {
				let memory_address = segment_start + offset as MemoryAddress;
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
						had_modifications |= Change::Modified;
						match *resolved_reference {
							Reference::Label(..) | Reference::Relative { .. } => resolved_reference.resolve_to(
								memory_address,
								datum.instruction_location,
								self.source_code.clone(),
							),
							Reference::MacroArgument { value: Some(_), .. } => Ok(()),
							Reference::MacroArgument { value: None, span, .. }
							| Reference::UnresolvedLabel { span, .. } => Err(AssemblyError::UnresolvedReference {
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
							Reference::RepeatCount { span } => Err(AssemblyError::RepeatCountOutsideRepeat {
								usage_location: span,
								src:            self.source_code.clone(),
							}
							.into()),
						}
						.err()
					})
					.for_each(|err| self.options.report_diagnostic(*err));
				// Resolve a reference used as a memory address, e.g. in an instruction operand like a jump target.
				had_modifications |= datum.try_resolve(memory_address, &self.source_code, &*self.options);
			}
		}
		had_modifications
	}
}
