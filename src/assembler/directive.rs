//! Directive assembly functions.

use std::fs::File;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};
use miette::SourceSpan;
use num_traits::{FromPrimitive, ToPrimitive};

use super::{resolve_file, AssembledData};
use crate::brr::wav;
use crate::directive::{symbolic_directives, DirectiveValue, FillOperation};
use crate::sema::instruction::MemoryAddress;
use crate::sema::reference::Reference;
use crate::sema::value::{Size, SizedAssemblyTimeValue};
use crate::sema::AssemblyTimeValue;
use crate::{brr, AssemblyError, Directive};

impl AssembledData {
	/// Assemble a single assembler directive into this assembly data.
	///
	/// # Errors
	/// Any error caused by the directive assembly process is returned.
	///
	/// # Panics
	/// All panics are programming bugs.
	#[allow(clippy::unnecessary_wraps)]
	pub fn assemble_directive(
		&mut self,
		directive: &mut Directive,
		current_labels: &Vec<Reference>,
	) -> Result<(), Box<AssemblyError>> {
		match directive.value {
			// Symbolic directives should not be around anymore.
			symbolic_directives!() => unreachable!(),
			DirectiveValue::Table { ref values } =>
				try {
					let mut is_first = true;
					for value in values {
						self.append_sized_unresolved(
							value.clone(),
							if is_first { current_labels } else { Self::DEFAULT_VEC },
							directive.span,
						)?;
						is_first = false;
					}
				},
			DirectiveValue::Brr { ref file, range, auto_trim, .. } =>
				self.assemble_brr(directive, file, range, auto_trim, current_labels),
			DirectiveValue::String { ref text, has_null_terminator } =>
				try {
					self.append_bytes(text.clone(), current_labels, directive.span)?;
					if has_null_terminator {
						self.append(
							0,
							if text.is_empty() { current_labels } else { Self::DEFAULT_VEC },
							directive.span,
						)?;
					}
				},
			DirectiveValue::Include { ref file, range } => {
				let binary_file = resolve_file(&self.source_code, file);
				let mut binary_data = std::fs::read(binary_file).map_err(|os_error| AssemblyError::FileNotFound {
					os_error:  Arc::new(os_error),
					file_name: file.clone(),
					src:       self.source_code.clone(),
					location:  directive.span,
				})?;

				binary_data = self.slice_data_if_necessary(file, directive.span, binary_data, range)?;
				self.append_bytes(binary_data, current_labels, directive.span)
			},
			DirectiveValue::SampleTable { auto_align } => {
				let current_address = self.segments.current_location().unwrap();
				if auto_align {
					if current_address & 0xff != 0 {
						let target_address = (current_address + 0x100) & !0xff;
						let missing_bytes = target_address - current_address;
						let space = std::iter::repeat(0).take(missing_bytes as usize).collect();
						self.append_bytes(space, &Vec::default(), directive.span)?;
					}
				} else if current_address & 0xff != 0 {
					return Err(AssemblyError::UnalignedSampleTable {
						memory_address: current_address,
						location:       directive.span,
						src:            self.source_code.clone(),
					}
					.into());
				}
				self.assemble_sample_table(current_labels, directive.span)
			},
			DirectiveValue::Fill { ref operation, ref parameter, ref value } => {
				let current_address = self.segments.current_location().unwrap();
				self.assemble_fill(operation, parameter, value.clone(), current_address, current_labels, directive.span)
			},
			DirectiveValue::Conditional { ref mut condition, ref mut true_block, ref mut false_block } => {
				let condition = condition.try_value(directive.span, &self.source_code)?;
				if condition == 0 {
					self.assemble_all_from_list(false_block)
				} else {
					self.assemble_all_from_list(true_block)
				}
			},
		}
	}

	pub(super) fn assemble_brr(
		&mut self,
		directive: &Directive,
		file_name: &str,
		range: Option<SourceSpan>,
		auto_trim: bool,
		current_labels: &[Reference],
	) -> Result<(), Box<AssemblyError>> {
		// Resolve the audio file's path relative to the source file.
		let actual_path = resolve_file(&self.source_code, file_name);
		let file = File::open(actual_path).map_err(|os_error| AssemblyError::FileNotFound {
			os_error:  Arc::new(os_error),
			file_name: file_name.to_string().into(),
			src:       self.source_code.clone(),
			location:  directive.span,
		})?;
		let mut sample_data =
			wav::read_wav_for_brr(file).map_err(|error_text| AssemblyError::AudioProcessingError {
				error_text,
				file_name: file_name.to_string().into(),
				src: self.source_code.clone(),
				location: directive.span,
			})?;

		sample_data = self.slice_data_if_necessary(file_name, directive.span, sample_data, range)?;
		#[cfg(debug_assertions)]
		let initial_size = sample_data.len();

		if auto_trim && !sample_data.is_empty() {
			let first_sample = *sample_data.first().unwrap();
			let last_sample = *sample_data.last().unwrap();
			sample_data = sample_data.into_iter().skip_while(|sample| sample == &first_sample).collect();
			sample_data.reverse();
			sample_data = sample_data.into_iter().skip_while(|sample| sample == &last_sample).collect();
			sample_data.push(first_sample);
			sample_data.reverse();
			sample_data.push(last_sample);
			#[cfg(debug_assertions)]
			println!("Auto trim reduced size from {} to {} samples", initial_size, sample_data.len());
		}

		let encoded = brr::encode_to_brr(&mut sample_data, None, brr::CompressionLevel::Max);

		self.append_bytes(encoded, current_labels, directive.span)
	}

	/// Applies the range to the given data if necessary.
	///
	/// # Errors
	/// Range out of bounds errors.
	pub(super) fn slice_data_if_necessary<T>(
		&self,
		file: &str,
		source_span: SourceSpan,
		data: Vec<T>,
		range: Option<SourceSpan>,
	) -> Result<Vec<T>, Box<AssemblyError>>
	where
		T: Clone,
	{
		Ok(if let Some(range) = range {
			let max_number_of_bytes = data.len().saturating_sub(range.offset());
			data.get(range.offset() .. range.offset().saturating_add(range.len()).min(max_number_of_bytes))
				.ok_or(AssemblyError::RangeOutOfBounds {
					start:    range.offset(),
					end:      range.offset().saturating_add(range.len()),
					file:     file.to_string().into(),
					file_len: data.len(),
					src:      self.source_code.clone(),
					location: source_span,
				})?
				.to_vec()
		} else {
			data
		})
	}

	/// Assemble a fill-like directive (fill, fill align, pad).
	pub(super) fn assemble_fill(
		&mut self,
		operation: &FillOperation,
		parameter: &AssemblyTimeValue,
		value: Option<SizedAssemblyTimeValue>,
		current_address: MemoryAddress,
		labels: &[Reference],
		location: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		let value = value.ok_or(AssemblyError::MissingFillParameter {
			operation: operation.to_string().into(),
			is_fill: operation.is_fill(),
			location,
			src: self.source_code.clone(),
		})?;
		let amount_to_fill = operation.amount_to_fill(
			parameter.try_value(location, &self.source_code)?,
			current_address,
			location,
			&self.source_code,
		)?;
		// Mostly an optimization.
		if amount_to_fill == 0 {
			return Ok(());
		}

		let numeric_size = MemoryAddress::from(value.size.to_u8().unwrap());
		// If we don't fill with bytes, there will be truncation at the end. Handle simple full-sized fills first.
		let full_sized_fills = amount_to_fill / numeric_size;
		let mut fill_labels = labels.to_owned();
		for _ in 0 .. full_sized_fills {
			self.append_sized_unresolved(value.clone(), &fill_labels, location)?;
			fill_labels.clear();
		}

		if amount_to_fill == full_sized_fills * numeric_size {
			return Ok(());
		}
		// Since remaining_fill is always smaller than the maximum size, this can not fail.
		let remaining_fill = Size::from_i64(amount_to_fill - full_sized_fills * numeric_size).unwrap();
		self.append_sized_unresolved(
			SizedAssemblyTimeValue { value: value.value, size: remaining_fill },
			&Vec::default(),
			location,
		)
	}
}
