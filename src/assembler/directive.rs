//! Directive assembly functions.

use std::fs::File;

use miette::SourceSpan;

use super::{resolve_file, AssembledData};
use crate::brr::wav;
use crate::{brr, AssemblyError, Directive};

impl AssembledData {
	pub(super) fn assemble_brr(
		&mut self,
		directive: &Directive,
		file_name: &str,
		range: Option<SourceSpan>,
		auto_trim: bool,
		#[allow(unused)] directory: bool,
	) -> Result<(), Box<AssemblyError>> {
		// Resolve the audio file's path relative to the source file.
		let actual_path = resolve_file(&self.source_code, directive.span, file_name)?;
		let file = File::open(actual_path).map_err(|os_error| AssemblyError::FileNotFound {
			os_error,
			file_name: file_name.to_string(),
			src: self.source_code.clone(),
			location: directive.span,
		})?;
		let mut sample_data =
			wav::read_wav_for_brr(file).map_err(|error_text| AssemblyError::AudioProcessingError {
				error_text,
				file_name: file_name.to_string(),
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

		let encoded = brr::encode_to_brr(&mut sample_data, false, brr::CompressionLevel::Max);

		self.append_bytes(encoded, &directive.label, directive.span)
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
					end:      range.offset() + range.len(),
					file:     file.to_string(),
					file_len: data.len(),
					src:      self.source_code.clone(),
					location: source_span,
				})?
				.to_vec()
		} else {
			data
		})
	}
}
