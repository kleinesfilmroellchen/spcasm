//! Symbolic memory values.

use std::sync::Arc;

use miette::SourceSpan;

use crate::change::Change;
use crate::cli::Frontend;
use crate::sema::instruction::MemoryAddress;
use crate::sema::reference::Reference;
use crate::sema::AssemblyTimeValue;
use crate::{AssemblyCode, AssemblyError};

/// Data in memory while we still need to resolve references.
/// This data may have an attached reference.
#[derive(Clone, Debug)]
pub struct LabeledMemoryValue {
	/// The label(s) of this memory value.
	pub labels:               Vec<Reference>,
	/// The actual memory value, which might or might not be resolved.
	pub value:                MemoryValue,
	/// The source span of the instruction or directive that was compiled to this memory value.
	pub instruction_location: SourceSpan,
}

impl LabeledMemoryValue {
	/// Try to resolve this memory value if it has a reference. This always does nothing if the data is already
	/// resolved.
	/// * `own_memory_address`: The actual location in memory that this value is at. Some resolution strategies need
	///   this.
	///
	/// Returns whether the resolution attempt changed anything.
	#[inline]
	#[must_use]
	pub(crate) fn try_resolve(
		&mut self,
		own_memory_address: MemoryAddress,
		src: &Arc<AssemblyCode>,
		frontend: &dyn Frontend,
	) -> Change {
		if let MemoryValue::Resolved(_) = self.value {
			Change::Unmodified
		} else {
			// FIXME: I can't figure out how to do this without copying first.
			let value_copy = self.value.clone();
			self.value = value_copy.try_resolve(own_memory_address, src, frontend);
			Change::Modified
		}
	}

	/// Return the resolved memory value.
	/// # Errors
	/// If the memory value is not resolved, a nice "unresolved reference" error is returned.
	#[inline]
	pub fn try_as_resolved(
		&self,
		own_memory_address: MemoryAddress,
		src: &Arc<AssemblyCode>,
		frontend: &dyn Frontend,
	) -> Result<u8, Box<AssemblyError>> {
		self.value.try_resolved(own_memory_address, src, frontend).map_err(|number| {
			{
				let first_reference = number
					.first_reference()
					.expect("AssemblyTimeValue resolution failure was not caused by reference; this is a bug!");
				AssemblyError::UnresolvedReference {
					reference:          first_reference.to_string().into(),
					reference_location: Some(first_reference.source_span()),
					usage_location:     self.instruction_location,
					src:                src.clone(),
				}
			}
			.into()
		})
	}
}

/// The internal data held in a byte in memory, which may not be resolved.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Debug)]
pub enum MemoryValue {
	/// Resolved data.
	Resolved(u8),
	/// Some byte of an (unresolved) number.
	Number {
		/// The unresolved value.
		value:           AssemblyTimeValue,
		/// The byte that will be used from the value. 0 means the lowest byte, 1 means the
		/// second-lowest byte etc.
		byte_index:      u8,
		/// Whether this number is the highest byte of a sequence of memory values referencing the same data; used for
		/// diagnostics since too large source values should only be reported once by the value with the highest byte.
		is_highest_byte: bool,
	},
	/// An (unresolved) number. The resolved memory value will be the difference between this memory value's location
	/// plus one and the number's location.
	NumberRelative(AssemblyTimeValue),
	/// An (unresolved) number. From the number, the high byte is used, and the higher bits that are used for the bit
	/// index are discarded. The upper three bits are used for the bit index value which can range from 0 to 7. This is
	/// used for most absolute bit addressing modes.
	NumberHighByteWithContainedBitIndex(AssemblyTimeValue, u8),
}

impl MemoryValue {
	#[allow(clippy::match_wildcard_for_single_variants)]
	fn try_resolve(
		self,
		own_memory_address: MemoryAddress,
		source_code: &Arc<AssemblyCode>,
		frontend: &dyn Frontend,
	) -> Self {
		match self {
			Self::Resolved(_) => self,
			Self::Number { value, byte_index, is_highest_byte } => match value.try_resolve() {
				AssemblyTimeValue::Literal(value, value_span) => {
					let unmasked_value = value >> (byte_index * 8);
					unmasked_value.try_into().map_or_else(
						|_| {
							if is_highest_byte {
								frontend.report_diagnostic(AssemblyError::ValueTooLarge {
									value,
									size: (byte_index + 1) * 8,
									location: value_span,
									src: source_code.clone(),
								});
							}
							Self::Resolved((unmasked_value & 0xFF) as u8)
						},
						Self::Resolved,
					)
				},
				resolved => Self::Number { value: resolved, byte_index, is_highest_byte },
			},
			Self::NumberRelative(number) => match number.clone().try_resolve() {
				AssemblyTimeValue::Literal(reference_memory_address, ..) => {
					let relative_offset = reference_memory_address - (own_memory_address + 1);
					<MemoryAddress as TryInto<i8>>::try_into(relative_offset).map_or_else(
						|_| {
							frontend.report_diagnostic(AssemblyError::RelativeOffsetTooLarge {
								location:        number.source_span(),
								src:             source_code.clone(),
								target:          reference_memory_address,
								address:         own_memory_address,
								target_location: number.first_reference().map(|reference| reference.source_span()),
							});
							Self::Resolved((relative_offset & 0xFF) as u8)
						},
						|byte| Self::Resolved(byte as u8),
					)
				},
				resolved => Self::NumberRelative(resolved),
			},
			Self::NumberHighByteWithContainedBitIndex(number, bit_index) => match number.try_resolve() {
				AssemblyTimeValue::Literal(reference_memory_address, ..) => {
					// TODO: perform byte size check
					let resolved_data = ((reference_memory_address & 0x1F00) >> 8) as u8 | (bit_index << 5);
					Self::Resolved(resolved_data)
				},
				resolved => Self::NumberHighByteWithContainedBitIndex(resolved, bit_index),
			},
		}
	}

	#[allow(clippy::missing_const_for_fn)]
	fn try_resolved(
		&self,
		own_memory_address: MemoryAddress,
		source_code: &Arc<AssemblyCode>,
		frontend: &dyn Frontend,
	) -> Result<u8, AssemblyTimeValue> {
		match self.clone().try_resolve(own_memory_address, source_code, frontend) {
			Self::Resolved(value) => Ok(value),
			Self::Number { value: number, .. }
			| Self::NumberHighByteWithContainedBitIndex(number, ..)
			| Self::NumberRelative(number) => Err(number),
		}
	}
}
