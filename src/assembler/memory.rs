//! Symbolic memory values.

use std::sync::Arc;

use miette::SourceSpan;

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
	/// If the memory value is not resolved, a nice "unresolved reference" error is returned.
	#[inline]
	pub fn try_as_resolved(
		&self,
		own_memory_address: MemoryAddress,
		src: &Arc<AssemblyCode>,
	) -> Result<u8, Box<AssemblyError>> {
		self.value.try_resolved(own_memory_address).map_err(|number| {
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
	/// Some byte of an (unresolved) number. The u8 is the byte index, where 0 means the lowest byte, 1 means the
	/// second-lowest byte etc.
	Number(AssemblyTimeValue, u8),
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
	fn try_resolve(self, own_memory_address: MemoryAddress) -> Self {
		match self {
			Self::Resolved(_) => self,
			Self::Number(number, byte) => match number.try_resolve() {
				AssemblyTimeValue::Literal(memory_location) =>
					Self::Resolved(((memory_location & (0xFF << (byte * 8))) >> (byte * 8)) as u8),
				resolved => Self::Number(resolved, byte),
			},
			Self::NumberRelative(number) => match number.try_resolve() {
				AssemblyTimeValue::Literal(reference_memory_address) => {
					let resolved_data = (reference_memory_address - (own_memory_address + 1)) as u8;
					Self::Resolved(resolved_data)
				},
				resolved => Self::NumberRelative(resolved),
			},
			Self::NumberHighByteWithContainedBitIndex(number, bit_index) => match number.try_resolve() {
				AssemblyTimeValue::Literal(reference_memory_address) => {
					let resolved_data = ((reference_memory_address & 0x1F00) >> 8) as u8 | (bit_index << 5);
					Self::Resolved(resolved_data)
				},
				resolved => Self::NumberHighByteWithContainedBitIndex(resolved, bit_index),
			},
		}
	}

	#[allow(clippy::missing_const_for_fn)]
	fn try_resolved(&self, own_memory_address: MemoryAddress) -> Result<u8, AssemblyTimeValue> {
		match self.clone().try_resolve(own_memory_address) {
			Self::Resolved(value) => Ok(value),
			Self::Number(number, ..)
			| Self::NumberHighByteWithContainedBitIndex(number, ..)
			| Self::NumberRelative(number) => Err(number),
		}
	}
}
