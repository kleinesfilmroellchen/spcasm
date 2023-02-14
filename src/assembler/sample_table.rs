//! BRR sample table infrastructure.

use miette::SourceSpan;

use super::AssembledData;
use crate::parser::reference::Reference;
use crate::parser::AssemblyTimeValue;
use crate::AssemblyError;

/// An entire sample table, can have at maximum 256 entries.
#[derive(Clone, Debug, PartialEq)]
pub struct SampleTable {
	/// The entries of the sample table.
	pub entries: Vec<SampleEntry>,
}

/// One entry in the sample table.
#[derive(Clone, Debug, PartialEq)]
pub struct SampleEntry {
	/// Start address of this sample, will occupy bytes 0-1 in the binary.
	pub start_address: AssemblyTimeValue,
}

impl Default for SampleTable {
	/// Create an empty sample table that pre-allocates 256 entries.
	fn default() -> Self {
		Self { entries: Vec::with_capacity(256) }
	}
}

impl SampleTable {
	/// Add a sample to this sample table.
	pub fn add_sample(&mut self, start_address: AssemblyTimeValue) {
		self.entries.push(SampleEntry { start_address });
	}
}

impl AssembledData {
	/// Assemble the sample table into this assembled data.
	///
	/// # Errors
	/// If the sample table is too large or a section is missing, an error is returned.
	#[allow(unused)]
	pub(super) fn assemble_sample_table(
		&mut self,
		reference: &Option<Reference>,
		span: SourceSpan,
	) -> Result<(), Box<AssemblyError>> {
		if self.segments.sample_table.entries.len() > 256 {
			return Err(AssemblyError::SampleTableTooLarge {
				entry_count:        self.segments.sample_table.entries.len(),
				src:                self.source_code.clone(),
				directive_location: span,
			}
			.into());
		}
		// FIXME: Borrow checker doesn't realize that a cloning iterator is not interfering with the later mutable
		// borrow.
		let iter = self.segments.sample_table.entries.clone();
		for entry in iter {
			let start_address = entry.start_address.clone().try_resolve();
			self.append_16_bits_unresolved(start_address.clone(), reference.clone(), span)?;
			// TODO: Loop points aren't user-specifyable yet. For now we use the beginning of the sample.
			self.append_16_bits_unresolved(start_address, reference.clone(), span)?;
		}
		Ok(())
	}
}
