//! Segment system.
//!
//! This lives in a separate module because both semantic analysis and assembler need to consider segments.

use std::collections::BTreeMap;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};

use crate::assembler::sample_table::SampleTable;
use crate::directive::DirectiveParameterTable;
use crate::sema::instruction::MemoryAddress;
use crate::{AssemblyCode, AssemblyError, pretty_hex};

/// Handles binary segments and assembler state.
///
/// The type of data contained within each segment is the generic parameter; the assembler uses memory values and other
/// analysis passes use other kinds of information per assembled element. Segments provide the datastructure
/// representation of the segment state that the user can manipulate via various segment-related directives. Segments
/// also contain other global state which is modified via directives.
#[derive(Debug, Clone)]
pub struct Segments<Contained> {
	/// The data segments. These are checked later when being combined into one.
	pub segments:              BTreeMap<MemoryAddress, Vec<Contained>>,
	/// The starting address of the current segment. This is the key to the segments map.
	pub current_segment_start: Option<MemoryAddress>,
	/// The stack of saved segments, manipulated with pushpc/pullpc.
	pub segment_stack:         Vec<MemoryAddress>,
	/// Current contents of the BRR sample table.
	pub sample_table:          SampleTable,
	/// Current state of the directive parameters.
	pub directive_parameters:  Box<DirectiveParameterTable>,
}

#[allow(clippy::result_unit_err)]
impl<Contained> Segments<Contained> {
	/// Push the current segment onto the segment stack, leaving the current segment vacant.
	///
	/// # Errors
	/// If there is no current segment.
	pub fn push_segment(&mut self) -> Result<(), ()> {
		self.segment_stack.push(self.current_segment_start.ok_or(())?);
		self.current_segment_start = None;
		Ok(())
	}

	/// Pop the current segment off the stack, re-enabling it.
	///
	/// # Errors
	/// If there is no segment on the stack.
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

	/// Returns the current memory location where data is written to.
	/// # Errors
	/// If this assembly data doesn't have a started segment yet, or the start address overflowed (unlikely).
	#[inline]
	#[allow(clippy::missing_panics_doc)]
	pub fn current_location(&self) -> Result<MemoryAddress, ()> {
		Ok(MemoryAddress::try_from(self.segments[&self.current_segment_start.ok_or(())?].len()).map_err(|_| ())?
			+ self.current_segment_start.unwrap())
	}

	/// Returns the current offset from the start of the current segment.
	/// # Errors
	/// If this assembly data doesn't have a started segment yet, or the start address overflowed (unlikely).
	#[inline]
	#[allow(clippy::missing_panics_doc)]
	pub fn current_offset(&self) -> Result<MemoryAddress, ()> {
		MemoryAddress::try_from(self.segments[&self.current_segment_start.ok_or(())?].len()).map_err(|_| ())
	}

	/// Returns a mutable reference to the data of the current segment.
	/// # Errors
	/// If this assembly data doesn't have a started segment yet.
	#[inline]
	#[allow(clippy::missing_panics_doc)]
	pub fn current_segment_mut(&mut self) -> Result<&mut Vec<Contained>, ()> {
		Ok(self.segments.get_mut(&self.current_segment_start.ok_or(())?).unwrap())
	}

	/// Add an element to the currently active segment.
	/// # Errors
	/// If there is no active segment.
	pub fn add_element(&mut self, element: Contained) -> Result<(), ()> {
		self.current_segment_mut().map(|segment| segment.push(element))
	}

	/// Creates new segments by mapping all of the segments over to a new container type.
	///
	/// # Errors
	/// May fail if the segment mapping fails.
	pub fn try_map_segments<Output, Error>(
		self,
		map: impl Fn(MemoryAddress, Vec<Contained>) -> Result<Vec<Output>, Error>,
	) -> Result<Segments<Output>, Error> {
		Ok(Segments::<Output> {
			current_segment_start: self.current_segment_start,
			segment_stack:         self.segment_stack,
			sample_table:          self.sample_table,
			directive_parameters:  self.directive_parameters,
			segments:              self
				.segments
				.into_iter()
				.map(|(address, contents)| -> Result<(MemoryAddress, Vec<Output>), Error> {
					Ok((address, map(address, contents)?))
				})
				.try_collect()?,
		})
	}
}

impl<Contained> Default for Segments<Contained> {
	fn default() -> Self {
		Self {
			segments:              BTreeMap::default(),
			current_segment_start: None,
			segment_stack:         Vec::default(),
			sample_table:          SampleTable::default(),
			directive_parameters:  DirectiveParameterTable::default().into(),
		}
	}
}

impl Segments<u8> {
	/// Flatten the binary segments into a single flat memory space.
	///
	/// # Errors
	/// If segments overlap, an error is produced.
	#[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
	pub fn flatten(&self, source_code: &Arc<AssemblyCode>) -> Result<Vec<u8>, Box<AssemblyError>> {
		let mut all_data = Vec::new();
		// The iteration is sorted
		for (starting_address, segment_data) in &self.segments {
			if *starting_address < all_data.len() as MemoryAddress {
				return Err(AssemblyError::SegmentMismatch {
					src:           Arc::new(AssemblyCode {
						text:         pretty_hex(&all_data, Some(*starting_address as usize)),
						name:         source_code.name.clone(),
						include_path: Vec::new(),
					}),
					// TODO: This location is wrong, it ignores newlines.
					location:      (*starting_address as usize * 3 + 1, 2).into(),
					segment_start: *starting_address,
					segment_end:   all_data.len() as MemoryAddress,
				}
				.into());
			}
			all_data.resize(*starting_address as usize, 0);
			all_data.extend_from_slice(segment_data);
		}

		Ok(all_data)
	}
}
