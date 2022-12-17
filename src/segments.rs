//! Segment system.
//!
//! This lives in a separate module because both semantic analysis and assembler need to consider segments.

use std::collections::BTreeMap;

use crate::parser::instruction::MemoryAddress;

/// Handles segments within the final assembled binary. The type of data contained within each segment is the generic
/// parameter; the assembler uses memory values and other analysis passes use other kinds of information per assembled
/// element. Segments provide the datastructure representation of the segment state that the user can manipulate via
/// various segment-related directives.
#[derive(Debug, Clone)]
pub struct Segments<Contained> {
	/// The data segments. These are checked later when being combined into one.
	pub segments:              BTreeMap<MemoryAddress, Vec<Contained>>,
	/// The starting address of the current segment. This is the key to the segments map.
	pub current_segment_start: Option<MemoryAddress>,
	/// The stack of saved segments, manipulated with pushpc/pullpc.
	pub segment_stack:         Vec<MemoryAddress>,
}

impl<Contained> Segments<Contained> {
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
	pub fn current_segment(&self) -> Result<&Vec<Contained>, ()> {
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
	pub fn current_segment_mut(&mut self) -> Result<&mut Vec<Contained>, ()> {
		Ok(self.segments.get_mut(&self.current_segment_start.ok_or(())?).unwrap())
	}

	/// Add an element to the currently active segment.
	/// # Errors
	/// If there is no active segment.
	pub fn add_element(&mut self, element: Contained) -> Result<(), ()> {
		self.current_segment_mut().map(|segment| segment.push(element))
	}
}

impl Segments<MemoryAddress> {
	/// Returns the address we're currently at within the active segment. This assumes that the segment is a list of
	/// sizes for assembled elements, such as instructions.
	pub fn current_address_within_segment(&self) -> Result<MemoryAddress, ()> {
		let assembled_size_within_segment = self.current_segment()?.iter().sum::<MemoryAddress>();
		let start = self.current_segment_start.ok_or(())?;
		Ok(assembled_size_within_segment + start)
	}
}

impl<Contained> Default for Segments<Contained> {
	fn default() -> Self {
		Self {
			segments:              BTreeMap::default(),
			current_segment_start: None,
			segment_stack:         Vec::default(),
		}
	}
}
