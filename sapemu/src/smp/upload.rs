//! Data upload emulation
//!
//! This package implements the main CPU side of the SPC700 data upload routine as defined in the boot ROM.

use std::error::Error;

use log::{debug, info};
use object::read::elf::ElfFile32;
use object::{Object, ObjectSegment};

use super::CpuIOPorts;

/// A single data block to be uploaded.
pub struct DataBlock {
	/// Start address of the data block.
	pub address: u16,
	/// Data in the block.
	data:        [u8; 256],
	/// Amount of data in the block.
	#[allow(unused)]
	length:      u16,
}

impl DataBlock {
	/// Create a data block from an iterable structure that yields bytes.
	pub fn new(address: u16, iter: impl IntoIterator<Item = u8>) -> Option<Self> {
		let elements: Vec<_> = iter.into_iter().collect();
		if elements.len() > 256 {
			None
		} else {
			let mut data = [0; 256];
			let length = elements.len() as u16;
			for (i, element) in elements.into_iter().enumerate() {
				data[i] = element;
			}
			Some(Self { address, data, length })
		}
	}

	/// Create a full 256-element data block.
	#[must_use]
	pub const fn new_full(address: u16, elements: [u8; 256]) -> Self {
		Self { address, data: elements, length: 256 }
	}
}

/// Uploader responsible for implementing the upload routine.
pub struct Uploader {
	current_address:  u16,
	remaining_blocks: Vec<DataBlock>,
	entry_point:      u16,
	state:            UploaderState,
}

/// Current state of the uploader.
#[derive(Clone, Copy, Debug, Default)]
#[allow(unused)]
enum UploaderState {
	/// Uploader is waiting for signal value 0xAA in port 0.
	#[default]
	WaitingForAA,
	/// Uploader is waiting for signal value 0xBB in port 1.
	WaitingForBB,
	/// Uploader is waiting for acknowledge of 0xCC in port 0.
	WaitingForCC,
	/// Uploader is waiting for a data byte to be acknowledged.
	WaitingForByteAck,
	// WaitingFor
	/// Uploader is done with uploading, all data has been sent.
	Finished,
}

impl Default for Uploader {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum NextByteStatus {
	Normal,
	NewBlock,
	NoMoreBlocks,
}

impl Uploader {
	/// Create a new uploader without data.
	#[must_use]
	pub fn new() -> Self {
		Self {
			current_address:  0,
			remaining_blocks: Vec::new(),
			entry_point:      0,
			state:            UploaderState::default(),
		}
	}

	/// Load uploader data from an ELF file.
	///
	/// # Errors
	/// Any ELF loading errors are passed on.
	///
	/// # Panics
	/// All panics are programming bugs.
	pub fn from_elf(file: &ElfFile32) -> Result<Self, Box<dyn Error>> {
		let entry_point = file.raw_header().e_entry.get(file.endian()) as u16;
		let mut this = Self::new().with_entry_point(entry_point);

		for segment in file.segments() {
			let start_address = segment.address() as u16;
			let data = segment.data()?;
			let (blocks, last_block) = data.as_chunks::<256>();
			this = blocks.iter().enumerate().fold(this, |this, (i, block)| {
				this.with_block(DataBlock::new_full(start_address + (i * 256) as u16, *block))
			});
			if !last_block.is_empty() {
				this = this.with_block(
					DataBlock::new(start_address + (blocks.len() * 256) as u16, last_block.iter().copied()).unwrap(),
				);
			}
		}

		info!("Loaded {} blocks from ELF file.", this.remaining_blocks.len());

		Ok(this)
	}

	/// Specify a code entry point the uploader will send after all blocks have been transferred. This API should be
	/// used as soon as possible, in particular before all data blocks are sent.
	#[must_use]
	pub const fn with_entry_point(mut self, entry_point: u16) -> Self {
		self.entry_point = entry_point;
		self
	}

	/// Add another data block to be transferred.
	#[must_use]
	pub fn with_block(mut self, block: DataBlock) -> Self {
		self.remaining_blocks.push(block);
		self
	}

	/// Return the current byte the uploader needs to send.
	#[must_use]
	pub fn current_byte(&self) -> Option<u8> {
		self.remaining_blocks
			.first()
			.map(|current_block| current_block.data[(self.current_address - current_block.address) as usize])
	}

	fn current_index(&self) -> Option<u16> {
		self.remaining_blocks.first().map(|current_block| self.current_address - current_block.address)
	}

	fn next_byte(&mut self) -> NextByteStatus {
		if self.remaining_blocks.is_empty() {
			return NextByteStatus::NoMoreBlocks;
		}

		self.current_address = self.current_address.wrapping_add(1);
		// If address is past current block length, remove this block and move on to the next
		if self.current_address >= self.remaining_blocks.first().map(|b| b.address + b.length).unwrap_or_default() {
			self.remaining_blocks.remove(0);
			self.current_address = self.remaining_blocks.first().map(|b| b.address).unwrap_or_default();

			if self.remaining_blocks.is_empty() {
				NextByteStatus::NoMoreBlocks
			} else {
				NextByteStatus::NewBlock
			}
		} else {
			NextByteStatus::Normal
		}
	}

	/// Runs the uploader as far as possible.
	///
	/// # Panics
	/// All panics are programming bugs.
	pub fn perform_step(&mut self, ports: &mut CpuIOPorts) {
		match self.state {
			UploaderState::WaitingForAA =>
				if ports.read_from_smp::<0>() == 0xAA {
					debug!("Read AA, waiting for BB...");
					self.state = UploaderState::WaitingForBB;
				},
			UploaderState::WaitingForBB =>
				if ports.read_from_smp::<1>() == 0xBB {
					debug!("Read BB, sending CC and start address");
					ports.write_to_smp::<1>(0x01);
					self.write_address(ports);
					ports.write_to_smp::<0>(0xCC);
					self.state = UploaderState::WaitingForCC;
				},
			UploaderState::WaitingForCC =>
				if ports.read_from_smp::<0>() == 0xCC {
					debug!("Read CC, starting first block transfer");
					if let Some(first_byte) = self.current_byte() {
						ports.write_to_smp::<0>(0x00);
						ports.write_to_smp::<1>(first_byte);
					}
					self.state = UploaderState::WaitingForByteAck;
				},
			UploaderState::WaitingForByteAck => {
				if self.current_index().is_some_and(|i| i == u16::from(ports.read_from_smp::<0>())) {
					debug!("Got ACKed index {}, sending next byte", self.current_index().unwrap());
					let status = self.next_byte();
					match status {
						NextByteStatus::Normal => {
							ports.write_to_smp::<0>(self.current_index().unwrap() as u8);
							ports.write_to_smp::<1>(self.current_byte().unwrap());
						},
						NextByteStatus::NewBlock => {
							self.write_address(ports);
							ports.write_to_smp::<0>(self.current_index().unwrap() as u8);
							ports.write_to_smp::<1>(self.current_byte().unwrap());
						},
						NextByteStatus::NoMoreBlocks => {
							todo!();
						},
					}
				}
			},
			UploaderState::Finished => {},
		}
	}

	/// Write the current address to the corresponding IO ports (2 and 3)
	fn write_address(&self, ports: &mut CpuIOPorts) {
		ports.write_to_smp::<2>((self.current_address & 0xFF) as u8);
		ports.write_to_smp::<3>(((self.current_address >> 8) & 0xFF) as u8);
	}
}
