//! Data upload emulation
//!
//! This package implements the main CPU side of the SPC700 data upload routine as defined in the boot ROM.

use super::CpuIOPorts;

/// A single data block to be uploaded.
pub struct DataBlock {
	/// Start address of the data block.
	pub address: u16,
	/// Data in the block.
	data:        [u8; 256],
	/// Amount of data in the block.
	#[allow(unused)]
	length:      u8,
}

impl DataBlock {
	/// Create a data block from an iterable structure that yields bytes.
	pub fn new(address: u16, iter: impl IntoIterator<Item = u8>) -> Option<Self> {
		let elements: Vec<_> = iter.into_iter().collect();
		if elements.len() > 256 {
			None
		} else {
			let mut data = [0; 256];
			let length = elements.len() as u8;
			for (i, element) in elements.into_iter().enumerate() {
				data[i] = element;
			}
			Some(Self { address, data, length })
		}
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
	WaitingForAcknowledge,
	/// Uploader is done with uploading, all data has been sent.
	Finished,
}

impl Uploader {
	/// Create a new uploader without data.
	pub fn new() -> Self {
		Self {
			current_address:  0,
			remaining_blocks: Vec::new(),
			entry_point:      0,
			state:            UploaderState::default(),
		}
	}

	/// Specify a code entry point the uploader will send after all blocks have been transferred. This API should be
	/// used as soon as possible, in particular before all data blocks are sent.
	pub fn with_entry_point(mut self, entry_point: u16) -> Self {
		self.entry_point = entry_point;
		self
	}

	/// Add another data block to be transferred.
	pub fn with_block(mut self, block: DataBlock) -> Self {
		self.remaining_blocks.push(block);
		self
	}

	/// Return the current byte the uploader needs to send.
	pub fn current_byte(&self) -> Option<u8> {
		self.remaining_blocks
			.first()
			.map(|current_block| current_block.data[(self.current_address - current_block.address) as usize])
	}

	/// Runs the uploader as far as possible.
	pub fn perform_step(&mut self, ports: &mut CpuIOPorts) {
		match self.state {
			UploaderState::WaitingForAA =>
				if ports.read_from_smp::<0>() == 0xAA {
					self.state = UploaderState::WaitingForBB;
				},
			UploaderState::WaitingForBB =>
				if ports.read_from_smp::<1>() == 0xBB {
					ports.write_to_smp::<1>(0x01);
					self.write_address(ports);
					ports.write_to_smp::<0>(0xCC);
					self.state = UploaderState::WaitingForAcknowledge;
				},
			UploaderState::WaitingForAcknowledge => {
				todo!()
			},
			UploaderState::Finished => {},
		}
	}

	/// Write the current address to the
	fn write_address(&self, ports: &mut CpuIOPorts) {
		ports.write_to_smp::<2>((self.current_address & 0xFF) as u8);
		ports.write_to_smp::<3>(((self.current_address >> 8) & 0xFF) as u8);
	}
}
