//! Shared memory (ARAM, external hardware registers)

use log::trace;

/// Size of ARAM and memory space.
pub const MEMORY_SIZE: usize = 0x10000;

/// Shared SMP-DSP memory.
pub struct Memory {
	ram: [u8; MEMORY_SIZE],
}

#[allow(unused)]
const DSPADDR: u16 = 0x00F2;
#[allow(unused)]
const DSPDATA: u16 = 0x00F3;

impl Memory {
	/// Creates a new memory instance that reflects the hardware reset state.
	pub fn new() -> Self {
		let mut ram = [0; MEMORY_SIZE];
		ram.chunks_exact_mut(32).enumerate().for_each(|(block, values)| {
			values.fill(if block & 1 == 0 { 0x00 } else { 0xff });
		});
		Self { ram }
	}

	/// Performs a write to memory at the given address.
	#[inline]
	pub fn write(&mut self, address: u16, value: u8) {
		// TODO: Doesn't handle external hardware registers.
		trace!("write {0:04x} = {1:02x} ({1})", address, value);
		self.ram[address as usize] = value;
	}

	/// Performs a read from memory at the given address.
	#[inline]
	pub fn read(&mut self, address: u16) -> u8 {
		// TODO: Doesn't handle external hardware registers.
		trace!("read {0:04x} = {1:02x} ({1})", address, self.ram[address as usize]);
		self.ram[address as usize]
	}

	/// Performs a 16-bit little endian read from memory at the given address.
	#[inline]
	pub fn read_word(&mut self, address: u16) -> u16 {
		self.read(address) as u16 | ((self.read(address + 1) as u16) << 8)
	}
}
