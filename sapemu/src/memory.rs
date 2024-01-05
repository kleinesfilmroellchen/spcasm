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

const BOOT_ROM_START: u16 = 0xFFC0;

const BOOT_ROM: &[u8; MEMORY_SIZE - BOOT_ROM_START as usize] = include_bytes!("../boot.sfc");

impl Default for Memory {
	fn default() -> Self {
		Self::new()
	}
}

impl Memory {
	/// Creates a new memory instance that reflects the hardware reset state.
	#[must_use]
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
	pub fn read(&mut self, address: u16, enable_boot_rom: bool) -> u8 {
		// TODO: Doesn't handle external hardware registers.
		let result = match address {
			BOOT_ROM_START ..= 0xFFFF if enable_boot_rom => BOOT_ROM[(address - BOOT_ROM_START) as usize],
			_ => self.ram[address as usize],
		};
		trace!("read {0:04x} = {1:02x} ({1})", address, result);
		result
	}

	/// Performs a 16-bit little endian read from memory at the given address.
	#[inline]
	pub fn read_word(&mut self, address: u16, enable_boot_rom: bool) -> u16 {
		u16::from(self.read(address, enable_boot_rom)) | (u16::from(self.read(address + 1, enable_boot_rom)) << 8)
	}
}
