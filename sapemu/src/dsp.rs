//! S-DSP (Synthesizer) emulator.

use registers::DspRegisters;

pub mod registers;
mod tables;

/// State of the S-DSP.
#[derive(Clone, Default)]
pub struct Dsp {
	/// Public DSP registers.
	pub registers: DspRegisters,
}

impl Dsp {
	/// Create a new DSP instance.
	#[must_use]
	pub fn new() -> Self {
		Self { registers: DspRegisters::default() }
	}

	/// Load the register state from a bank of registers (e.g. a memory dump).
	#[allow(clippy::cast_possible_truncation)]
	pub fn load_register_bank(&mut self, register_bank: &[u8; 128]) {
		for (address, value) in register_bank.iter().enumerate() {
			self.registers.write(address as u8, *value);
		}
	}
}
