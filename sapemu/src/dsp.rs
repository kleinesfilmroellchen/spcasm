//! S-DSP (Synthesizer) emulator.

use registers::DspRegisters;

pub mod registers;
mod tables;

/// State of the S-DSP.
pub struct Dsp {
	/// Public DSP registers.
	pub registers: DspRegisters,
}
