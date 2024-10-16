//! Audio processing code for the DSP.

use super::{Dsp, VoiceState};
use crate::memory::Memory;

impl Dsp {
	#[allow(clippy::match_same_arms)]
	pub(super) fn run_audio_processing(&mut self) {
		match self.sample_cycle {
			0 => {},
			1 => {},
			2 => self.voices_state[0].run_voice_processing(),
			3 => {},
			4 => {},
			5 => self.voices_state[1].run_voice_processing(),
			6 => {},
			7 => {},
			8 => self.voices_state[2].run_voice_processing(),
			9 => {},
			10 => {},
			11 => self.voices_state[3].run_voice_processing(),
			12 => {},
			13 => {},
			14 => self.voices_state[4].run_voice_processing(),
			15 => {},
			16 => {},
			17 => self.voices_state[5].run_voice_processing(),
			18 => {},
			19 => {},
			20 => self.voices_state[6].run_voice_processing(),
			21 => {},
			22 => {},
			23 => self.voices_state[7].run_voice_processing(),
			24 => {},
			25 => {},
			26 => {},
			27 => {},
			28 => {},
			29 => self.run_main_processing(),
			30 => {},
			31 => {},
			_ => unreachable!(),
		}
	}

	/// Runs the main audio processing for voice mixing and echo processing.
	/// In hardware, this step is distributed between cycles 24 (or 25) and 31 (or 29 for echo).
	/// Since all necessary registers are read in cycle 29 (EDL&ESA take effect one sample later at the earliest!), but
	/// we need to write the echo result starting in cycle 30, this seems to be the perfect time to run the sound
	/// emulation.
	#[allow(clippy::needless_pass_by_ref_mut, clippy::unused_self)]
	fn run_main_processing(&mut self) {}
}

impl VoiceState {
	/// Runs the per-voice audio processing. In hardware, this step is distributed between the first cycle that reads
	/// the voice’s SRCN register (this happens fairly early!) and the last cycle before the voice’s OUTX register is
	/// written. We run all the processing in the cycle before the OUTX write, which doesn’t lose cycle-accuracy.
	#[allow(clippy::needless_pass_by_ref_mut, clippy::unused_self)]
	pub(super) fn run_voice_processing(&mut self) {}
}
