//! Audio processing code for the DSP.

use super::Dsp;
use crate::memory::Memory;

impl Dsp {
	#[allow(clippy::needless_pass_by_ref_mut, clippy::unused_self)]
	pub(super) fn run_audio_processing(&mut self, memory: &mut Memory) {}
}
