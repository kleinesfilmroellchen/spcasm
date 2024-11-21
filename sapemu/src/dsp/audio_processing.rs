//! Audio processing code for the DSP.

use spcasm::brr::{split_bytes_into_nybbles, Block, Header, WarmUpSamples};

use super::{BrrDecoderStep, Dsp, EnvelopeState, VoiceState, BRR_DECODE_BUFFER_SIZE};
use crate::memory::Memory;
use crate::trace;

impl Dsp {
	#[allow(clippy::match_same_arms)]
	pub(super) fn run_audio_processing(&mut self) {
		match self.sample_cycle {
			0 => {},
			1 => {},
			2 => {
				trace!("processing voice 0");
				self.voices_state[0].run_voice_processing();
			},
			3 => {},
			4 => {},
			5 => {
				trace!("processing voice 1");
				self.voices_state[1].run_voice_processing();
			},
			6 => {},
			7 => {},
			8 => {
				trace!("processing voice 2");
				self.voices_state[2].run_voice_processing();
			},
			9 => {},
			10 => {},
			11 => {
				trace!("processing voice 3");
				self.voices_state[3].run_voice_processing();
			},
			12 => {},
			13 => {},
			14 => {
				trace!("processing voice 4");
				self.voices_state[4].run_voice_processing();
			},
			15 => {},
			16 => {},
			17 => {
				trace!("processing voice 5");
				self.voices_state[5].run_voice_processing();
			},
			18 => {},
			19 => {},
			20 => {
				trace!("processing voice 6");
				self.voices_state[6].run_voice_processing();
			},
			21 => {},
			22 => {},
			23 => {
				trace!("processing voice 7");
				self.voices_state[7].run_voice_processing();
			},
			24 => {},
			25 => {},
			26 => {},
			27 => {},
			28 => {},
			29 => {
				trace!("running main processing");
				self.run_main_processing();
			},
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
	pub(super) fn run_voice_processing(&mut self) {
		if self.was_keyed_on {
			self.is_playing = true;
		}
		if self.was_keyed_off {
			self.envelope_volume = 0;
			self.envelope_state = EnvelopeState::Release;
		}
		if self.envelope_volume == 0 && self.envelope_state == EnvelopeState::Release {
			self.is_playing = false;
		}

		if !self.is_playing {
			return;
		}

		self.fill_sample_buffer_if_needed();

		self.was_keyed_off = false;
		self.was_keyed_on = false;
	}

	/// Fills the sample buffer with 4 samples decoded from BRR if necessary.
	fn fill_sample_buffer_if_needed(&mut self) {
		if !self.needs_more_brr_data() {
			// End the state that sets the ENDX flag after one sample.
			if self.brr_decoder_step == BrrDecoderStep::ReadHeader {
				self.brr_decoder_step = BrrDecoderStep::Read4Samples;
			}
			return;
		}

		let mut expanded_samples = [0; 4];
		if self.brr_decoder_step == BrrDecoderStep::Done {
			// Read the next header
			self.brr_header = Header::from(self.brr_read_bytes[0]);
			split_bytes_into_nybbles(&self.brr_read_bytes[1 ..= 2], &mut expanded_samples);
			self.brr_decoder_step = BrrDecoderStep::ReadHeader;
		} else {
			split_bytes_into_nybbles(&self.brr_read_bytes[0 ..= 1], &mut expanded_samples);
			self.brr_decoder_step.advance();
		}

		let block_third = Block::decode_block_third(self.brr_header, expanded_samples, self.warm_up_samples());
		self.decoded_sample_buffer[self.brr_buffer_index as usize .. self.brr_buffer_index as usize + 4]
			.copy_from_slice(&block_third);
		trace!(
			"decoded samples {:?} to {:?} starting at index {}",
			expanded_samples,
			block_third,
			self.brr_buffer_index
		);

		self.brr_buffer_index += 4;
		debug_assert!(
			self.brr_buffer_index <= self.pitch_counter.sample_index(),
			"BRR buffer index should not have overshot pitch counter"
		);
	}

	/// Returns the current warm up samples corresponding to the BRR decoder sample index.
	const fn warm_up_samples(&self) -> WarmUpSamples {
		[
			self.decoded_sample_buffer
				[(self.brr_buffer_index as usize + BRR_DECODE_BUFFER_SIZE - 1) % BRR_DECODE_BUFFER_SIZE],
			self.decoded_sample_buffer
				[(self.brr_buffer_index as usize + BRR_DECODE_BUFFER_SIZE - 2) % BRR_DECODE_BUFFER_SIZE],
		]
	}
}
