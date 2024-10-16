//! S-DSP (Synthesizer) emulator.

#![allow(unused)]

use registers::{DspFlags, DspRegisters, PerVoiceFlag};
use spcasm::brr;
use tables::RATE_TABLE;

use crate::memory::Memory;
use crate::trace;

mod audio_processing;
pub mod registers;
mod tables;

/// State of the S-DSP.
#[derive(Clone, Debug, Default)]
pub struct Dsp {
	/// Public DSP registers.
	pub registers: DspRegisters,

	// Main state
	/// Main volume for left channel, copied from DSP registers.
	main_volume_left:  i8,
	/// Main volume for right channel, copied from DSP registers.
	main_volume_right: i8,
	/// Internal voice state, data copied from DSP registers into here as in hardware timings.
	voices_state:      [VoiceState; 8],
	/// Sample directory address.
	sample_directory:  u16,
	/// Noise frequency divider.
	noise_frequency:   u16,
	/// Last stereo sample (left, right) produced by the DSP, for sending to other audio systems.
	pub last_sample:   (i16, i16),

	// Echo
	/// Echo volume for left channel, copied from DSP registers.
	echo_volume_left:   i8,
	/// Echo volume for right channel, copied from DSP registers.
	echo_volume_right:  i8,
	/// Feedback from echo output back to input, copied from DSP registers.
	echo_feedback:      i8,
	/// Amount of samples that the echo buffer delays by.
	echo_delay:         u16,
	/// Base address of in-memory echo ring buffer.
	echo_base_address:  u16,
	/// Disable all writes to the echo buffer (true), copied from DSP registers.
	echo_write_disable: bool,
	/// Current memory address where the echo buffer data is written or read.
	echo_buffer_head:   u16,
	/// FIR filter coefficients, data copied from DSP registers.
	fir_coefficients:   [u8; 8],
	/// Last few samples that passed through the FIR filter, for the previous samples in the FIR calculation.
	fir_buffer:         [u16; 8],
	/// Echo output or input values, which are first read from memory, then written (with the new output) into memory
	/// at the appropriate cycles.
	echo_data:          (u16, u16),

	// Cycle-accurate emulation
	/// Internal sample cycle, used to perform memory accesses at the correct time.
	sample_cycle:                 u8,
	/// Whether the next cycles 30 and 31 read the KON and KOFF flags (which are only read every 2 samples).
	current_sample_read_kon_koff: bool,
}

impl Dsp {
	/// Create a new DSP instance.
	#[must_use]
	pub fn new() -> Self {
		Self::default()
	}

	/// Load the register state from a bank of registers (e.g. a memory dump).
	#[allow(clippy::cast_possible_truncation)]
	pub fn load_register_bank(&mut self, register_bank: &[u8; 128]) {
		for (address, value) in register_bank.iter().enumerate() {
			self.registers.write(address as u8, *value);
		}
	}

	/// Execute one DSP tick. This may execute up to two memory accesses.
	pub fn tick(&mut self, memory: &mut Memory) {
		self.sample_cycle = (self.sample_cycle + 1) % 32;
		if self.sample_cycle == 0 {
			self.current_sample_read_kon_koff = !self.current_sample_read_kon_koff;
		}

		self.read_memory(memory);
		self.read_write_registers();

		// Note that we can't run the processing just once per sample cycle due to when hardware registers are written
		// back.
		self.run_audio_processing();
	}

	/// Execute memory reads for this sample cycle.
	#[allow(clippy::match_same_arms)]
	fn read_memory(&mut self, memory: &mut Memory) {
		match self.sample_cycle {
			0 => self.voices_state[0].read_brr_2(memory),
			1 => self.voices_state[1].read_sample_directory(memory, self.sample_directory),
			2 => self.voices_state[1].read_brr_1(memory),
			3 => self.voices_state[1].read_brr_2(memory),
			4 => self.voices_state[2].read_sample_directory(memory, self.sample_directory),
			5 => self.voices_state[2].read_brr_1(memory),
			6 => self.voices_state[2].read_brr_2(memory),
			7 => self.voices_state[3].read_sample_directory(memory, self.sample_directory),
			8 => self.voices_state[3].read_brr_1(memory),
			9 => self.voices_state[3].read_brr_2(memory),
			10 => self.voices_state[4].read_sample_directory(memory, self.sample_directory),
			11 => self.voices_state[4].read_brr_1(memory),
			12 => self.voices_state[4].read_brr_2(memory),
			13 => self.voices_state[5].read_sample_directory(memory, self.sample_directory),
			14 => self.voices_state[5].read_brr_1(memory),
			15 => self.voices_state[5].read_brr_2(memory),
			16 => self.voices_state[6].read_sample_directory(memory, self.sample_directory),
			17 => self.voices_state[6].read_brr_1(memory),
			18 => self.voices_state[6].read_brr_2(memory),
			19 => self.voices_state[7].read_sample_directory(memory, self.sample_directory),
			20 => self.voices_state[7].read_brr_1(memory),
			21 => self.voices_state[7].read_brr_2(memory),
			22 => self.voices_state[0].read_sample_directory(memory, self.sample_directory),
			23 => self.read_echo(memory, true),
			24 => self.read_echo(memory, false),
			25 => {},
			26 => self.voices_state[0].read_brr_1(memory),
			27 => {},
			28 => {},
			29 => {},
			30 => self.write_echo(memory, true),
			31 => self.write_echo(memory, false),
			_ => unreachable!(),
		}
	}

	/// Execute BRR register reads and writes for this sample cycle.
	#[allow(clippy::cast_possible_wrap, clippy::too_many_lines)]
	fn read_write_registers(&mut self) {
		match self.sample_cycle {
			0 => {
				// V0VOLL
				self.voices_state[0].volume_left = self.registers.voices[0].volume_left as i8;
				// V2SRCN
				self.voices_state[2].sample_index = self.registers.voices[2].sample_number;
			},
			1 => {
				// V0VOLR
				self.voices_state[0].volume_right = self.registers.voices[0].volume_right as i8;
				// V1PITCHL
				self.voices_state[1].pitch_counter.set_lsb(self.registers.voices[1].pitch_low);
				// V1ADSR1
				self.voices_state[1].envelope_settings.set_adsr1(self.registers.voices[1].adsr_low);
				// ENDX.0
				self.registers
					.voice_end
					.set(PerVoiceFlag::ZERO, self.voices_state[0].brr_decoder_step.just_read_end_block());
			},
			2 => {
				// V0ENVX
				self.registers.voices[0].envelope_value = self.voices_state[0].envelope_output();
				// V1PITCHH
				self.voices_state[1].pitch_counter.set_msb(self.registers.voices[1].pitch_high);
				// V1ADSR2 / V1GAIN
				self.voices_state[1].envelope_settings.set_adsr2(self.registers.voices[1].adsr_high);
				self.voices_state[1].envelope_settings.set_gain(self.registers.voices[1].gain_settings);
				// FLG.7 for V1
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[1].soft_reset();
				}
			},
			3 => {
				// V0OUTX
				self.registers.voices[0].output_value = self.voices_state[0].output();
				// V1VOLL
				self.voices_state[1].volume_left = self.registers.voices[1].volume_left as i8;
				// V3SRCN
				self.voices_state[3].sample_index = self.registers.voices[3].sample_number;
			},
			4 => {
				// V1VOLR
				self.voices_state[1].volume_right = self.registers.voices[1].volume_right as i8;
				// V2PITCHL
				self.voices_state[2].pitch_counter.set_lsb(self.registers.voices[2].pitch_low);
				// V2ADSR1
				self.voices_state[2].envelope_settings.set_adsr1(self.registers.voices[2].adsr_low);
				// ENDX.1
				self.registers
					.voice_end
					.set(PerVoiceFlag::ONE, self.voices_state[1].brr_decoder_step.just_read_end_block());
			},
			5 => {
				// V1ENVX
				self.registers.voices[1].envelope_value = self.voices_state[1].envelope_output();
				// V2PITCHH
				self.voices_state[2].pitch_counter.set_msb(self.registers.voices[2].pitch_high);
				// V2ADSR2 / V2GAIN
				self.voices_state[2].envelope_settings.set_adsr2(self.registers.voices[2].adsr_high);
				self.voices_state[2].envelope_settings.set_gain(self.registers.voices[2].gain_settings);
				// FLG.7 for V2
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[2].soft_reset();
				}
			},
			6 => {
				// V1OUTX
				self.registers.voices[1].output_value = self.voices_state[1].output();
				// V2VOLL
				self.voices_state[2].volume_left = self.registers.voices[2].volume_left as i8;
				// V4SRCN
				self.voices_state[4].sample_index = self.registers.voices[4].sample_number;
			},
			7 => {
				// V2VOLR
				self.voices_state[2].volume_right = self.registers.voices[2].volume_right as i8;
				// V3PITCHL
				self.voices_state[3].pitch_counter.set_lsb(self.registers.voices[3].pitch_low);
				// V3ADSR1
				self.voices_state[3].envelope_settings.set_adsr1(self.registers.voices[3].adsr_low);
				// ENDX.2
				self.registers
					.voice_end
					.set(PerVoiceFlag::TWO, self.voices_state[2].brr_decoder_step.just_read_end_block());
			},
			8 => {
				// V2ENVX
				self.registers.voices[2].envelope_value = self.voices_state[2].envelope_output();
				// V3PITCHH
				self.voices_state[3].pitch_counter.set_msb(self.registers.voices[3].pitch_high);
				// V3ADSR2 / V3GAIN
				self.voices_state[3].envelope_settings.set_adsr2(self.registers.voices[3].adsr_high);
				self.voices_state[3].envelope_settings.set_gain(self.registers.voices[3].gain_settings);
				// FLG.7 for V3
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[3].soft_reset();
				}
			},
			9 => {
				// V2OUTX
				self.registers.voices[2].output_value = self.voices_state[2].output();
				// V3VOLL
				self.voices_state[3].volume_left = self.registers.voices[3].volume_left as i8;
				// V5SRCN
				self.voices_state[5].sample_index = self.registers.voices[5].sample_number;
			},
			10 => {
				// V3VOLR
				self.voices_state[3].volume_right = self.registers.voices[3].volume_right as i8;
				// V4PITCHL
				self.voices_state[4].pitch_counter.set_lsb(self.registers.voices[4].pitch_low);
				// V4ADSR1
				self.voices_state[4].envelope_settings.set_adsr1(self.registers.voices[4].adsr_low);
				// ENDX.3
				self.registers
					.voice_end
					.set(PerVoiceFlag::THREE, self.voices_state[3].brr_decoder_step.just_read_end_block());
			},
			11 => {
				// V3ENVX
				self.registers.voices[3].envelope_value = self.voices_state[3].envelope_output();
				// V4PITCHH
				self.voices_state[4].pitch_counter.set_msb(self.registers.voices[4].pitch_high);
				// V4ADSR2 / V4GAIN
				self.voices_state[4].envelope_settings.set_adsr2(self.registers.voices[4].adsr_high);
				self.voices_state[4].envelope_settings.set_gain(self.registers.voices[4].gain_settings);
				// FLG.7 for V4
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[4].soft_reset();
				}
			},
			12 => {
				// V3OUTX
				self.registers.voices[3].output_value = self.voices_state[3].output();
				// V4VOLL
				self.voices_state[4].volume_left = self.registers.voices[4].volume_left as i8;
				// V6SRCN
				self.voices_state[6].sample_index = self.registers.voices[6].sample_number;
			},
			13 => {
				// V4VOLR
				self.voices_state[4].volume_right = self.registers.voices[4].volume_right as i8;
				// V5PITCHL
				self.voices_state[5].pitch_counter.set_lsb(self.registers.voices[5].pitch_low);
				// V5ADSR1
				self.voices_state[5].envelope_settings.set_adsr1(self.registers.voices[5].adsr_low);
				// ENDX.4
				self.registers
					.voice_end
					.set(PerVoiceFlag::FOUR, self.voices_state[4].brr_decoder_step.just_read_end_block());
			},
			14 => {
				// V4ENVX
				self.registers.voices[4].envelope_value = self.voices_state[4].envelope_output();
				// V5PITCHH
				self.voices_state[5].pitch_counter.set_msb(self.registers.voices[5].pitch_high);
				// V5ADSR2 / V5GAIN
				self.voices_state[5].envelope_settings.set_adsr2(self.registers.voices[5].adsr_high);
				self.voices_state[5].envelope_settings.set_gain(self.registers.voices[5].gain_settings);
				// FLG.7 for V5
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[5].soft_reset();
				}
			},
			15 => {
				// V4OUTX
				self.registers.voices[4].output_value = self.voices_state[4].output();
				// V5VOLL
				self.voices_state[5].volume_left = self.registers.voices[5].volume_left as i8;
				// V7SRCN
				self.voices_state[7].sample_index = self.registers.voices[7].sample_number;
			},
			16 => {
				// V5VOLR
				self.voices_state[5].volume_right = self.registers.voices[5].volume_right as i8;
				// V6PITCHL
				self.voices_state[6].pitch_counter.set_lsb(self.registers.voices[6].pitch_low);
				// V6ADSR1
				self.voices_state[6].envelope_settings.set_adsr1(self.registers.voices[6].adsr_low);
				// ENDX.5
				self.registers
					.voice_end
					.set(PerVoiceFlag::FIVE, self.voices_state[5].brr_decoder_step.just_read_end_block());
			},
			17 => {
				// V5ENVX
				self.registers.voices[5].envelope_value = self.voices_state[5].envelope_output();
				// V6PITCHH
				self.voices_state[6].pitch_counter.set_msb(self.registers.voices[6].pitch_high);
				// V6ADSR2 / V6GAIN
				self.voices_state[6].envelope_settings.set_adsr2(self.registers.voices[6].adsr_high);
				self.voices_state[6].envelope_settings.set_gain(self.registers.voices[6].gain_settings);
				// FLG.7 for V6
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[6].soft_reset();
				}
			},
			18 => {
				// V5OUTX
				self.registers.voices[5].output_value = self.voices_state[5].output();
				// V6VOLL
				self.voices_state[6].volume_left = self.registers.voices[6].volume_left as i8;
				// V0SRCN
				self.voices_state[0].sample_index = self.registers.voices[0].sample_number;
			},
			19 => {
				// V6VOLR
				self.voices_state[6].volume_right = self.registers.voices[6].volume_right as i8;
				// V7PITCHL
				self.voices_state[7].pitch_counter.set_lsb(self.registers.voices[7].pitch_low);
				// V7ADSR1
				self.voices_state[7].envelope_settings.set_adsr1(self.registers.voices[7].adsr_low);
				// ENDX.6
				self.registers
					.voice_end
					.set(PerVoiceFlag::SIX, self.voices_state[6].brr_decoder_step.just_read_end_block());
			},
			20 => {
				// V6ENVX
				self.registers.voices[6].envelope_value = self.voices_state[6].envelope_output();
				// V7PITCHH
				self.voices_state[7].pitch_counter.set_msb(self.registers.voices[7].pitch_high);
				// V7ADSR2 / V7GAIN
				self.voices_state[7].envelope_settings.set_adsr2(self.registers.voices[7].adsr_high);
				self.voices_state[7].envelope_settings.set_gain(self.registers.voices[7].gain_settings);
				// FLG.7 for V7
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[7].soft_reset();
				}
			},
			21 => {
				// V6OUTX
				self.registers.voices[6].output_value = self.voices_state[6].output();
				// V7VOLL
				self.voices_state[7].volume_left = self.registers.voices[7].volume_left as i8;
				// V1SRCN
				self.voices_state[1].sample_index = self.registers.voices[1].sample_number;
			},
			22 => {
				// V7VOLR
				self.voices_state[7].volume_right = self.registers.voices[7].volume_right as i8;
				// V0PITCHL
				self.voices_state[0].pitch_counter.set_lsb(self.registers.voices[0].pitch_low);
				// V0ADSR1
				self.voices_state[0].envelope_settings.set_adsr1(self.registers.voices[0].adsr_low);
				// ENDX.7
				self.registers
					.voice_end
					.set(PerVoiceFlag::SEVEN, self.voices_state[7].brr_decoder_step.just_read_end_block());
			},
			23 => {
				// V7ENVX
				self.registers.voices[7].envelope_value = self.voices_state[7].envelope_output();
				// V0PITCHH
				self.voices_state[0].pitch_counter.set_msb(self.registers.voices[0].pitch_high);
				// FIR0
				self.fir_coefficients[0] = self.registers.fir_coefficients[0];
			},
			24 => {
				// V7OUTX
				self.registers.voices[7].output_value = self.voices_state[7].output();
				// FIR1
				self.fir_coefficients[1] = self.registers.fir_coefficients[1];
				// FIR2
				self.fir_coefficients[2] = self.registers.fir_coefficients[2];
			},
			25 => {
				// FIR3
				self.fir_coefficients[3] = self.registers.fir_coefficients[3];
				// FIR4
				self.fir_coefficients[4] = self.registers.fir_coefficients[4];
				// FIR5
				self.fir_coefficients[5] = self.registers.fir_coefficients[5];
			},
			26 => {
				// FIR6
				self.fir_coefficients[6] = self.registers.fir_coefficients[6];
				// FIR7
				self.fir_coefficients[7] = self.registers.fir_coefficients[7];
			},
			27 => {
				// MVOLL
				self.main_volume_left = self.registers.main_volume_left as i8;
				// EVOLL
				self.echo_volume_left = self.registers.echo_volume_left as i8;
				// EFB
				self.echo_feedback = self.registers.echo_feedback_volume as i8;
			},
			28 => {
				// MVOLR
				self.main_volume_left = self.registers.main_volume_left as i8;
				// EVOLR
				self.echo_volume_left = self.registers.echo_volume_left as i8;
				// PMON
				self.voices_state[1].pitch_mod_enable = self.registers.pitch_mod_enable.contains(PerVoiceFlag::ONE);
				self.voices_state[2].pitch_mod_enable = self.registers.pitch_mod_enable.contains(PerVoiceFlag::TWO);
				self.voices_state[3].pitch_mod_enable = self.registers.pitch_mod_enable.contains(PerVoiceFlag::THREE);
				self.voices_state[4].pitch_mod_enable = self.registers.pitch_mod_enable.contains(PerVoiceFlag::FOUR);
				self.voices_state[5].pitch_mod_enable = self.registers.pitch_mod_enable.contains(PerVoiceFlag::FIVE);
				self.voices_state[6].pitch_mod_enable = self.registers.pitch_mod_enable.contains(PerVoiceFlag::SIX);
				self.voices_state[7].pitch_mod_enable = self.registers.pitch_mod_enable.contains(PerVoiceFlag::SEVEN);
			},
			29 => {
				// NON
				self.voices_state[0].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::ZERO);
				self.voices_state[1].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::ONE);
				self.voices_state[2].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::TWO);
				self.voices_state[3].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::THREE);
				self.voices_state[4].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::FOUR);
				self.voices_state[5].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::FIVE);
				self.voices_state[6].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::SIX);
				self.voices_state[7].noise_enable = self.registers.noise_enable.contains(PerVoiceFlag::SEVEN);
				// EON
				self.voices_state[0].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::ZERO);
				self.voices_state[1].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::ONE);
				self.voices_state[2].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::TWO);
				self.voices_state[3].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::THREE);
				self.voices_state[4].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::FOUR);
				self.voices_state[5].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::FIVE);
				self.voices_state[6].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::SIX);
				self.voices_state[7].echo_enable = self.registers.echo_enable.contains(PerVoiceFlag::SEVEN);
				// DIR
				self.sample_directory = self.registers.sample_directory();
				// FLG.5
				self.echo_write_disable = self.registers.flags.contains(DspFlags::ECHO_WRITE_DISABLE);
			},
			30 => {
				// EDL
				self.echo_delay = self.registers.echo_delay_samples();
				// ESA
				self.echo_base_address = self.registers.echo_base_address();
				// Fullsnes says "KON?" but we assume that KON is actually read in cycle 31.
				// FLG.5 (again!)
				self.echo_write_disable = self.registers.flags.contains(DspFlags::ECHO_WRITE_DISABLE);
			},
			31 => {
				// KOFF, KON
				if self.current_sample_read_kon_koff {
					self.voices_state[0].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::ZERO);
					self.voices_state[1].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::ONE);
					self.voices_state[2].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::TWO);
					self.voices_state[3].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::THREE);
					self.voices_state[4].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::FOUR);
					self.voices_state[5].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::FIVE);
					self.voices_state[6].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::SIX);
					self.voices_state[7].was_keyed_on = self.registers.key_on.contains(PerVoiceFlag::SEVEN);
					self.voices_state[0].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::ZERO);
					self.voices_state[1].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::ONE);
					self.voices_state[2].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::TWO);
					self.voices_state[3].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::THREE);
					self.voices_state[4].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::FOUR);
					self.voices_state[5].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::FIVE);
					self.voices_state[6].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::SIX);
					self.voices_state[7].was_keyed_off = self.registers.key_off.contains(PerVoiceFlag::SEVEN);
				}
				// V0ADSR2 / V0GAIN
				self.voices_state[0].envelope_settings.set_adsr2(self.registers.voices[0].adsr_high);
				self.voices_state[0].envelope_settings.set_gain(self.registers.voices[0].gain_settings);
				// FLG.0-4
				self.noise_frequency = self.registers.noise_frequency();
				// FLG.7 for V0
				if self.registers.flags.contains(DspFlags::SOFT_RESET) {
					self.voices_state[0].soft_reset();
				}
			},
			_ => unreachable!(),
		}
	}

	fn read_echo(&mut self, memory: &Memory, is_left: bool) {
		if is_left {
			let lsb = memory.read(self.echo_buffer_head, false);
			let msb = memory.read(self.echo_buffer_head.wrapping_add(1), false);
			self.echo_data.0 = echo_value_from_memory(lsb, msb);
			trace!("echo read left: {}", self.echo_data.0);
		} else {
			let lsb = memory.read(self.echo_buffer_head.wrapping_add(2), false);
			let msb = memory.read(self.echo_buffer_head.wrapping_add(3), false);
			self.echo_data.1 = echo_value_from_memory(lsb, msb);
			trace!("echo read right: {}", self.echo_data.1);
		}
	}

	fn write_echo(&self, memory: &mut Memory, is_left: bool) {
		if is_left {
			let (lsb, msb) = echo_value_to_memory(self.echo_data.0);
			memory.write(self.echo_buffer_head, lsb);
			memory.write(self.echo_buffer_head.wrapping_add(1), msb);
			trace!("echo write left: {}", self.echo_data.0);
		} else {
			let (lsb, msb) = echo_value_to_memory(self.echo_data.1);
			memory.write(self.echo_buffer_head.wrapping_add(2), lsb);
			memory.write(self.echo_buffer_head.wrapping_add(3), msb);
			trace!("echo write right: {}", self.echo_data.1);
		}
	}
}

/// Returns an echo sample value from the very weirdly stored 15-bit value in memory.
#[must_use]
pub const fn echo_value_from_memory(lsb: u8, msb: u8) -> u16 {
	((lsb as u16) >> 1) | ((msb as u16) << 7)
}

/// Converts a 15-bit echo sample to the weird memory format as (lsb, msb).
#[must_use]
#[allow(clippy::cast_possible_truncation)]
pub const fn echo_value_to_memory(sample: u16) -> (u8, u8) {
	(((sample & 0x7f) << 1) as u8, ((sample >> 7) as u8))
}

/// Size of the internal BRR decode buffer that is used for pitch interpolation.
pub const BRR_DECODE_BUFFER_SIZE: usize = 12;

/// Internal per-voice state.
#[derive(Clone, Debug, Default)]
#[allow(clippy::struct_excessive_bools)]
struct VoiceState {
	// Main voice state
	/// Pre-channel mixing output value, mirrored in the OUTX register.
	output:        i16,
	/// Left channel volume, copied from hardware register.
	volume_left:   i8,
	/// Right channel volume, copied from hardware register.
	volume_right:  i8,
	/// Whether the voice was just triggered (keyed on) via the KON register. Reset after processing the start of a new
	/// sample playback.
	was_keyed_on:  bool,
	/// Whether the voice was just stopped (keyed off) via the KOFF register. Reset after processing the start of a new
	/// sample playback.
	was_keyed_off: bool,
	/// Enable noise output instead of BRR samples.
	noise_enable:  bool,
	/// Enable output to echo.
	echo_enable:   bool,

	// Pitch handling
	/// Pitch scale, copied from hardware registers.
	pitch_scale:      u16,
	/// Internal pitch counter for running the pitch processing. Effectively a fixed-point index into the decoded
	/// sample buffer.
	pitch_counter:    PitchCounter,
	/// Enable pitch modulation for this channel.
	pitch_mod_enable: bool,

	// BRR state
	/// Current BRR sample index, copied from hardware register. Has an effect as soon as KON or a loop point are
	/// encountered.
	sample_index:          u8,
	/// At which step the decoder currently is. This mainly determines what the next memory access looks like.
	brr_decoder_step:      BrrDecoderStep,
	/// Current BRR read address.
	brr_address:           u16,
	/// Last 12 decoded samples, for pitch interpolation.
	decoded_sample_buffer: [i16; BRR_DECODE_BUFFER_SIZE],
	/// Next write index for the sample buffer. 4 samples are decoded at a time (from 2 bytes in memory), so this
	/// always advances in steps of 4: 0 -> 4 -> 8 -> 0.
	brr_buffer_index:      u8,
	/// Header of the BRR block currently in processing.
	brr_header:            brr::Header,
	/// Bytes read in from memory during last memory access. This is written by the memory access logic at the
	/// appropriate time.
	brr_read_bytes:        [u8; 3],
	/// Whether the channel is playing.
	is_playing:            bool,

	// Envelope state
	/// Current envelope volume (11 bit).
	envelope_volume:       u16,
	/// Current envelope state.
	envelope_state:        EnvelopeState,
	/// Current envelope settings, copied from hardware register.
	envelope_settings:     EnvelopeSettings,
	/// Sample step counter, incremented at 32KHz. Current rate divider from `envelope_settings` determines at what
	/// value this is reset and another envelope step is taken.
	envelope_step_counter: u16,
}

macro_rules! brr_read {
	($self:ident, $memory:ident[$data_index:ident]) => {
		#[allow(unused_assignments)]
		{
			$self.brr_read_bytes[$data_index as usize] =
				$memory.read($self.brr_address.wrapping_add($data_index), false);
			trace!("BRR data read: {}", $self.brr_read_bytes[$data_index as usize]);
			$data_index += 1;
		}
	};
}

impl VoiceState {
	/// Read the sample directory entry for this voice. This is done if the voice was keyed on or when the last BRR
	/// block has hit a block with both loop and end flags (i.e. the sample needs to loop and we need to jump to the
	/// loop point given by the directory entry).
	pub fn read_sample_directory(&mut self, memory: &mut Memory, sample_directory_address: u16) {
		let directory_address = sample_directory_address.wrapping_add(u16::from(self.sample_index) * 4);
		if self.was_keyed_on {
			// Read address of sample start
			self.brr_address = memory.read_word(directory_address, false);
		}
		if self.will_loop() {
			// Read address of loop point
			self.brr_address = memory.read_word(directory_address.wrapping_add(2), false);
		}
	}

	/// Perform part 1 of the BRR data read if necessary.
	pub fn read_brr_1(&mut self, memory: &Memory) {
		if self.needs_more_brr_data() {
			let mut data_index = 0;
			if self.brr_decoder_step.will_read_three_bytes() {
				brr_read!(self, memory[data_index]);
			}
			brr_read!(self, memory[data_index]);
			self.brr_address = self.brr_address.wrapping_add(data_index);
		}
	}

	/// Perform part 2 of the BRR data read if necessary.
	pub fn read_brr_2(&mut self, memory: &Memory) {
		if self.needs_more_brr_data() {
			let mut data_index = if self.brr_decoder_step.will_read_three_bytes() { 2 } else { 1 };
			brr_read!(self, memory[data_index]);
			self.brr_address = self.brr_address.wrapping_add(1);
		}
	}

	/// Returns true if we need to jump to the loop point of the current sample to continue processing BRR data. This is
	/// therefore only true if the last sample before the loop point has been fully processed and more data is needed.
	const fn will_loop(&self) -> bool {
		self.needs_more_brr_data() && self.brr_header.flags.will_loop_afterwards()
	}

	/// Returns true if more BRR block data is needed from memory.
	#[allow(clippy::cast_possible_truncation)]
	const fn needs_more_brr_data(&self) -> bool {
		// Buffer index has fallen at least 4 samples behind the pitch index, so we can now write there.
		let buffer_write_end = (self.brr_buffer_index + 4) % (BRR_DECODE_BUFFER_SIZE as u8);
		self.pitch_counter.sample_index() >= buffer_write_end
	}

	/// ENVX register value for this voice, which are the upper 7 bits of the 11-bit envelope value.
	pub const fn envelope_output(&self) -> u8 {
		((self.envelope_volume >> 4) & 0xff) as u8
	}

	/// OUTX register value for this voice, which are the upper 8 bits of the 16-bit pre-channel mixer output value.
	pub const fn output(&self) -> u8 {
		self.output.to_le_bytes()[1]
	}

	/// Perform soft reset as per reset flag.
	pub fn soft_reset(&mut self) {
		self.envelope_volume = 0;
		self.was_keyed_off = true;
	}
}

/// Fixed-point 16-bit pitch counter. Top 4 bits are sample index, bits 11 to 3 are gauss table index, bits 2 to 0 are
/// not used directly.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
#[repr(transparent)]
struct PitchCounter(u16);
impl std::ops::Deref for PitchCounter {
	type Target = u16;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}
impl std::ops::DerefMut for PitchCounter {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl PitchCounter {
	pub const fn sample_index(self) -> u8 {
		(self.0 >> 12) as u8
	}

	pub const fn gauss_table_index(self) -> usize {
		((self.0 >> 3) & 0x1ff) as usize
	}

	pub fn set_msb(&mut self, msb: u8) {
		let [lsb, _] = self.to_le_bytes();
		self.0 = u16::from_le_bytes([lsb, msb]);
	}

	pub fn set_lsb(&mut self, lsb: u8) {
		let [_, msb] = self.to_le_bytes();
		self.0 = u16::from_le_bytes([lsb, msb]);
	}
}

/// Internal envelope state, determine how envelope volume proceeds. This is also updated during gain mode, although it
/// does not affect the envelope except for Release mode.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum EnvelopeState {
	Attack,
	Decay,
	#[default]
	Sustain,
	Release,
}

/// Internal envelope settings, rates are the full 32KHz divider after the rate table is applied.
#[derive(Clone, Copy, Debug)]
enum EnvelopeSettings {
	Adsr { attack_rate: u16, decay_rate: u16, sustain_rate: u16, sustain_level: u16 },
	FixedGain { level: u16 },
	CustomGain { rate: u16, mode: GainMode },
}

impl Default for EnvelopeSettings {
	fn default() -> Self {
		Self::Adsr { attack_rate: 0, decay_rate: 0, sustain_rate: 0, sustain_level: 0 }
	}
}

impl EnvelopeSettings {
	/// Set the ADSR1 register value.
	pub fn set_adsr1(&mut self, adsr1: u8) {
		if adsr1 & 0x80 > 1 {
			// ADSR mode.
			let attack_rate = (adsr1 & 0xf) * 2 + 1;
			let decay_rate = ((adsr1 >> 4) & 0x7) * 2 + 16;

			*self = match *self {
				Self::Adsr { sustain_rate, sustain_level, .. } => Self::Adsr {
					attack_rate: RATE_TABLE[attack_rate as usize],
					decay_rate: RATE_TABLE[decay_rate as usize],
					sustain_level,
					sustain_rate,
				},
				_ => Self::Adsr {
					attack_rate:   RATE_TABLE[attack_rate as usize],
					decay_rate:    RATE_TABLE[decay_rate as usize],
					sustain_level: 0,
					sustain_rate:  0,
				},
			};
		} else {
			// Gain mode.
			todo!()
		}
	}

	/// Set the ADSR2 register value, if applicable.
	#[allow(clippy::needless_pass_by_ref_mut)]
	pub fn set_adsr2(&mut self, adsr2: u8) {
		todo!()
	}

	/// Set the GAIN register value, if applicable.
	#[allow(clippy::needless_pass_by_ref_mut)]
	pub fn set_gain(&mut self, gain: u8) {
		todo!()
	}
}

/// Custom gain modes.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum GainMode {
	#[default]
	LinearDecrease,
	ExponentialDecrease,
	LinearIncrease,
	BentIncrease,
}

/// Internal BRR decoder state machine.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum BrrDecoderStep {
	/// BRR decoder is done with a block (read 16 samples) and will read a header next.
	#[default]
	Done,
	/// BRR decoder has just read the header and four samples.
	ReadHeader,
	/// BRR decoder has read four samples, but it's not right after the header read. This is important for handling the
	/// ENDX flag write correctly.
	Read4Samples,
	/// BRR decoder has read 8 samples.
	Read8Samples,
	/// BRR decoder has read 12 samples.
	Read12Samples,
}

impl BrrDecoderStep {
	/// Advances the decoder to the next step once the next pitch counter overflow (across a 4-sample boundary) happened
	/// and the memory accesses were executed.
	pub fn advance(&mut self) {
		*self = match *self {
			Self::Done => Self::ReadHeader,
			Self::ReadHeader | Self::Read4Samples => Self::Read8Samples,
			Self::Read8Samples => Self::Read12Samples,
			Self::Read12Samples => Self::Done,
		};
	}

	/// Returns whether the decoder will need to read three bytes of memory due to needing to read a BRR header.
	pub fn will_read_three_bytes(self) -> bool {
		self == Self::Done
	}

	/// Returns whether the decoder just read the header of an end block, which should trigger the flag write to the
	/// ENDX register.
	pub fn just_read_end_block(self) -> bool {
		self == Self::ReadHeader
	}
}
