//! DSP registers.
#![allow(clippy::module_name_repetitions)]

use std::ops::Deref;

use bitflags::bitflags;

/// All DSP registers exposed to the SMP.
#[derive(Copy, Clone, Debug)]
pub struct DspRegisters {
	/// x0-x9, per-voice registers `VxVOLL` - `VxOUTX`
	voices:               [VoiceRegisters; 8],
	/// 0C, MVOLL
	main_volume_left:     u8,
	/// 1C, MVOLR
	main_volume_right:    u8,
	/// 2C, EVOLL
	echo_volume_left:     u8,
	/// 3C, EVOLR
	echo_volume_right:    u8,
	/// 4C, KON
	key_on:               PerVoiceFlag,
	/// 5C, KOFF
	key_off:              PerVoiceFlag,
	/// 6C, FLG
	flags:                DspFlags,
	/// 7C, ENDX
	voice_end:            PerVoiceFlag,
	/// 0D, EFB
	echo_feedback_volume: u8,
	/// 2D, PMON
	pitch_mod_enable:     PerVoiceFlag,
	/// 3D, NON
	noise_enable:         PerVoiceFlag,
	/// 4D, EON
	echo_enable:          PerVoiceFlag,
	/// 5D, DIR
	sample_table_address: u8,
	/// 6D, ESA 🚀
	echo_buffer_address:  u8,
	/// 7D, EDL
	echo_delay:           u8,
	/// xF, `FIRx`
	fir_coefficients:     [u8; 8],
	/// xA, unused
	unused_a:             [u8; 8],
	/// xB, unused
	unused_b:             [u8; 8],
	/// 1D, unused
	unused_1d:            u8,
	/// xE, unused
	unused_e:             [u8; 8],
}

impl Default for DspRegisters {
	fn default() -> Self {
		Self {
			voices:               Default::default(),
			main_volume_left:     Default::default(),
			main_volume_right:    Default::default(),
			echo_volume_left:     Default::default(),
			echo_volume_right:    Default::default(),
			key_on:               PerVoiceFlag::default(),
			key_off:              PerVoiceFlag::default(),
			flags:                DspFlags::default(),
			voice_end:            PerVoiceFlag(0xff),
			echo_feedback_volume: Default::default(),
			pitch_mod_enable:     PerVoiceFlag::default(),
			noise_enable:         PerVoiceFlag::default(),
			echo_enable:          PerVoiceFlag::default(),
			sample_table_address: Default::default(),
			echo_buffer_address:  Default::default(),
			echo_delay:           Default::default(),
			fir_coefficients:     Default::default(),
			unused_a:             Default::default(),
			unused_b:             Default::default(),
			unused_1d:            Default::default(),
			unused_e:             Default::default(),
		}
	}
}

impl DspRegisters {
	/// Read from a DSP register.
	pub fn read(&mut self, address: u8) -> u8 {
		let lower_nibble = address & 0xf;
		let upper_nibble = (address >> 4) & 0x7;
		match (upper_nibble, lower_nibble) {
			(_, 0x0 ..= 0x9) => self.voices[upper_nibble as usize].read(lower_nibble),
			(_, 0xA) => self.unused_a[upper_nibble as usize],
			(_, 0xB) => self.unused_b[upper_nibble as usize],
			(_, 0xE) => self.unused_e[upper_nibble as usize],
			(0x1, 0xD) => self.unused_1d,
			(0x0, 0xC) => self.main_volume_left,
			(0x1, 0xC) => self.main_volume_right,
			(0x2, 0xC) => self.echo_volume_left,
			(0x3, 0xC) => self.echo_volume_right,
			// FIXME: it's unclear what happens if KON is read.
			(0x4, 0xC) => self.key_on.0,
			(0x5, 0xC) => self.key_off.0,
			(0x6, 0xC) => self.flags.0,
			(0x7, 0xC) => self.voice_end.0,
			(0x0, 0xD) => self.echo_feedback_volume,
			(0x2, 0xD) => self.pitch_mod_enable.0,
			(0x3, 0xD) => self.noise_enable.0,
			(0x4, 0xD) => self.echo_enable.0,
			(0x5, 0xD) => self.sample_table_address,
			(0x6, 0xD) => self.echo_buffer_address,
			(0x7, 0xD) => self.echo_delay,
			(_, 0xF) => self.fir_coefficients[upper_nibble as usize],
			_ => unreachable!(),
		}
	}

	/// Write to a DSP register.
	pub fn write(&mut self, address: u8, value: u8) {
		if address >= 0x80 {
			return;
		}
		let lower_nibble = address & 0xf;
		let upper_nibble = address >> 4;
		match (upper_nibble, lower_nibble) {
			(_, 0x0 ..= 0x9) => self.voices[upper_nibble as usize].write(lower_nibble, value),
			(_, 0xA) => self.unused_a[upper_nibble as usize] = value,
			(_, 0xB) => self.unused_b[upper_nibble as usize] = value,
			(_, 0xE) => self.unused_e[upper_nibble as usize] = value,
			(0x1, 0xD) => self.unused_1d = value,
			(0x0, 0xC) => self.main_volume_left = value,
			(0x1, 0xC) => self.main_volume_right = value,
			(0x2, 0xC) => self.echo_volume_left = value,
			(0x3, 0xC) => self.echo_volume_right = value,
			// FIXME: it's unclear what happens if KON is read.
			(0x4, 0xC) => self.key_on.0 = value,
			(0x5, 0xC) => self.key_off.0 = value,
			(0x6, 0xC) => self.flags.0 = value,
			(0x7, 0xC) => self.voice_end.0 = value,
			(0x0, 0xD) => self.echo_feedback_volume = value,
			(0x2, 0xD) => self.pitch_mod_enable.0 = value,
			(0x3, 0xD) => self.noise_enable.0 = value,
			(0x4, 0xD) => self.echo_enable.0 = value,
			(0x5, 0xD) => self.sample_table_address = value,
			(0x6, 0xD) => self.echo_buffer_address = value,
			(0x7, 0xD) => self.echo_delay = value,
			(_, 0xF) => self.fir_coefficients[upper_nibble as usize] = value,
			_ => unreachable!(),
		}
	}
}

/// Per-voice registers, x0 - xB
#[derive(Clone, Copy, Debug, Default)]
pub struct VoiceRegisters {
	/// 0, VOLL
	volume_left:    u8,
	/// 1, VOLR
	volume_right:   u8,
	/// 2, PITCHL and 3, PITCHH
	pitch:          u16,
	/// 4, SRCN
	sample_number:  u8,
	/// 5, ADSR1 (low) and 6, ADSR2 (high)
	adsr_settings:  AdsrSettings,
	/// 7, GAIN
	gain_settings:  u8,
	/// 8, ENVX
	envelope_value: u8,
	/// 9, OUTX
	output_value:   u8,
}

impl VoiceRegisters {
	#[inline]
	pub(super) fn read(&self, address: u8) -> u8 {
		match address {
			0 => self.volume_left,
			1 => self.volume_right,
			2 => self.pitch.to_le_bytes()[0],
			3 => self.pitch.to_le_bytes()[1],
			4 => self.sample_number,
			5 => self.adsr_settings.to_be_bytes()[0],
			6 => self.adsr_settings.to_be_bytes()[1],
			7 => self.gain_settings,
			8 => self.envelope_value,
			9 => self.output_value,
			_ => unreachable!(),
		}
	}

	#[inline]
	pub(super) fn write(&mut self, address: u8, value: u8) {
		match address {
			0 => self.volume_left = value,
			1 => self.volume_right = value,
			2 => self.pitch = (self.pitch & 0xf0) | u16::from(value),
			3 => self.pitch = (self.pitch & 0xf) | (u16::from(value) << 8),
			4 => self.sample_number = value,
			5 => self.adsr_settings.0 = (self.adsr_settings.0 & 0xf0) | u16::from(value),
			6 => self.adsr_settings.0 = (self.adsr_settings.0 & 0xf) | (u16::from(value) << 8),
			7 => self.gain_settings = value,
			8 => self.envelope_value = value,
			9 => self.output_value = value,
			_ => unreachable!(),
		}
	}
}

/// Register with one bit per voice.
#[derive(Clone, Copy, Debug, Default)]
#[repr(transparent)]
pub struct PerVoiceFlag(u8);

/// ADSR settings per voice, a 16-bit field.
#[derive(Clone, Copy, Debug, Default)]
#[repr(transparent)]
pub struct AdsrSettings(u16);

impl Deref for AdsrSettings {
	type Target = u16;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

/// Global DSP flags in the FLG register.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct DspFlags(u8);

impl Default for DspFlags {
	fn default() -> Self {
		Self(0xE0)
	}
}

bitflags! {
	impl PerVoiceFlag : u8 {
		/// Voice 0
		const ZERO = 1 << 0;
		/// Voice 1
		const ONE = 1 << 1;
		/// Voice 2
		const TWO = 1 << 2;
		/// Voice 3
		const THREE = 1 << 3;
		/// Voice 4
		const FOUR = 1 << 4;
		/// Voice 5
		const FIVE = 1 << 5;
		/// Voice 6
		const SIX = 1 << 6;
		/// Voice 7
		const SEVEN = 1 << 7;
	}

	impl DspFlags : u8 {
		/// Noise frequency
		const NOISE_FREQUENCY = 0xf;
		/// Disable echo writes
		const ECHO_WRITE_DISABLE = 1 << 5;
		/// Mute analog amplifier
		const AMPLIFIER_MUTE = 1 << 6;
		/// Key-off all voices and set all envelopes to 0
		const SOFT_RESET = 1 << 7;
	}
}
