//! WAV file handling and conversion.

use std::fs::File;

#[allow(unused)]
use smartstring::alias::String;
use wav::{read as wav_read, BitDepth};

use super::DecodedSample;

const i24max: f64 = (0xff_ffff - 1) as f64;

/// Treats the given file as a WAV file, reads its samples and converts them to 16-bit mono. Note that there currently
/// is no sample rate conversion.
///
/// # Errors
/// Any errors from the WAV support library are passed on, as well as some custom errors.
pub fn read_wav_for_brr(mut file: File) -> Result<Vec<DecodedSample>, String> {
	let (header, data) = wav_read(&mut file).map_err(|err| String::from(err.to_string()))?;
	convert_sample_format(data, header.channel_count)
}

/// Convert the sample format to signed 16 bit mono.
fn convert_sample_format(source_data: BitDepth, channels: u16) -> Result<Vec<DecodedSample>, String> {
	let s16bit_data = convert_bit_depth_to_16_bits(source_data)?;
	if channels > 1 {
		average_channels(&s16bit_data, channels)
	} else {
		Ok(s16bit_data)
	}
}

/// Convert all samples to 16 bits.
fn convert_bit_depth_to_16_bits(source_data: BitDepth) -> Result<Vec<i16>, String> {
	match source_data {
		BitDepth::Sixteen(data) => Ok(data),
		BitDepth::Eight(u8bit) => Ok(u8bit
			.into_iter()
			// The order of operations is very important! If we don't bracket the scaling constant at the end, we will overflow the first value into oblivion.
			.map(|sample| (i16::from(sample) - i16::from(u8::MAX / 2)) * (i16::MAX / i16::from(u8::MAX)))
			.collect()),
		// It appears that we can only downsample effectively with an intermediary floating-point step.
		// The problem is that the i16max / i24max is less than 1, so cannot be calculated in integer math.
		BitDepth::TwentyFour(s24bit) =>
			Ok(s24bit.into_iter().map(|sample| (f64::from(sample) / i24max * f64::from(i16::MAX)) as i16).collect()),
		BitDepth::ThirtyTwoFloat(f32bit) =>
			Ok(f32bit.into_iter().map(|sample| (sample * f32::from(i16::MAX)) as i16).collect()),
		BitDepth::Empty => Err("Empty audio file".to_owned().into()),
	}
}

/// Convert any number of channels to mono by averaging all channels together.
fn average_channels(multichannel_data: &[i16], channels: u16) -> Result<Vec<DecodedSample>, String> {
	debug_assert!(channels > 1);
	let mut result = Vec::with_capacity(multichannel_data.len() / channels as usize);
	let mut multichannel_data_iterator = multichannel_data.iter();
	while !multichannel_data_iterator.is_empty() {
		let mut next_sample: i64 = 0;
		for i in 0 .. channels {
			next_sample += i64::from(*multichannel_data_iterator.next().ok_or_else(|| {
				format!("{}-channel audio, but last frame only contains {} samples", channels, i - 1)
			})?);
		}
		result.push((next_sample / i64::from(channels)) as i16);
	}
	Ok(result)
}
