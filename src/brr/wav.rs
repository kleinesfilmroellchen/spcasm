//! WAV file handling and conversion.

use std::fs::File;
use std::io::{BufReader, Read};

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use hound::{SampleFormat, WavReader};

use super::DecodedSample;

const i24max: f64 = (0xff_ffff - 1) as f64;

/// Treats the given file as a WAV file, reads its samples and converts them to 16-bit mono. Note that there currently
/// is no sample rate conversion.
///
/// # Errors
/// Any errors from the WAV support library are passed on, as well as some custom errors.
pub fn read_wav_for_brr(file: File) -> Result<Vec<DecodedSample>, SharedStr> {
	let reader = WavReader::new(BufReader::new(file)).map_err(|err| SharedStr::from(err.to_string()))?;
	convert_sample_format(reader)
}

/// Convert the sample format to signed 16 bit mono.
fn convert_sample_format<R: Read>(reader: WavReader<R>) -> Result<Vec<DecodedSample>, SharedStr> {
	let channels = reader.spec().channels;
	let s16bit_data = convert_bit_depth_to_16_bits(reader)?;
	if channels > 1 { average_channels(&s16bit_data, channels) } else { Ok(s16bit_data) }
}

/// Convert all samples to 16 bits.
fn convert_bit_depth_to_16_bits<R: Read>(mut reader: WavReader<R>) -> Result<Vec<i16>, SharedStr> {
	match reader.spec().bits_per_sample {
		16 => reader.samples::<i16>().try_collect(),
		8 => reader.samples::<i8>()
			// The order of operations is very important! If we don't bracket the scaling constant at the end, we will overflow the first value into oblivion.
			.map(|sample| Ok((i16::from(sample?) - i16::from(u8::MAX / 2)) * (i16::MAX / i16::from(u8::MAX))))
			.try_collect(),
		// It appears that we can only downsample effectively with an intermediary floating-point step.
		// The problem is that the i16max / i24max is less than 1, so cannot be calculated in integer math.
		24 => reader
			.samples::<i32>()
			.map(|sample| Ok((f64::from(sample?) / i24max * f64::from(i16::MAX)) as i16))
			.try_collect(),
		32 if reader.spec().sample_format == SampleFormat::Float =>
			reader.samples::<f32>().map(|sample| Ok((sample? * f32::from(i16::MAX)) as i16)).try_collect(),
		_ => Err(hound::Error::FormatError("Unsupported bit depth")),
	}
	.map_err(|e| e.to_string().into())
}

/// Convert any number of channels to mono by averaging all channels together.
fn average_channels(multichannel_data: &[i16], channels: u16) -> Result<Vec<DecodedSample>, SharedStr> {
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
