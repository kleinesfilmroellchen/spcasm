//! BRR (Bit Rate Reduced) a.k.a. SNES ADPCM sample format parsing, writing and reading.
#![allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation, clippy::fallible_impl_from, clippy::use_self)]

use std::convert::TryInto;

use fixed::traits::LossyInto;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[cfg(test)] mod test;

/// Only the lower 4 bits are used.
type EncodedSample = u8;
/// The DAC internally uses 16-bit signed samples.
type DecodedSample = i16;
/// This matches the fixed-point arithmetic (16.16) used in the SNES DAC.
type FilterCoefficient = fixed::FixedI32<fixed::types::extra::U16>;

type DecodedBlockSamples = [DecodedSample; 16];
type EncodedBlockSamples = [EncodedSample; 16];

type FilterCoefficients = [FilterCoefficient; 2];

/// A 9-byte encoded BRR block.
///
/// Each BRR block starts with a header byte followed by 8 sample bytes. Each sample byte in turn holds 2 4-bit samples.
/// each.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Block {
	header:          Header,
	encoded_samples: EncodedBlockSamples,
}

// Most of the encode/decode functions can *almost* be const...
impl Block {
	pub const fn new(header: Header, encoded_samples: EncodedBlockSamples) -> Self {
		Self { header, encoded_samples }
	}

	/// Encode the given sample block using the most accurate filter possible. The given
	pub fn encode(warm_up_samples: [DecodedSample; 2], samples: DecodedBlockSamples, flags: LoopEndFlags) -> Self {
		todo!()
	}

	/// Encode the given sample block using filter 0. This is necessary for the first block of a sample and recommended
	/// for the first block at the loop point.
	pub fn encode_0(samples: DecodedBlockSamples, flags: LoopEndFlags) -> Self {
		const zeroes: [DecodedSample; 2] = [0, 0];
		let (encoded, real_shift) = Self::internal_encode_0(zeroes, samples);
		Self { header: Header { filter: LPCFilter::Zero, flags, real_shift }, encoded_samples: encoded }
	}

	#[allow(clippy::cast_sign_loss)]
	fn internal_encode_0(
		_warm_up_samples: [DecodedSample; 2],
		samples: DecodedBlockSamples,
	) -> (EncodedBlockSamples, i8) {
		// Comparing the unsigned values means that negative values are always larger, as they have their highest bit
		// set. This is intended!
		let maximum_sample = *samples.iter().max_by_key(|x| **x as u16).unwrap_or(&0);
		let maximum_bits_used = match maximum_sample {
			i16::MIN ..= -1 => i16::BITS,
			0 => 0,
			_ => maximum_sample.ilog2(),
		};
		// Only shift as far as necessary to get the most significant bits within the 4-bit value we can store.
		let necessary_shift = maximum_bits_used.saturating_sub(4);
		let mut encoded = [0; 16];
		for (encoded, sample) in encoded.iter_mut().zip(samples.iter()) {
			*encoded = sample.wrapping_shr(necessary_shift) as u8;
		}

		(encoded, necessary_shift as i8)
	}

	pub const fn is_end(&self) -> bool {
		self.header.flags.is_end()
	}

	pub const fn is_loop(&self) -> bool {
		self.header.flags.is_loop()
	}

	pub fn decode(&self, warm_up_samples: [DecodedSample; 2]) -> (DecodedBlockSamples, FilterCoefficients) {
		(match self.header.filter {
			LPCFilter::Zero => Self::internal_decode_0,
			LPCFilter::One => Self::internal_decode_1,
			LPCFilter::Two => Self::internal_decode_2,
			LPCFilter::Three => Self::internal_decode_3,
		})(self, warm_up_samples)
	}

	fn internal_decode_0(&self, _warm_up_samples: [DecodedSample; 2]) -> (DecodedBlockSamples, FilterCoefficients) {
		let mut decoded_samples: DecodedBlockSamples = [0; 16];
		for (decoded, encoded) in decoded_samples.iter_mut().zip(self.encoded_samples) {
			// This conversion pipeline ensures that signed numbers stay signed.
			*decoded = i16::from(encoded as i8) << self.header.real_shift;
		}
		(decoded_samples, [fixed(decoded_samples[14]), fixed(decoded_samples[15])])
	}

	fn internal_decode_1(&self, mut warm_up_samples: [DecodedSample; 2]) -> (DecodedBlockSamples, FilterCoefficients) {
		self.internal_decode_lpc([fixed(warm_up_samples[0]), fixed(warm_up_samples[1])], [
			FilterCoefficient::from_num(15) / 16,
			0i16.into(),
		])
	}

	fn internal_decode_2(&self, warm_up_samples: [DecodedSample; 2]) -> (DecodedBlockSamples, FilterCoefficients) {
		self.internal_decode_lpc([fixed(warm_up_samples[0]), fixed(warm_up_samples[1])], [
			FilterCoefficient::from_num(61) / 32,
			FilterCoefficient::from_num(15) / 16,
		])
	}

	fn internal_decode_3(&self, warm_up_samples: [DecodedSample; 2]) -> (DecodedBlockSamples, FilterCoefficients) {
		self.internal_decode_lpc([fixed(warm_up_samples[0]), fixed(warm_up_samples[1])], [
			FilterCoefficient::from_num(115) / 64,
			FilterCoefficient::from_num(13) / 16,
		])
	}

	fn internal_decode_lpc(
		&self,
		mut warm_up_samples: FilterCoefficients,
		filter_coefficients: FilterCoefficients,
	) -> (DecodedBlockSamples, FilterCoefficients) {
		let mut decoded_samples: DecodedBlockSamples = [0; 16];
		for (decoded, encoded) in decoded_samples.iter_mut().zip(self.encoded_samples) {
			let decimal_decoded = fixed(i16::from(encoded as i8) << self.header.real_shift)
				+ filter_coefficients[0] * warm_up_samples[0]
				+ filter_coefficients[1] * warm_up_samples[0];
			*decoded = decimal_decoded.lossy_into();
			// Shift last samples through the buffer
			warm_up_samples[1] = warm_up_samples[0];
			warm_up_samples[0] = decimal_decoded;
		}
		(decoded_samples, warm_up_samples)
	}
}

fn fixed(int: i16) -> FilterCoefficient {
	FilterCoefficient::from(int)
}

impl From<[u8; 9]> for Block {
	fn from(data: [u8; 9]) -> Self {
		Self { header: data[0].into(), encoded_samples: data[1 .. 9].try_into().unwrap() }
	}
}

/// A BRR header byte.
///
/// In memory the BRR header is laid out as: `ssssffle`
/// * `ssss` is the "shift value", equivalent to AIFF's or FLAC's wasted bits per sample: <https://datatracker.ietf.org/doc/html/draft-ietf-cellar-flac-04#section-10.2.2>.
///   This determines how many bits the decoded samples are shifted left, minus 1. E.g. 0011 = shift left twice, 0001 =
///   don't shift, 0000 = shift right once (!). In the data structure, we store the actual shift amount, with -1
///   representing the right shift.
/// * `ff` is the "filter", or linear predictor used by the sample. The four filters all at maximum use the last two
///   samples ("t-1" and "t-2") in addition to the current sample to compute the output sample. For each filter, the
///   coefficients are:
///
/// | `ff` | "t-1" factor | "t-2" factor | explanation                         |
/// |------|--------------|--------------|-------------------------------------|
/// | 0    | 0            | 0            | verbatim samples                    |
/// | 1    | 15/16 ~ 1    | 0            | delta/differential coding           |
/// | 2    | 61/32 ~ 2    | -15/16 ~ -1  | almost polynomial order 2 predictor |
/// | 3    | 115/64 ~ 2   | -13/16 ~ -1  | ???                                 |
///
/// * `l` is the loop bit, indicating whether playback should loop to the loop point (specified in the sample table)
///   after this block. Note that a `l` bit set without an `e` bit set has no effect.
/// * `e` is the end bit, indicating that the sample has ended.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Header {
	real_shift: i8,
	filter:     LPCFilter,
	flags:      LoopEndFlags,
}

impl From<u8> for Header {
	fn from(data: u8) -> Self {
		Self {
			real_shift: (((data >> 4) & 0xF) as i8) - 1,
			filter:     LPCFilter::from_u8((data >> 2) & 0b11).unwrap(),
			flags:      LoopEndFlags::from_u8(data & 0b11).unwrap(),
		}
	}
}

/// Linear predictive coding (LPC) filters used by the BRR format. Filters are specified per BRR block.
///
/// The four filters all at maximum use the last two samples ("t-1" and "t-2") in addition to the current sample to
/// compute the output sample. The coefficient for the current sample, after shifting, is 1. For each filter, the other
/// coefficients are:
///
/// | `ff` | "t-1" factor | "t-2" factor | explanation                         |
/// |------|--------------|--------------|-------------------------------------|
/// | 0    | 0            | 0            | verbatim samples                    |
/// | 1    | 15/16 ~ 1    | 0            | delta/differential coding           |
/// | 2    | 61/32 ~ 2    | -15/16 ~ -1  | almost polynomial order 2 predictor |
/// | 3    | 115/64 ~ 2   | -13/16 ~ -1  | ???                                 |
#[derive(Clone, Copy, Debug, Eq, PartialEq, FromPrimitive)]
#[repr(u8)]
enum LPCFilter {
	/// Filter 0, verbatim samples.
	Zero = 0,
	/// Filter 1, differential coding.
	One = 1,
	/// Filter 2, something extremely close to polynomial order 2 predictive coding (which would be s[t] + 2* s[t-1] -
	/// s[t-2]).
	Two = 2,
	/// Filter 3, again very close to polynomial order 3 predictive coding but more off than filter 2. I can't make
	/// sense of this one.
	Three = 3,
}

/// Loop and end flags used in the BRR block header to determine sample end and looping.
#[derive(Clone, Copy, Debug, Eq, PartialEq, FromPrimitive)]
#[repr(u8)]
enum LoopEndFlags {
	/// Nothing special happens, this is a normal sample.
	Nothing = 0,
	/// End the sample playback without looping.
	EndWithoutLooping = 1,
	/// The loop flag is set, but it has no effect without the end flag being set.
	Ignored = 2,
	/// Loop back to the sample specified in the sample table.
	Loop = 3,
}

impl LoopEndFlags {
	pub fn new(is_end: bool, is_loop: bool) -> Self {
		Self::from_u8(if is_loop { 0b10 } else { 0 } | if is_end { 0b01 } else { 0 }).unwrap()
	}

	pub const fn is_end(self) -> bool {
		self as u8 & 0b01 > 0
	}

	pub const fn is_loop(self) -> bool {
		self as u8 & 0b10 > 0
	}
}
