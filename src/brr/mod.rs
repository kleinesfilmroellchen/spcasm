//! BRR (Bit Rate Reduced) a.k.a. SNES ADPCM sample format parsing, writing and reading.
#![allow(
	clippy::cast_possible_wrap,
	clippy::cast_possible_truncation,
	clippy::cast_sign_loss,
	clippy::fallible_impl_from,
	clippy::use_self
)]

use std::convert::TryInto;

use az::Az;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[cfg(test)] mod test;

/// Only the lower 4 bits are used.
pub type EncodedSample = u8;
/// The DAC internally uses 16-bit signed samples.
pub type DecodedSample = i16;
/// This matches the fixed-point arithmetic (16.16) used in the SNES DAC.
type FilterCoefficient = fixed::FixedI32<fixed::types::extra::U16>;

/// A block's samples, decoded.
pub type DecodedBlockSamples = [DecodedSample; 16];
/// A block's samples, encoded.
pub type EncodedBlockSamples = [EncodedSample; 16];

type FilterCoefficients = [FilterCoefficient; 2];
type WarmUpSamples = [DecodedSample; 2];
/// A block's samples' representation internally in the DAC.
type DecimalBlockSamples = [FilterCoefficient; 16];

/// A 9-byte encoded BRR block.
///
/// Each BRR block starts with a header byte followed by 8 sample bytes. Each sample byte in turn holds 2 4-bit samples.
/// each.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Block {
	/// The header of this block.
	pub header:          Header,
	/// The encoded samples in this block.
	pub encoded_samples: EncodedBlockSamples,
}

// Most of the encode/decode functions can *almost* be const... but the loops and iterators destroy everything even
// though they are even unrolled on optimization level 3.
impl Block {
	/// Create a new block from already-encoded data.
	#[must_use]
	pub const fn new(header: Header, encoded_samples: EncodedBlockSamples) -> Self {
		Self { header, encoded_samples }
	}

	/// Encode the given sample block using the most accurate filter possible. The given warm-up samples are possibly
	/// used in some filter types and should come from the previously encoded filter block.
	#[must_use]
	#[allow(clippy::missing_panics_doc)]
	pub fn encode(warm_up_samples: WarmUpSamples, samples: DecodedBlockSamples, flags: LoopEndFlags) -> Self {
		// FIXME: This might be too slow. We try all four filters and return the most accurate encode.
		[
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::Zero, flags),
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::One, flags),
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::Two, flags),
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::Three, flags),
		]
		.into_iter()
		.min_by_key(|encoded| encoded.total_encode_error(warm_up_samples, &samples))
		.unwrap()
	}

	/// Calculate and return the total encoding error that this block has, given that it was encoded from the
	/// `real_samples`. This is an important internal utility function for finding the best filter for these samples.
	#[must_use]
	pub fn total_encode_error(&self, warm_up_samples: WarmUpSamples, real_samples: &DecodedBlockSamples) -> u128 {
		let (decoded, _) = self.decode(warm_up_samples);
		decoded.iter().zip(real_samples.iter()).map(|(actual, expected)| u128::from(actual.abs_diff(*expected))).sum()
	}

	/// Encode the given sample block using the given filter.
	#[must_use]
	pub fn encode_with_filter(
		warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		filter: LPCFilter,
		flags: LoopEndFlags,
	) -> Self {
		let (encoded, real_shift) = Self::internal_encode_lpc(warm_up_samples, samples, filter.coefficient());
		Self { header: Header { real_shift, filter, flags }, encoded_samples: encoded }
	}

	fn internal_encode_lpc(
		mut warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		filter_coefficients: FilterCoefficients,
	) -> (EncodedBlockSamples, i8) {
		let mut previous_samples = [fixed(warm_up_samples[0]), fixed(warm_up_samples[1])];
		// Let's first encode without concerning ourselves with the shift amount. That can be dealt with later.
		let mut decimal_encoded_samples = [FilterCoefficient::ZERO; 16];
		for (decimal_encoded, sample) in decimal_encoded_samples.iter_mut().zip(samples.iter()) {
			let fixed_sample = fixed(*sample);
			*decimal_encoded = fixed_sample
				- previous_samples[0].checked_div(filter_coefficients[0]).unwrap_or(FilterCoefficient::ZERO)
				- previous_samples[1].checked_div(filter_coefficients[1]).unwrap_or(FilterCoefficient::ZERO);
			previous_samples[1] = previous_samples[0];
			previous_samples[0] = fixed_sample;
		}
		let necessary_shift = Self::necessary_shift_for(&decimal_encoded_samples);
		let mut encoded = [0; 16];
		for (encoded, sample) in encoded.iter_mut().zip(decimal_encoded_samples.iter()) {
			*encoded = sample.floor().az::<i16>().wrapping_shr(necessary_shift) as u8;
		}

		(encoded, necessary_shift as i8)
	}

	/// Computes the necessary shift in order to get all of the given samples in range for 4-bit signed integers.
	///
	/// # Implementation details
	/// *This section tries to explain some of the reasoning behind the innocent-looking logic below.*
	///
	/// First of all, we need to realize that the necessary shift depends on the necessary bits for a number, computable
	/// with (floored) logarithm base 2. As we only need to deal with bit counts above 4, we can subtract 4 from that
	/// and clamp at 0? The first problem is that the logarithm effectively gives us a zero-based index, so log = 0
	/// means that we need 1 bit for the value. Therefore, we instead subtract 3 from the bit index so that if the
	/// highest bit set is the fourth bit (the lowest bit in the second nybble!), we indicate a shift of 1.
	///
	/// The next concern is about negative and positive numbers. The output must be 4-bit signed, so the highest bit in
	/// the nybble is still a sign bit! Therefore, the shift must be even one more than previously expected to retain
	/// the sign bit, and we only want to subtract 2.
	///
	/// If you're still not satisfied: 0 -> 0 to indicate no shift. Also, input data is rounded to zero as I *think*
	/// that's what the DSP does as well.
	fn necessary_shift_for(samples: &DecimalBlockSamples) -> u32 {
		samples
			.iter()
			.map(|x| {
				let rounded = x.floor().az::<i16>();
				match rounded {
					i16::MIN ..= -1 => (-rounded).ilog2(),
					0 => 0,
					1.. => rounded.ilog2(),
				}
			})
			.max()
			.unwrap_or(0)
			// Only shift as far as necessary to get the most significant bits within the 4-bit value we can store.
			.saturating_sub(2)
	}

	/// Returns whether playback will end or loop after this block.
	#[must_use]
	pub const fn is_end(&self) -> bool {
		self.header.flags.is_end()
	}

	/// Returns whether playback will loop after this block.
	#[must_use]
	pub const fn is_loop(&self) -> bool {
		self.header.flags.will_loop_afterwards()
	}

	/// Decodes this block and returns the decoded samples as well as the last two internal fixed-point samples used for
	/// filters. These two should be fed into the next decode call as the warm-up samples.
	#[must_use]
	pub fn decode(&self, warm_up_samples: WarmUpSamples) -> (DecodedBlockSamples, WarmUpSamples) {
		self.internal_decode_lpc(warm_up_samples, self.header.filter.coefficient())
	}

	fn internal_decode_lpc(
		&self,
		mut warm_up_samples: WarmUpSamples,
		filter_coefficients: FilterCoefficients,
	) -> (DecodedBlockSamples, WarmUpSamples) {
		let mut decoded_samples: DecodedBlockSamples = [0; 16];
		for (decoded, encoded) in decoded_samples.iter_mut().zip(self.encoded_samples) {
			// TODO: Check whether overflowing (i.e. wrap-around) arithmetic is correct here.
			// The convoluted cast ensures we retain the nybble sign bit.
			let decimal_decoded = fixed(i16::from(((encoded as i8) << 4) >> 4) << self.header.real_shift)
				.wrapping_add(filter_coefficients[0].wrapping_mul(fixed(warm_up_samples[0]))).floor()
				.wrapping_add(filter_coefficients[1].wrapping_mul(fixed(warm_up_samples[1]))).floor();
			*decoded = decimal_decoded.az::<i16>();
			// Shift last samples through the buffer
			warm_up_samples[1] = warm_up_samples[0];
			warm_up_samples[0] = *decoded;
		}
		(decoded_samples, warm_up_samples)
	}
}

fn fixed(int: i16) -> FilterCoefficient {
	FilterCoefficient::from(int)
}

fn fixed_arr(ints: DecodedBlockSamples) -> DecimalBlockSamples {
	let mut ret: DecimalBlockSamples = [fixed(0); 16];
	for (ret, int) in ret.iter_mut().zip(ints.into_iter()) {
		*ret = fixed(int);
	}
	ret
}

impl From<[u8; 9]> for Block {
	fn from(data: [u8; 9]) -> Self {
		Self {
			header:          data[0].into(),
			encoded_samples: split_bytes_into_nybbles(data[1 .. 9].try_into().unwrap()),
		}
	}
}

fn split_bytes_into_nybbles(bytes: [u8; 8]) -> EncodedBlockSamples {
	let mut nybbles: EncodedBlockSamples = [0; 16];
	for (index, byte) in bytes.into_iter().enumerate() {
		let index = index * 2;
		// most significant nybble order (is that even a word?)
		nybbles[index] = (byte & 0xf0) >> 4;
		nybbles[index + 1] = byte & 0x0f;
	}
	nybbles
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
pub struct Header {
	/// The real amount of shift this header specifies for the samples in its block.
	pub real_shift: i8,
	/// The filter the header specifies for its block.
	pub filter:     LPCFilter,
	/// Flags for the header's block.
	pub flags:      LoopEndFlags,
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
pub enum LPCFilter {
	/// Filter 0, verbatim samples.
	Zero = 0,
	/// Filter 1, differential coding.
	One = 1,
	/// Filter 2, something extremely close to polynomial order 2 predictive coding (which would be s[t] + 2* s[t-1] -
	/// s[t-2]).
	Two = 2,
	/// Filter 3, again very close to polynomial order 2 predictive coding but more off than filter 2. I can't make
	/// sense of this one.
	Three = 3,
}

impl LPCFilter {
	/// Return the coefficients used by this filter. The first coefficent is for the last sample, while the second
	/// coefficient is for the second-to-last sample.
	#[must_use]
	pub fn coefficient(self) -> FilterCoefficients {
		match self {
			Self::Zero => [0i16.into(), 0i16.into()],
			Self::One => [FilterCoefficient::from_num(15) / 16, 0i16.into()],
			Self::Two => [FilterCoefficient::from_num(61) / 32, -FilterCoefficient::from_num(15) / 16],
			Self::Three => [FilterCoefficient::from_num(115) / 64, -FilterCoefficient::from_num(13) / 16],
		}
	}

	/// Return all possible filters.
	#[must_use]
	pub const fn all_filters() -> [Self; 4] {
		[Self::Zero, Self::One, Self::Two, Self::Three]
	}
}

impl std::fmt::Display for LPCFilter {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", *self as u8)
	}
}

/// Loop and end flags used in the BRR block header to determine sample end and looping.
#[derive(Clone, Copy, Debug, Eq, FromPrimitive)]
#[repr(u8)]
pub enum LoopEndFlags {
	/// Nothing special happens, this is a normal sample.
	Nothing = 0,
	/// End the sample playback without looping.
	EndWithoutLooping = 1,
	/// The loop flag is set, but it has no effect without the end flag being set.
	Ignored = 2,
	/// Loop back to the sample specified in the sample table.
	Loop = 3,
}

impl PartialEq for LoopEndFlags {
	fn eq(&self, other: &Self) -> bool {
		self.eq_(*other)
	}
}

impl LoopEndFlags {
	/// Creates combined flags.
	#[must_use]
	#[allow(clippy::missing_panics_doc)]
	pub fn new(is_end: bool, is_loop: bool) -> Self {
		Self::from_u8(if is_loop { 0b10 } else { 0 } | if is_end { 0b01 } else { 0 }).unwrap()
	}

	const fn eq_(self, other: Self) -> bool {
		self as u8 == other as u8
	}

	/// Returns whether these flags signal the end of the sample.
	#[must_use]
	pub const fn is_end(self) -> bool {
		self as u8 & 0b01 > 0
	}

	/// Returns whether these flags signal that after this block, playback should jump back to the loop point specified
	/// in the sample table. Note that this raw loop flag only has effect if ``is_end`` is set.
	#[must_use]
	pub const fn is_raw_loop(self) -> bool {
		self as u8 & 0b10 > 0
	}

	/// Returns whether the playback will *actually* loop after this sample, i.e. jump to the loop point. This is
	/// different from the raw loop flag (``Self::is_raw_loop``), which has no effect without the end flag.
	#[must_use]
	pub const fn will_loop_afterwards(self) -> bool {
		self.eq_(Self::Loop)
	}
}
