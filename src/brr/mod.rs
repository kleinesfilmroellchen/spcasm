//! BRR (Bit Rate Reduced) a.k.a. SNES ADPCM sample format parsing, writing and reading.
#![allow(
	clippy::cast_possible_wrap,
	clippy::cast_possible_truncation,
	clippy::cast_sign_loss,
	clippy::fallible_impl_from,
	clippy::use_self
)]

use std::convert::TryInto;

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[cfg(test)] mod test;

pub mod dsp;
pub mod wav;

/// Only the lower 4 bits are used.
pub type EncodedSample = u8;
/// The DAC internally uses 16-bit signed samples.
pub type DecodedSample = i16;

/// A block's samples, decoded.
pub type DecodedBlockSamples = [DecodedSample; 16];
/// A block's samples, encoded.
pub type EncodedBlockSamples = [EncodedSample; 16];

/// In the DSP hardware, decimal multiplication is simulated with addition and shifting. We therefore use functions that
/// provide the hardware calculation simulating the multiplication. See [`LPCFilter`] for the implementations of the
/// various shift functions.
type FilterCoefficients = [fn(i16) -> i16; 2];
type WarmUpSamples = [DecodedSample; 2];

/// The compression level the encoder should use.
#[derive(Debug, Clone, Copy, FromPrimitive)]
#[repr(u8)]
pub enum CompressionLevel {
	/// Use filter 0 with non-wrapping optimal shift for all blocks.
	OnlyFilterZero = 0,
	/// Use the optimal filter, but use the non-wrapping optimal shift.
	EstimateShift = 1,
	/// Brute-force all filters and shifts.
	Max = 2,
}

impl CompressionLevel {
	pub(super) const fn estimates_shift(self) -> bool {
		match self {
			Self::EstimateShift | Self::OnlyFilterZero => true,
			Self::Max => false,
		}
	}
}

/// Encode the given 16-bit samples as BRR samples. The data may be padded via repetition to fit a multiple of 16; if
/// you don't want this to happen, provide a multiple of 16 samples.
///
/// Set the loop point to None if the sample is not looped.
#[must_use]
#[allow(clippy::module_name_repetitions)]
pub fn encode_to_brr(
	samples: &mut Vec<DecodedSample>,
	loop_point: Option<usize>,
	compression: CompressionLevel,
) -> Vec<u8> {
	if samples.is_empty() {
		return Vec::new();
	}

	let first_block_encoder = if compression.estimates_shift() {
		Block::encode_with_filter_good_shift
	} else {
		Block::encode_with_filter_best
	};
	let main_block_encoder = match compression {
		CompressionLevel::OnlyFilterZero => Block::encode_with_filter_0_good_shift,
		CompressionLevel::EstimateShift => Block::encode_with_good_shift,
		CompressionLevel::Max => Block::encode,
	};

	#[cfg(debug_assertions)]
	let mut filter_type_counts: [usize; 4] = [1, 0, 0, 0];

	if samples.len() % 16 != 0 {
		let needed_elements = 16 - (samples.len() % 16);
		samples.splice(0 .. 0, [0].repeat(needed_elements));
	}

	let (sample_chunks, maybe_last_chunk) = samples.as_chunks::<16>();
	debug_assert!(maybe_last_chunk.is_empty());
	debug_assert!(!sample_chunks.is_empty());

	let mut sample_chunks = sample_chunks.to_vec();
	let first_chunk = sample_chunks.remove(0);
	let chunk_count = sample_chunks.len();

	// Determine whether the first chunk is also the last; i.e. there are exactly 16 samples.
	let first_block_is_end = sample_chunks.len() == 1;

	// This should allocate enough so the vector never needs to reallocate.
	let mut result = Vec::with_capacity(samples.len() / 2 + sample_chunks.len() + 9);

	// The first chunk must be encoded with filter 0 to prevent glitches.
	let first_block_data = first_block_encoder(
		[0, 0],
		first_chunk,
		LPCFilter::Zero,
		LoopEndFlags::new(first_block_is_end, first_block_is_end && loop_point.is_some()),
	);
	result.extend_from_slice(&<[u8; 9]>::from(first_block_data));
	let mut warm_up: WarmUpSamples = [first_chunk[first_chunk.len() - 1], first_chunk[first_chunk.len() - 2]];
	for (i, chunk) in sample_chunks.into_iter().enumerate() {
		let flags =
			if i == chunk_count - 1 { LoopEndFlags::new(true, loop_point.is_some()) } else { LoopEndFlags::Nothing };

		let block_data = if let Some(point) = loop_point
			&& ((point & !0b1111) == i * 16)
		{
			Block::encode_with_filter_0_good_shift(warm_up, chunk, flags)
		} else {
			main_block_encoder(warm_up, chunk, flags)
		};

		#[cfg(debug_assertions)]
		{
			filter_type_counts[block_data.header.filter as u8 as usize] += 1;
		}

		result.extend_from_slice(&<[u8; 9]>::from(block_data));
		warm_up = [chunk[chunk.len() - 1], chunk[chunk.len() - 2]];
	}

	#[cfg(debug_assertions)]
	println!(
		"Encoded {} blocks (0: {}, 1: {}, 2: {}, 3: {})",
		filter_type_counts.iter().sum::<usize>(),
		filter_type_counts[0],
		filter_type_counts[1],
		filter_type_counts[2],
		filter_type_counts[3]
	);

	result
}

/// Decode samples from a stream of BRR blocks. Note that this discards loop data and any potential end point is
/// disregarded; this function always decodes all the input data.
///
/// # Errors
/// All possible header bytes are valid on some level, so no errors are thrown because of this. If the encoded data does
/// not line up with a BRR block however, an error is returned.
#[allow(clippy::module_name_repetitions)]
pub fn decode_from_brr(encoded: &[u8]) -> Result<Vec<DecodedSample>, SharedStr> {
	let (blocks, remainder) = encoded.as_chunks();
	if !remainder.is_empty() {
		return Err(format!("Cut off BRR block (size {}) at the end of the stream", remainder.len()).into());
	}

	let mut decoded_samples = Vec::with_capacity(blocks.len() * 16);
	let mut previous_samples: [DecodedSample; 2] = [0, 0];

	#[cfg(debug_assertions)]
	let mut filter_type_counts: [usize; 4] = [0, 0, 0, 0];

	for raw_block in blocks {
		let block = Block::from(*raw_block);
		let (samples, new_previous_samples) = block.decode(previous_samples);

		#[cfg(debug_assertions)]
		{
			filter_type_counts[block.header.filter as u8 as usize] += 1;
		}

		decoded_samples.extend_from_slice(&samples);
		previous_samples = new_previous_samples;
	}

	#[cfg(debug_assertions)]
	println!(
		"Decoded {} blocks (0: {}, 1: {}, 2: {}, 3: {})",
		filter_type_counts.iter().sum::<usize>(),
		filter_type_counts[0],
		filter_type_counts[1],
		filter_type_counts[2],
		filter_type_counts[3]
	);

	Ok(decoded_samples)
}

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
		[
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::Zero, flags),
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::One, flags),
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::Two, flags),
			Self::encode_with_filter(warm_up_samples, samples, LPCFilter::Three, flags),
		]
		.into_iter()
		.flatten()
		.min_by_key(|encoded| encoded.total_encode_error(warm_up_samples, &samples))
		.unwrap()
	}

	/// Calculate and return the total encoding error that this block has, given that it was encoded from the
	/// `real_samples`. This is an important internal utility function for finding the best filter for these samples.
	#[must_use]
	pub fn total_encode_error(&self, warm_up_samples: WarmUpSamples, real_samples: &DecodedBlockSamples) -> u128 {
		let (decoded, _) = self.decode(warm_up_samples);
		decoded
			.iter()
			.zip(real_samples.iter())
			.map(|(actual, expected)| i128::from(*actual).abs_diff(i128::from(*expected)))
			.sum()
	}

	/// Encode the given sample block using the given filter. This returns all shift amounts.
	pub fn encode_with_filter(
		warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		filter: LPCFilter,
		flags: LoopEndFlags,
	) -> impl Iterator<Item = Self> {
		(-1 ..= 11).map(move |real_shift| {
			let (_, encoded) =
				Self::internal_encode_lpc(warm_up_samples, samples, filter.coefficient(), |_| real_shift);
			Self { header: Header { real_shift, filter, flags }, encoded_samples: encoded }
		})
	}

	/// Encode a block of samples exactly as specified; this is in contrast to most encoding functions which try to use
	/// the best encoding parameters.
	#[must_use]
	pub fn encode_exact(
		warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		filter: LPCFilter,
		flags: LoopEndFlags,
		real_shift: i8,
	) -> Self {
		let (_, encoded) = Self::internal_encode_lpc(warm_up_samples, samples, filter.coefficient(), |_| real_shift);
		Self { header: Header { real_shift, filter, flags }, encoded_samples: encoded }
	}

	/// Encode the given sample block using the most accurate filter possible. Instead of brute-forcing all shift
	/// amounts, estimate a good shift amount that doesn't account for the -1 shift or overflow "exploits".
	#[must_use]
	#[allow(clippy::missing_panics_doc)]
	pub fn encode_with_good_shift(
		warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		flags: LoopEndFlags,
	) -> Self {
		[
			Self::encode_with_filter_good_shift(warm_up_samples, samples, LPCFilter::Zero, flags),
			Self::encode_with_filter_good_shift(warm_up_samples, samples, LPCFilter::One, flags),
			Self::encode_with_filter_good_shift(warm_up_samples, samples, LPCFilter::Two, flags),
			Self::encode_with_filter_good_shift(warm_up_samples, samples, LPCFilter::Three, flags),
		]
		.into_iter()
		.min_by_key(|encoded| encoded.total_encode_error(warm_up_samples, &samples))
		.unwrap()
	}

	/// Encode a block of samples with the specified filter, but instead of brute-forcing all shift amounts, estimate a
	/// good shift amount that doesn't account for the -1 shift or overflow "exploits".
	#[must_use]
	pub fn encode_with_filter_good_shift(
		warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		filter: LPCFilter,
		flags: LoopEndFlags,
	) -> Self {
		let (real_shift, encoded) =
			Self::internal_encode_lpc(warm_up_samples, samples, filter.coefficient(), Self::necessary_shift_for);
		Self { header: Header { real_shift, filter, flags }, encoded_samples: encoded }
	}

	/// Encode a block of samples with filter 0 and estimated good shift amount.
	#[must_use]
	#[inline]
	pub fn encode_with_filter_0_good_shift(
		warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		flags: LoopEndFlags,
	) -> Self {
		Self::encode_with_filter_good_shift(warm_up_samples, samples, LPCFilter::Zero, flags)
	}

	/// Encode the given sample block using the given filter. This brute forces all shift amounts and returns the best.
	#[must_use]
	#[allow(clippy::missing_panics_doc)]
	pub fn encode_with_filter_best(
		warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		filter: LPCFilter,
		flags: LoopEndFlags,
	) -> Self {
		Self::encode_with_filter(warm_up_samples, samples, filter, flags)
			.min_by_key(|encoded| encoded.total_encode_error(warm_up_samples, &samples))
			.unwrap()
	}

	fn internal_encode_lpc(
		mut warm_up_samples: WarmUpSamples,
		samples: DecodedBlockSamples,
		filter_coefficients: FilterCoefficients,
		shift_function: impl Fn(&DecodedBlockSamples) -> i8,
	) -> (i8, EncodedBlockSamples) {
		// Let's first encode without concerning ourselves with the shift amount. That can be dealt with later.
		let mut unshifted_encoded_samples = [0; 16];
		for (unshifted_encoded, sample) in unshifted_encoded_samples.iter_mut().zip(samples.iter()) {
			// Dividing the sample by 2 reduces it to 15 bit, which is the actual bit depth of the decoder. Therefore,
			// in order for our encoding to be accurate, we have to reduce the bit depth here.
			*unshifted_encoded = (*sample / 2)
				// It is now known whether wrapping addition (or subtraction for encoding) is correct here.
				// The multiplication seems to saturate indeed, as it is implemented with a series of shifts.
				.wrapping_sub(filter_coefficients[0](warm_up_samples[0]))
				.wrapping_sub(filter_coefficients[1](warm_up_samples[1]));
			warm_up_samples[1] = warm_up_samples[0];
			warm_up_samples[0] = *sample / 2;
		}
		let mut encoded = [0; 16];
		let shift = shift_function(&unshifted_encoded_samples);
		for (encoded, sample) in encoded.iter_mut().zip(unshifted_encoded_samples.iter()) {
			// The shift needs to be inverted, as we're encoding here and the shift function is intended for decoding.
			*encoded = Self::perform_shift_with(-shift, *sample) as u8;
		}

		(shift, encoded)
	}

	/// Computes the necessary shift in order to get all of the given samples in range for 4-bit signed integers.
	///
	/// # Implementation details
	/// *This section tries to explain some of the reasoning behind the innocent-looking logic below.*
	///
	/// We need to realize that the necessary shift depends on the necessary bits for a number. As we only
	/// need to deal with bit counts above 4, we can subtract 4 from that and clamp at 0? However, the output must be
	/// 4-bit signed, so the highest bit in the nybble is still a sign bit! Therefore, the shift must be one more
	/// than previously expected to retain the sign bit, and we only want to subtract 3.
	///
	/// This function is not used anymore in the current encoder implementation. While theoretically correct, abusing
	/// wrapping behavior of the DSP decoder and allowing the use of the -1 shift can sometimes lead to better shifts in
	/// combination with specific filters. This function is only ideal if we assume no wrapping happens, which of course
	/// isn't the case.
	fn necessary_shift_for(samples: &DecodedBlockSamples) -> i8 {
		samples
			.iter()
			.map(|x| match x {
				i16::MIN ..= -1 => i16::BITS - x.leading_ones(),
				0 => 0,
				1 .. => i16::BITS - x.leading_zeros(),
			})
			.max()
			.unwrap_or(0)
			.saturating_sub(3) as i8
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

	/// Executes the shift in this header on the given sample, taking care to shift right by 1 if the shift amount is
	/// -1.
	#[inline]
	#[must_use]
	pub fn perform_shift(&self, sample: DecodedSample) -> DecodedSample {
		Self::perform_shift_with(self.header.real_shift, sample)
	}

	/// Executes the given shift on the given sample, taking care to shift right by 1 if the shift amount is -1.
	#[inline]
	#[must_use]
	pub fn perform_shift_with(shift: i8, sample: DecodedSample) -> DecodedSample {
		match shift {
			0 => Some(sample),
			1.. => sample.checked_shl(u32::from(shift.unsigned_abs())),
			_ => sample.checked_shr(u32::from(shift.unsigned_abs())),
		}
		// FIXME: unwrap_or suddenly became non-const in 1.71 nightly, make function const again once that is fixed.
		.unwrap_or(if sample > 0 { DecodedSample::MAX } else { DecodedSample::MIN })
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

		let shifted_encoded =
			self.encoded_samples.map(|encoded| self.perform_shift(i16::from(((encoded as i8) << 4) >> 4)));
		for (decoded, encoded) in decoded_samples.iter_mut().zip(shifted_encoded) {
			let decimal_decoded = encoded
				.wrapping_add(filter_coefficients[0](warm_up_samples[0]))
				.wrapping_add(filter_coefficients[1](warm_up_samples[1]));
			*decoded = simulate_hardware_glitches(decimal_decoded);
			// Shift last samples through the buffer
			warm_up_samples[1] = warm_up_samples[0];
			warm_up_samples[0] = *decoded;

			// After doing its special kind of restricting to 15 bits (hardware stuff), we need to extend the sample
			// back to 16 bits.
			*decoded = decoded.wrapping_mul(2);
		}
		(decoded_samples, warm_up_samples)
	}
}

/// Decoded BRR samples must be within the range -0x3ffa to 0x3ff8, otherwise hardware glitches occur.
/// The exact hardware glitch behavior can be seen in the implementation of this function, but additionally the Gaussian
/// interpolation that runs for pitch-shifting can cause overflows when the values is outside the above range but not
/// modified by this function. Also, the initial decode steps will clamp around the signed 16-bit boundaries.
fn simulate_hardware_glitches(n: i16) -> i16 {
	// Remap 0x4000 - 0x7fff to -0x4000 - -0x0001
	if (0x4000 ..= 0x7fff).contains(&n) {
		// We can't subtract 0x4000*2 as that overflows 16 bits!
		n - 0x4000 - 0x4000
	// Remap -0x8000 - -0x4001 to 0x0 - -0x3FFF
	} else if (-0x8000 ..= -0x4001).contains(&n) {
		n + 0x4000 + 0x4000
	} else {
		n
	}
}

impl From<[u8; 9]> for Block {
	fn from(data: [u8; 9]) -> Self {
		Self {
			header:          data[0].into(),
			encoded_samples: split_bytes_into_nybbles(data[1 .. 9].try_into().unwrap()),
		}
	}
}

impl From<Block> for [u8; 9] {
	fn from(block: Block) -> [u8; 9] {
		arrcat::concat_arrays![[block.header.into()], (merge_nybbles_into_bytes(block.encoded_samples)): [_; 8]]
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

fn merge_nybbles_into_bytes(nybbles: EncodedBlockSamples) -> [u8; 8] {
	let mut bytes = [0; 8];
	for (index, [high_nybble, low_nybble]) in nybbles.as_chunks::<2>().0.iter().enumerate() {
		bytes[index] = ((high_nybble << 4) & 0xf0) | (low_nybble & 0x0f);
	}
	bytes
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
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
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

impl From<Header> for u8 {
	fn from(header: Header) -> u8 {
		((((header.real_shift + 1) as u8) << 4) & 0xf0)
			| (((header.filter as u8) << 2) & 0b1100)
			| ((header.flags as u8) & 0b11)
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
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum LPCFilter {
	/// Filter 0, verbatim samples.
	#[default]
	Zero = 0,
	/// Filter 1, differential coding.
	One = 1,
	/// Filter 2, something extremely close to polynomial order 2 predictive coding (which would be `s[t] + 2* s[t-1] -
	/// s[t-2])`.
	Two = 2,
	/// Filter 3, again very close to polynomial order 2 predictive coding but more off than filter 2. I can't make
	/// sense of this one.
	Three = 3,
}

impl LPCFilter {
	/// Return the coefficients used by this filter. The first coefficient is for the last sample, while the second
	/// coefficient is for the second-to-last sample.
	#[must_use]
	pub fn coefficient(self) -> FilterCoefficients {
		match self {
			Self::Zero => [Self::coefficient_0, Self::coefficient_0],
			Self::One => [Self::coefficient_15_16, Self::coefficient_0],
			Self::Two => [Self::coefficient_61_32, Self::coefficient_negative_15_16],
			Self::Three => [Self::coefficient_115_64, Self::coefficient_negative_13_16],
		}
	}

	/// Return all possible filters.
	#[must_use]
	pub const fn all_filters() -> [Self; 4] {
		[Self::Zero, Self::One, Self::Two, Self::Three]
	}

	// Coefficient functions (https://problemkaputt.de/fullsnes.htm#snesapudspbrrsamples)

	/// * 0
	#[inline]
	#[must_use]
	pub const fn coefficient_0(_: i16) -> i16 {
		0
	}

	/// * 15/16
	#[inline]
	#[must_use]
	pub const fn coefficient_15_16(i: i16) -> i16 {
		let i = i as i32;
		(i + ((-i) >> 4)) as i16
	}

	/// * -15/16
	#[inline]
	#[must_use]
	pub const fn coefficient_negative_15_16(i: i16) -> i16 {
		-Self::coefficient_15_16(i)
	}

	/// * 61/32
	#[inline]
	#[must_use]
	pub const fn coefficient_61_32(i: i16) -> i16 {
		let i = i as i32;
		(i * 2 + ((-i * 3) >> 5)) as i16
	}

	/// * 115/64
	#[inline]
	#[must_use]
	pub const fn coefficient_115_64(i: i16) -> i16 {
		let i = i as i32;
		(i * 2 + ((-i * 13) >> 6)) as i16
	}

	/// * -13/16
	#[inline]
	#[must_use]
	pub const fn coefficient_negative_13_16(i: i16) -> i16 {
		let i = i as i32;
		(-i + ((i * 3) >> 4)) as i16
	}
}

impl std::fmt::Display for LPCFilter {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", *self as u8)
	}
}

/// Loop and end flags used in the BRR block header to determine sample end and looping.
#[derive(Clone, Copy, Debug, Default, Eq, FromPrimitive)]
#[repr(u8)]
pub enum LoopEndFlags {
	/// Nothing special happens, this is a normal sample.
	#[default]
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
		Self::from_u8(if is_loop { 0b10 } else { 0 } | u8::from(is_end)).unwrap()
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
	/// in the sample table. Note that this raw loop flag only has effect if [`Self::is_end`] is set.
	#[must_use]
	pub const fn is_raw_loop(self) -> bool {
		self as u8 & 0b10 > 0
	}

	/// Returns whether the playback will *actually* loop after this sample, i.e. jump to the loop point. This is
	/// different from the raw loop flag ([`Self::is_raw_loop`]), which has no effect without the end flag.
	#[must_use]
	pub const fn will_loop_afterwards(self) -> bool {
		self.eq_(Self::Loop)
	}
}

impl std::fmt::Display for LoopEndFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.is_raw_loop() {
			f.write_str("loop")?;
		}
		// end & loop: print separator
		if self.will_loop_afterwards() {
			f.write_str(", ")?;
		}
		if self.is_end() {
			f.write_str("end")?;
		}
		Ok(())
	}
}
