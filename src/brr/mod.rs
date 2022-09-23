//! BRR (Bit Rate Reduced) a.k.a. SNES ADPCM sample format parsing, writing and reading.
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(
	clippy::cast_possible_wrap,
	clippy::cast_possible_truncation,
	clippy::cast_sign_loss,
	clippy::fallible_impl_from,
	clippy::use_self
)]

use std::convert::TryInto;

use num_derive::FromPrimitive;
use num_traits::{CheckedShr, FromPrimitive};

#[cfg(test)] mod test;

pub mod wav;

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

/// Encode the given 16-bit samples as BRR samples. The data may be padded via repetition to fit a multiple of 16; if
/// you don't want this to happen, provide a multiple of 16 samples.
///
/// TODO: We don't know about the loop point, so we can't set that block to use filter 0.
#[must_use]
#[allow(clippy::module_name_repetitions)]
pub fn encode_to_brr(samples: &Vec<DecodedSample>, is_loop: bool) -> Vec<u8> {
	if samples.is_empty() {
		return Vec::new();
	}
	let (sample_chunks, maybe_last_chunk) = samples.as_chunks::<16>();

	if sample_chunks.is_empty() {
		// Just process the remainder chunk as the first and last chunk.
		let single_chunk = fill_slice_to_16(maybe_last_chunk);
		let encoded = <[u8; 9]>::from(Block::encode_with_filter(
			[0, 0],
			single_chunk,
			LPCFilter::Zero,
			LoopEndFlags::new(true, is_loop),
		));
		return encoded.to_vec();
	}
	let mut sample_chunks = sample_chunks.to_vec();
	let first_chunk = sample_chunks.remove(0);

	let last_chunk =
		if maybe_last_chunk.is_empty() { sample_chunks.pop() } else { Some(fill_slice_to_16(maybe_last_chunk)) };

	// Determine whether the first chunk is also the last; i.e. there are exactly 16 samples.
	let first_block_is_end = last_chunk.is_none();

	// This should allocate enough so the vector never needs to reallocate.
	let mut result = Vec::with_capacity(samples.len() / 2 + sample_chunks.len() + 9);

	// The first chunk must be encoded with filter 0 to prevent glitches.
	let first_block_data = Block::encode_with_filter(
		[0, 0],
		first_chunk,
		LPCFilter::Zero,
		LoopEndFlags::new(first_block_is_end, first_block_is_end && is_loop),
	);
	result.extend_from_slice(&<[u8; 9]>::from(first_block_data));
	let mut warm_up: WarmUpSamples = [first_chunk[first_chunk.len() - 1], first_chunk[first_chunk.len() - 2]];
	for chunk in sample_chunks {
		let block_data = Block::encode(warm_up, chunk, LoopEndFlags::Nothing);
		result.extend_from_slice(&<[u8; 9]>::from(block_data));
		warm_up = [chunk[chunk.len() - 1], chunk[chunk.len() - 2]];
	}

	if let Some(last_chunk) = last_chunk {
		let last_block_data = Block::encode(warm_up, last_chunk, LoopEndFlags::new(true, is_loop));
		result.extend_from_slice(&<[u8; 9]>::from(last_block_data));
	}

	result
}

/// Decode samples from a stream of BRR blocks. Note that this discards loop data and any potential end point is
/// disregarded; this function always decodes all the input data.
///
/// # Errors
/// All possible header bytes are valid on some level, so no errors are thrown because of this. If the encoded data does
/// not line up with a BRR block however, an error is returned.
#[allow(clippy::module_name_repetitions)]
pub fn decode_from_brr(encoded: &[u8]) -> Result<Vec<DecodedSample>, String> {
	let (blocks, remainder) = encoded.as_chunks();
	if !remainder.is_empty() {
		return Err(format!("Cut off BRR block (size {}) at the end of the stream", remainder.len()));
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

fn fill_slice_to_16(slice: &[i16]) -> DecodedBlockSamples {
	let mut result_chunk = [0; 16];
	for (i, entry) in result_chunk.iter_mut().enumerate() {
		*entry = *slice.get(i).or_else(|| slice.last()).unwrap();
	}
	result_chunk
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
		// Let's first encode without concerning ourselves with the shift amount. That can be dealt with later.
		let mut unshifted_encoded_samples = [0; 16];
		for (unshifted_encoded, sample) in unshifted_encoded_samples.iter_mut().zip(samples.iter()) {
			*unshifted_encoded = fixed(*sample)
				.wrapping_sub(filter_coefficients[0].wrapping_mul(fixed(warm_up_samples[0])))
				.ceil()
				.wrapping_sub(filter_coefficients[1].wrapping_mul(fixed(warm_up_samples[1])))
				.ceil()
				.to_num();
			warm_up_samples[1] = warm_up_samples[0];
			warm_up_samples[0] = *sample;
		}
		let necessary_shift = Self::necessary_shift_for(&unshifted_encoded_samples);
		let mut encoded = [0; 16];
		for (encoded, sample) in encoded.iter_mut().zip(unshifted_encoded_samples.iter()) {
			*encoded = sample.checked_shr(necessary_shift).unwrap_or(0) as u8;
		}

		(encoded, necessary_shift as i8)
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
	fn necessary_shift_for(samples: &DecodedBlockSamples) -> u32 {
		samples
			.iter()
			.map(|x| match x {
				i16::MIN ..= -1 => i16::BITS - x.leading_ones(),
				0 => 0,
				1 .. => i16::BITS - x.leading_zeros(),
			})
			.max()
			.unwrap_or(0)
			.saturating_sub(3)
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
		let shift_function = |encoded: i16| {
			if self.header.real_shift > 0 {
				encoded.checked_shl(self.header.real_shift.unsigned_abs().into()).unwrap_or(0)
			} else {
				encoded.checked_shr(self.header.real_shift.unsigned_abs().into()).unwrap_or(0)
			}
		};
		let shifted_encoded =
			self.encoded_samples.map(|encoded| shift_function(i16::from(((encoded as i8) << 4) >> 4)));
		for (decoded, encoded) in decoded_samples.iter_mut().zip(shifted_encoded) {
			// TODO: Check whether overflowing (i.e. wrap-around) arithmetic is correct here.
			// The convoluted cast ensures we retain the nybble sign bit.
			let decimal_decoded = fixed(encoded)
				.wrapping_add(filter_coefficients[0].wrapping_mul(fixed(warm_up_samples[0])))
				.floor()
				.wrapping_add(filter_coefficients[1].wrapping_mul(fixed(warm_up_samples[1])))
				.floor();
			*decoded = decimal_decoded.to_num();
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
