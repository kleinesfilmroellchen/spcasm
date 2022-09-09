//! BRR format tests.
//!
//! In the test names, we use "round trip" as a term for encode -> decode -> encode (or the other way around) and
//! checking that the data survives.
#![allow(overflowing_literals)]

use super::*;

#[test]
fn no_shift_filter_0_roundtrip() {
	const data: DecodedBlockSamples = [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2];
	let (encoded, shift) = Block::internal_encode_lpc([0, 0], data, LPCFilter::Zero.coefficient());
	assert_eq!(shift, 0);
	let block =
		Block::new(Header { real_shift: shift, filter: LPCFilter::Zero, flags: LoopEndFlags::Nothing }, encoded);
	let (decoded, _) = block.internal_decode_0([0, 0]);
	assert_eq!(data, decoded);
}

#[test]
fn shift_filter_0_roundtrip() {
	const data: DecodedBlockSamples = [64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64];
	let (encoded, shift) = Block::internal_encode_lpc([0, 0], data, LPCFilter::Zero.coefficient());
	assert_eq!(shift, 2);
	let block =
		Block::new(Header { real_shift: shift, filter: LPCFilter::Zero, flags: LoopEndFlags::Nothing }, encoded);
	let (decoded, _) = block.internal_decode_0([0, 0]);
	assert_eq!(data, decoded);
}

#[test]
fn negative_1_filter_0_roundtrip() {
	const data: DecodedBlockSamples = [1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1];
	let (encoded, shift) = Block::internal_encode_lpc([0, 0], data, LPCFilter::Zero.coefficient());
	assert_eq!(shift, 12);
	let block =
		Block::new(Header { real_shift: shift, filter: LPCFilter::Zero, flags: LoopEndFlags::Nothing }, encoded);
	let (decoded, _) = block.internal_decode_0([0, 0]);
	// This roundtrip actually loses data. Because we shift so much to the right, the lower three nibbles of the -1 get
	// discarded.
	for (index, sample) in decoded.iter().enumerate() {
		assert_eq!(*sample, match index % 2 {
			0 => 0,
			1 => 0xf000i16,
			_ => unreachable!(),
		});
	}
}

#[test]
fn header_decode() {
	const plain: u8 = 0b0001_00_00;
	const shift_negative_1: u8 = 0b0000_00_00;
	const max_shift: u8 = 0b1111_00_00;
	const r#loop: u8 = 0b0101_01_11;
	const filter3: u8 = 0b0001_11_01;
	const ignored: u8 = 0b0010_10_10;

	let plain_header = Header::from(plain);
	assert_eq!(plain_header.filter, LPCFilter::Zero);
	assert_eq!(plain_header.flags, LoopEndFlags::Nothing);
	assert_eq!(plain_header.real_shift, 0);

	assert_eq!(Header::from(shift_negative_1).real_shift, -1);
	assert_eq!(Header::from(max_shift).real_shift, 14);
	assert_eq!(Header::from(r#loop).flags, LoopEndFlags::Loop);
	assert_eq!(Header::from(filter3).filter, LPCFilter::Three);
	assert_eq!(Header::from(ignored).flags, LoopEndFlags::Ignored);
}
