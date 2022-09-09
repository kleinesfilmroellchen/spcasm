//! BRR format tests.
//!
//! In the test names, we use "round trip" as a term for encode -> decode -> encode (or the other way around) and
//! checking that the data survives.
#![allow(overflowing_literals)]

use super::*;

const zero_warmup: FilterCoefficients = [FilterCoefficient::ZERO, FilterCoefficient::ZERO];

#[test]
fn no_shift_filter_0_roundtrip() {
	const data: DecodedBlockSamples = [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2];
	let (encoded, shift) = Block::internal_encode_lpc([0, 0], data, LPCFilter::Zero.coefficient());
	assert_eq!(shift, 0);
	let block =
		Block::new(Header { real_shift: shift, filter: LPCFilter::Zero, flags: LoopEndFlags::Nothing }, encoded);
	let (decoded, _) = block.internal_decode_lpc(zero_warmup, LPCFilter::Zero.coefficient());
	assert_eq!(data, decoded);
}

#[test]
fn shift_filter_0_roundtrip() {
	const data: DecodedBlockSamples = [64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64];
	let (encoded, shift) = Block::internal_encode_lpc([0, 0], data, LPCFilter::Zero.coefficient());
	assert_eq!(shift, 4);
	let block =
		Block::new(Header { real_shift: shift, filter: LPCFilter::Zero, flags: LoopEndFlags::Nothing }, encoded);
	let (decoded, _) = block.internal_decode_lpc(zero_warmup, LPCFilter::Zero.coefficient());
	assert_eq!(data, decoded);
}

#[test]
fn negative_1_filter_0_roundtrip() {
	const data: DecodedBlockSamples = [1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1];
	let (encoded, shift) = Block::internal_encode_lpc([0, 0], data, LPCFilter::Zero.coefficient());
	assert_eq!(shift, 0);
	let block =
		Block::new(Header { real_shift: shift, filter: LPCFilter::Zero, flags: LoopEndFlags::Nothing }, encoded);
	let (decoded, _) = block.internal_decode_lpc(zero_warmup, LPCFilter::Zero.coefficient());
	assert_eq!(data, decoded);
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

#[test]
fn full_decode_filter_0() {
	// https://youtu.be/bgh5_gxT2eg?t=1230
	const data: [u8; 9] = [0x90, 0x00, 0x01, 0x64, 0xae, 0x76, 0x46, 0x42, 0x3e]; //, 0x8c, 0xa0, 0x07, 0x77, 0x55, 0xf9,
																			  //, 0xb8, 0x75, 0x64];
	let block = Block::from(data);
	assert_eq!(block.header.real_shift, 8);
	assert_eq!(block.header.filter, LPCFilter::Zero);
	assert_eq!(block.decode([0, 0]).0, [
		0, 0, 0, 0x100, 0x600, 0x400, -0x600, -0x200, 0x700, 0x600, 0x400, 0x600, 0x400, 0x200, 0x300, -0x200
	]);
}
