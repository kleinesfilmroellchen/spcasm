//! BRR format tests.
//!
//! In the test names, we use "round trip" as a term for encode -> decode -> encode (or the other way around) and
//! checking that the data survives.
#![allow(overflowing_literals)]

extern crate test;
use test::Bencher;

use super::*;

const zero_warmup: WarmUpSamples = [0, 0];

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

const data_block_1: [u8; 9] = [0x90, 0x00, 0x01, 0x64, 0xae, 0x76, 0x46, 0x42, 0x3e];
const data_block_2: [u8; 9] = [0x8c, 0xa0, 0x07, 0x77, 0x55, 0xf9, 0xb8, 0x75, 0x64];
const decoded_block_1: [i16; 16] =
	[0, 0, 0, 0x100, 0x600, 0x400, -0x600, -0x200, 0x700, 0x600, 0x400, 0x600, 0x400, 0x200, 0x300, -0x200];
const decoded_block_2: [i16; 16] = [
	-0x908, -0xe9b, -0x12e9, -0x129e, -0xe97, -0x798, 0xb4, 0x9ee, 0x10c4, 0x128e, 0x1137, 0xbda, 0xace, 0xc48, 0x1049,
	0x1548,
];

#[bench]
fn full_decode(bencher: &mut Bencher) {
	// https://youtu.be/bgh5_gxT2eg?t=1230
	bencher.iter(|| {
		let block_1 = Block::from(data_block_1);
		assert_eq!(block_1.header.real_shift, 8);
		assert_eq!(block_1.header.filter, LPCFilter::Zero);
		let (actual_block_1_decoded, warm_up) = block_1.decode([0, 0]);
		assert_eq!(actual_block_1_decoded, decoded_block_1);
		let block_2 = Block::from(data_block_2);
		assert_eq!(block_2.header.real_shift, 7);
		assert_eq!(block_2.header.filter, LPCFilter::Three);
		assert_eq!(block_2.decode(warm_up).0, decoded_block_2);
	});
}

#[test]
fn multiple_roundtrips() {
	let block_1 = Block::from(data_block_1);
	let (_, warm_up) = block_1.decode([0, 0]);
	let block_2 = Block::from(data_block_2);
	let (block_2_decoded_once, _) = block_2.decode(warm_up);
	let mut previous_decode = block_2_decoded_once;
	for _ in 0 .. 15 {
		let next_block =
			Block::encode_with_filter(warm_up, previous_decode, block_2.header.filter, block_2.header.flags);
		(previous_decode, _) = next_block.decode(warm_up);
		assert_eq!(previous_decode, decoded_block_2);
	}
}

#[bench]
fn packing(bencher: &mut Bencher) {
	let block = Block::from(data_block_1);
	bencher.iter(|| assert_eq!(<[u8; 9]>::from(block), data_block_1));
}

#[test]
fn encode_block_best() {
	let block_1 = Block::from(data_block_1);
	let (_, warm_up) = block_1.decode([0, 0]);
	let block_2 = Block::from(data_block_2);
	let next_block = Block::encode(warm_up, decoded_block_2, block_2.header.flags);
	let (redecoded, _) = next_block.decode(warm_up);
	assert_eq!(redecoded, decoded_block_2);
}

#[bench]
fn microbench_encode_block_best(bencher: &mut Bencher) {
	let block_1 = Block::from(data_block_1);
	let (_, warm_up) = block_1.decode([0, 0]);
	let block_2 = Block::from(data_block_2);
	bencher.iter(|| Block::encode(warm_up, decoded_block_2, block_2.header.flags));
}

#[bench]
fn microbench_encode_block_filter_3(bencher: &mut Bencher) {
	let block_1 = Block::from(data_block_1);
	let (_, warm_up) = block_1.decode([0, 0]);
	let block_2 = Block::from(data_block_2);
	bencher.iter(|| Block::encode_with_filter(warm_up, decoded_block_2, block_2.header.filter, block_2.header.flags));
}

#[bench]
fn microbench_deconstruct_block(bencher: &mut Bencher) {
	bencher.iter(|| Block::from(test::black_box(data_block_1)));
}

#[bench]
fn microbench_decode_block_filter_0(bencher: &mut Bencher) {
	let block_1 = test::black_box(Block::from(data_block_1));
	bencher.iter(|| block_1.decode([0, 0]));
}

#[bench]
fn microbench_decode_block_filter_3(bencher: &mut Bencher) {
	let block_1 = Block::from(data_block_1);
	let (_, warm_up) = block_1.decode([0, 0]);
	let block_2 = test::black_box(Block::from(data_block_2));
	bencher.iter(|| block_2.decode(warm_up));
}

#[bench]
fn short_sample_encode(bencher: &mut Bencher) {
	use ::wav::read as wav_read;

	let (_, data) = wav_read(&mut std::fs::File::open("src/brr/yoshi.wav").unwrap()).unwrap();
	let data = data.as_sixteen().expect("must be signed 16-bit WAV");
	bencher.iter(|| encode_to_brr(data, false));
}

#[cfg(feature = "expensive_tests")]
#[bench]
fn extremely_long_encode(bencher: &mut Bencher) {
	use ::wav::read as wav_read;

	let (_, data) = wav_read(&mut std::fs::File::open("src/brr/song.wav").unwrap()).unwrap();
	let data = data.as_sixteen().expect("must be signed 16-bit WAV");
	let now = std::time::Instant::now();
	bencher.iter(|| encode_to_brr(data, false));
	let diff = now.elapsed();
	println!("took: {}ns ({} samples/s)", diff.as_nanos(), data.len() as f64 / diff.as_secs_f64());
}
