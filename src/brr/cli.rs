//! Interactive BRR test program.
#![deny(clippy::all, clippy::nursery, clippy::pedantic)]

use std::convert::TryInto;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

use ::wav::WAV_FORMAT_PCM;
use clap::{Parser, Subcommand};
#[allow(clippy::wildcard_imports)]
use spcasm::brr::*;

#[derive(Parser)]
#[clap(about = "Bit Rate Reduced (BRR) / SNES ADPCM tools", long_about=None)]
struct Arguments {
	#[clap(subcommand)]
	command: Command,
	#[clap(long, short, help = "Print detailed information even for non-interactive commands")]
	verbose: bool,
}

#[derive(Subcommand)]
enum Command {
	#[clap(
		about = "Encode a single block of samples",
		long_about = "Encode a single block of samples. Displays various information about the encoding process, \
		              including how accurately the data compresses under various filter modes. This command is \
		              intended for interactive experimenting with BRR encoding."
	)]
	EncodeBlock {
		#[clap(
			value_parser = from_lenient_i16,
			help = "The samples to encode.",
			long_help = "The samples to encode, 16-bit signed integers. There must be exactly 16 samples to encode."
		)]
		samples: Vec<DecodedSample>,
		#[clap(
			value_parser = from_lenient_i16,
			short,
			long,
			help = "Override the previous samples to use for encoding",
			long_help = "Override the previous samples to use for encoding. There must be exactly two of these, \
			             otherwise the previous samples are assumed to be zero."
		)]
		warm_up: Option<Vec<DecodedSample>>,
	},

	#[clap(
		about = "Decode a single block of samples",
		long_about = "Decode a single block of samples. Displays various information about the decoding process. This \
		              command is intended for interactive experimenting with BRR decoding."
	)]
	DecodeBlock {
		#[clap(
			value_parser,
			help = "The BRR-encoded block to decode",
			long_help = "The BRR-encoded block to decode, given in its individual bytes. There must be exactly nine \
			             bytes."
		)]
		block:   Vec<u8>,
		#[clap(
			short,
			long,
			value_parser = from_lenient_i16,
			help = "Set the previous two decoded samples",
			long_help = "Set the previous two decoded samples, 16-bit signed integers. There must be exactly two of \
			             these, otherwise the previous samples are assumed to be zero."
		)]
		warm_up: Option<Vec<DecodedSample>>,
	},

	#[clap(about = "Encode a WAV file into BRR.")]
	Encode {
		#[clap(
			value_parser,
			help = "The WAV file to encode",
			long_help = "The WAV file to encode. Only uncompressed WAV (integer or float) is supported. Sample rate \
			             is not converted, so in order for audio to not be pitch-shifted, the input has to be at \
			             32kHz, matching the SNES DSP sample rate."
		)]
		input:  PathBuf,
		#[clap(
			value_parser,
			help = "Output BRR file to write",
			long_help = "Output BRR file to write. By default, a file with the same name but a `.brr` extension is \
			             used as output."
		)]
		output: Option<PathBuf>,
	},

	#[clap(about = "Decode a BRR file into a WAV.")]
	Decode {
		#[clap(
			value_parser,
			help = "The BRR file to decode",
			long_help = "The BRR file to decode. Only raw BRR files are supported right now."
		)]
		input:  PathBuf,
		#[clap(
			value_parser,
			help = "Output WAV file to write",
			long_help = "Output WAV file to write. The format is always mono 16-bit signed integer with a sample rate \
			             of 32kHz, matching the SNES DSP."
		)]
		output: Option<PathBuf>,
	},
}

/// Parse an i16 while intentionally allowing wrapping and hex numbers.
#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)] // this is very intentional!
fn from_lenient_i16(string: &str) -> Result<i16, String> {
	string
		.parse::<i32>()
		.map(|int| int as i16)
		.or_else(|err| {
			string.strip_prefix("0x").map_or_else(
				|| {
					string.strip_prefix("-0x").map_or_else(
						|| Err(err),
						|hex_string| u16::from_str_radix(hex_string, 16).map(|int| -(int as i16)),
					)
				},
				|hex_string| u16::from_str_radix(hex_string, 16).map(|int| int as i16),
			)
		})
		.map_err(|err| err.to_string())
}

#[allow(clippy::too_many_lines)]
fn main() {
	let arguments = Arguments::parse();

	match arguments.command {
		Command::EncodeBlock { samples, warm_up } => {
			let warm_up: [i16; 2] = warm_up.unwrap_or_else(|| vec![0, 0]).try_into().unwrap_or_else(|_| {
				eprintln!("error: you must provide exactly 2 warm-up samples");
				std::process::exit(1);
			});
			let samples: DecodedBlockSamples = samples.try_into().unwrap_or_else(|_| {
				eprintln!("error: you must provide exactly 16 unencoded samples");
				std::process::exit(1);
			});
			println!("Encoding {:?} as BRR block. Warm-up: {:?}", samples, warm_up);
			for filter in LPCFilter::all_filters() {
				println!("filter {}:", filter);
				for shift in -1 ..= 11 {
					let block = Block::encode_exact(warm_up, samples, filter, LoopEndFlags::Nothing, shift);
					println!(
						"  shift {:2}:\n    encode to: {:?}\n    decoded: {:?}\n    error: {:10}",
						block.header.real_shift,
						block.encoded_samples,
						block.decode(warm_up).0,
						block.total_encode_error(warm_up, &samples)
					);
				}
				println!(
					"  optimal shift for this filter: {:2}",
					Block::encode_with_filter_best(warm_up, samples, filter, LoopEndFlags::Nothing).header.real_shift
				);
			}
			let actual_encoded = Block::encode(warm_up, samples, LoopEndFlags::Nothing);
			println!(
				"optimal encoding: filter {} shift {}",
				actual_encoded.header.filter, actual_encoded.header.real_shift
			);
		},
		Command::DecodeBlock { block, warm_up } => {
			let warm_up = warm_up.unwrap_or_else(|| vec![0, 0]).try_into().unwrap_or_else(|_| {
				eprintln!("error: you must provide exactly 2 warm-up samples");
				std::process::exit(1);
			});
			let raw_block: [u8; 9] = block.try_into().unwrap_or_else(|_| {
				eprintln!("error: you must provide exactly 16 unencoded samples");
				std::process::exit(1);
			});
			println!("Decoding BRR block {:X?}:", raw_block);
			let block = Block::from(raw_block);
			println!(
				"\tflags:           {:>11}(raw: {:02b})\n\tfilter:          {}\n\tshift:           {} (raw: \
				 {:04b})\n\tencoded samples: {}",
				block.header.flags,
				block.header.flags as u8,
				block.header.filter as u8,
				block.header.real_shift,
				block.header.real_shift + 1,
				block
					.encoded_samples
					.iter()
					.map(|sample| String::from(format!("{:01X}", sample).chars().last().unwrap()))
					.collect::<Vec<_>>()
					.join(" "),
			);

			let (decoded, _) = block.decode(warm_up);
			println!("Decoded samples: {:?}", decoded);
		},
		Command::Encode { input, output } => {
			let output = output.unwrap_or_else(|| input.with_extension("brr"));
			let mut samples = File::open(input)
				.map_err(|err| err.to_string())
				.and_then(wav::read_wav_for_brr)
				.unwrap_or_else(|error| {
					eprintln!("error: {}", error);
					std::process::exit(1);
				});

			let start = std::time::Instant::now();
			let encoded = encode_to_brr(&mut samples, false);
			let duration = start.elapsed();
			if arguments.verbose {
				println!(
					"Encoded {} samples to {} bytes BRR in {} μs.",
					samples.len(),
					encoded.len(),
					duration.as_micros(),
				);
			}
			let mut output_file =
				File::options().write(true).create(true).append(false).open(output).unwrap_or_else(|error| {
					eprintln!("error opening output: {}", error);
					std::process::exit(1);
				});
			output_file.write_all(&encoded).unwrap_or_else(|error| {
				eprintln!("error while writing output: {}", error);
				std::process::exit(1);
			});
		},
		Command::Decode { input, output } => {
			let output = output.unwrap_or_else(|| input.with_extension("wav"));
			let mut encoded = Vec::new();
			let samples = File::open(input)
				.and_then(|mut input_file| input_file.read_to_end(&mut encoded))
				.map_err(|err| err.to_string())
				.and_then(|_| decode_from_brr(&encoded))
				.unwrap_or_else(|error| {
					eprintln!("error: {}", error);
					std::process::exit(1);
				});
			let header = ::wav::Header::new(WAV_FORMAT_PCM, 1, 32_000, 16);

			let mut output_file =
				File::options().write(true).create(true).append(false).open(output).unwrap_or_else(|error| {
					eprintln!("error opening output: {}", error);
					std::process::exit(1);
				});

			::wav::write(header, &samples.into(), &mut output_file).unwrap_or_else(|error| {
				eprintln!("error writing output: {}", error);
				std::process::exit(1);
			});
		},
	}
}
