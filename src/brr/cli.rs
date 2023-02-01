//! Interactive BRR test program.
#![deny(missing_docs, unused, clippy::all, clippy::pedantic, clippy::nursery)]

use std::convert::TryInto;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

use ::wav::WAV_FORMAT_PCM;
use clap::{Parser, Subcommand};
use num_traits::cast::FromPrimitive;
#[allow(clippy::wildcard_imports)]
use spcasm::brr::*;

#[derive(Parser)]
#[command(about = "Bit Rate Reduced (BRR) / SNES ADPCM tools", long_about = None, version, name = "brr")]
struct Arguments {
	#[command(subcommand)]
	command: Command,
	#[arg(long, short, help = "Print detailed information even for non-interactive commands")]
	verbose: bool,
}

#[derive(Subcommand)]
enum Command {
	#[command(
		about = "Encode a single block of samples",
		long_about = "Encode a single block of samples. Displays various information about the encoding process, \
		              including how accurately the data compresses under various filter modes. This command is \
		              intended for interactive experimenting with BRR encoding."
	)]
	EncodeBlock {
		#[arg(
			value_parser = from_lenient_i16,
			num_args = 16,
			help = "The samples to encode.",
			long_help = "The samples to encode, 16-bit signed integers. There must be exactly 16 samples to encode."
		)]
		samples: Vec<DecodedSample>,
		#[arg(
			short,
			long,
			required = false,
			num_args = 2,
			value_parser = from_lenient_i16,
			help = "Override the previous samples to use for encoding",
			long_help = "Override the previous samples to use for encoding. There must be exactly two of these, \
			             otherwise the previous samples are assumed to be zero."
		)]
		warm_up: Option<Vec<DecodedSample>>,
	},

	#[command(
		about = "Decode a single block of samples",
		long_about = "Decode a single block of samples. Displays various information about the decoding process. This \
		              command is intended for interactive experimenting with BRR decoding."
	)]
	DecodeBlock {
		#[arg(
			num_args = 9,
			help = "The BRR-encoded block to decode",
			long_help = "The BRR-encoded block to decode, given in its individual bytes. There must be exactly nine \
			             bytes."
		)]
		block:   Vec<u8>,
		#[arg(
			short,
			long,
			required = false,
			num_args = 2,
			value_parser = from_lenient_i16,
			help = "Set the previous two decoded samples",
			long_help = "Set the previous two decoded samples, 16-bit signed integers. There must be exactly two of \
			             these, otherwise the previous samples are assumed to be zero."
		)]
		warm_up: Option<Vec<DecodedSample>>,
	},

	#[command(about = "Encode a WAV file into a BRR file")]
	Encode {
		#[arg(
			help = "The WAV file to encode",
			long_help = "The WAV file to encode. Only uncompressed WAV (integer or float) is supported. Sample rate \
			             is not converted, so in order for audio to not be pitch-shifted, the input has to be at \
			             32kHz, matching the SNES DSP sample rate."
		)]
		input:       PathBuf,
		#[arg(
			help = "Output BRR file to write",
			long_help = "Output BRR file to write. By default, a file with the same name but a `.brr` extension is \
			             used as output."
		)]
		output:      Option<PathBuf>,
		#[arg(
			value_parser = |string: &str| string.parse().map_err(|err: std::num::ParseIntError| err.to_string()).and_then(|int| CompressionLevel::from_u8(int).ok_or("compression level out of range".to_string())),
			default_value = "2",
			long,
			short,
			help = "Compression level to use",
			long_help = "Compression level to use; higher levels mean better audio fidelity. 0: Only use filter 0, 1: Use all filters with non-wrapping optimal shift, 2: Use all filters with optimal shift."
		)]
		compression: CompressionLevel,
		#[arg(
			long,
			short,
			required = false,
			help = "Boost treble for accurate audio reproduction",
			long_help = "The hardware Gaussian filter of the S-SMP intended for better pitch shifting always has the \
			             effect of a low-pass filter on the input sample. To counteract this, brr can apply an \
			             inverse treble boost filter to the sample before encoding, which will make the output sound \
			             more accurate to the original sample."
		)]
		filter:      bool,
	},

	#[command(about = "Decode a BRR file into a WAV file")]
	Decode {
		#[arg(
			help = "The BRR file to decode",
			long_help = "The BRR file to decode. Only raw BRR files are supported right now."
		)]
		input:  PathBuf,
		#[arg(
			help = "Output WAV file to write",
			long_help = "Output WAV file to write. The format is always mono 16-bit signed integer with a sample rate \
			             of 32kHz, matching the SNES DSP."
		)]
		output: Option<PathBuf>,
		#[arg(
			long,
			short,
			required = false,
			help = "Emulate hardware filtering",
			long_help = "Emulate the hardware Gaussian filter. This filter is applied by S-SMP sample playback \
			             hardware after decoding for a good-enough pitch shift, but it applies even if the pitch is \
			             not shifted. The emulation helps recover audio data the way it would have been heard on \
			             original hardware."
		)]
		filter: bool,
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
		Command::Encode { input, output, compression, filter } => {
			let output = output.unwrap_or_else(|| input.with_extension("brr"));
			let mut samples = File::open(input)
				.map_err(|err| err.to_string())
				.and_then(wav::read_wav_for_brr)
				.unwrap_or_else(|error| {
					eprintln!("error: {}", error);
					std::process::exit(1);
				});

			let start = std::time::Instant::now();
			if filter {
				samples = dsp::apply_treble_boost_filter(&samples);
			}
			let encoded = encode_to_brr(&mut samples, false, compression);
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
		Command::Decode { input, output, filter } => {
			let output = output.unwrap_or_else(|| input.with_extension("wav"));
			let mut encoded = Vec::new();
			let mut samples = File::open(input)
				.and_then(|mut input_file| input_file.read_to_end(&mut encoded))
				.map_err(|err| err.to_string())
				.and_then(|_| decode_from_brr(&encoded))
				.unwrap_or_else(|error| {
					eprintln!("error: {}", error);
					std::process::exit(1);
				});

			if filter {
				dsp::apply_hardware_gauss_filter(&mut samples);
			}

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
