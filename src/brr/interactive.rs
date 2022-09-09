//! Interactive BRR test program.

use std::convert::TryInto;

use clap::{Parser, Subcommand};
use spcasm::brr::*;

#[derive(Parser)]
#[clap(about = "Interactive BRR encode/decode testing and experimenting", long_about=None)]
struct Arguments {
	#[clap(subcommand)]
	mode: ExecMode,
}

#[derive(Subcommand)]
enum ExecMode {
	// FIXME: allow for the specification of warm-up samples
	Encode {
		#[clap(value_parser)]
		samples: Vec<DecodedSample>,
	},
}

fn main() {
	let arguments = Arguments::parse();

	match arguments.mode {
		ExecMode::Encode { samples } => {
			let samples: DecodedBlockSamples = samples.try_into().unwrap_or_else(|_| {
				eprintln!("error: you must provide exactly 16 unencoded samples");
				std::process::exit(1);
			});
			println!("Encoding {:?} as BRR block.", samples);
			for filter in LPCFilter::all_filters() {
				let block = Block::encode_with_filter([0, 0], samples, filter, LoopEndFlags::Nothing);
				println!(
					"{}: encode to: {:?}\n\tshift: {:2}\n\tdecoded: {:?}\n\terror: {:10}",
					filter,
					block.encoded_samples,
					block.header.real_shift,
					block.decode([0, 0]).0,
					block.total_encode_error([0, 0], &samples)
				)
			}
			let actual_encoded = Block::encode([0, 0], samples, LoopEndFlags::Nothing);
			println!("optimal encoding: filter {}", actual_encoded.header.filter);
		},
	}
}
