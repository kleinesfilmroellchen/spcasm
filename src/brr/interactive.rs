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
	Encode {
		#[clap(value_parser)]
		samples: Vec<DecodedSample>,
		#[clap(short, long, help = "Override the previous samples to use for encoding")]
		warm_up: Option<Vec<DecodedSample>>,
	},
}

fn main() {
	let arguments = Arguments::parse();

	match arguments.mode {
		ExecMode::Encode { samples, warm_up } => {
			let warm_up = warm_up.unwrap_or(vec![0, 0]).try_into().unwrap_or_else(|_| {
				eprintln!("error: you must provide exactly 2 warm-up samples");
				std::process::exit(1);
			});
			let samples: DecodedBlockSamples = samples.try_into().unwrap_or_else(|_| {
				eprintln!("error: you must provide exactly 16 unencoded samples");
				std::process::exit(1);
			});
			println!("Encoding {:?} as BRR block.", samples);
			for filter in LPCFilter::all_filters() {
				let block = Block::encode_with_filter(warm_up, samples, filter, LoopEndFlags::Nothing);
				println!(
					"{}: encode to: {:?}\n\tshift: {:2}\n\tdecoded: {:?}\n\terror: {:10}",
					filter,
					block.encoded_samples,
					block.header.real_shift,
					block.decode(warm_up).0,
					block.total_encode_error(warm_up, &samples)
				)
			}
			let actual_encoded = Block::encode(warm_up, samples, LoopEndFlags::Nothing);
			println!("optimal encoding: filter {}", actual_encoded.header.filter);
		},
	}
}
