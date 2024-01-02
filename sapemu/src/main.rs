//! S-APU / SPC700 emulator.
#![deny(missing_docs, unused, clippy::all, clippy::pedantic, clippy::nursery, rustdoc::all)]

use std::path::PathBuf;
use std::time::Instant;

use clap::Parser;
use log::{info, warn, LevelFilter};
use time::macros::format_description;

use crate::memory::Memory;
use crate::smp::Smp;

pub mod dsp;
pub mod memory;
pub mod smp;

#[derive(Clone, Debug, Parser)]
#[command(version = spcasm::buildinfo::PKG_VERSION, about, long_about = None)]
struct CliArguments {
	/// Input ELF to execute.
	input:   PathBuf,
	/// Verbosity level to use.
	#[arg(long, short, action = clap::ArgAction::Count)]
	verbose: u8,
	/// CPU cycles to execute at maximum.
	#[arg(long)]
	cycles:  usize,
}

#[allow(clippy::cast_precision_loss)]
fn main() {
	human_panic::setup_panic!(human_panic::metadata!());

	let arguments = CliArguments::parse();
	let log_level = match arguments.verbose {
		0 => LevelFilter::Warn,
		1 => LevelFilter::Info,
		2 => LevelFilter::Debug,
		3 .. => LevelFilter::Trace,
	};
	simple_logger::SimpleLogger::new()
		.with_level(log_level)
		.with_local_timestamps()
		.with_timestamp_format(format_description!(version = 2, "[year]-[month]-[day] [hour]:[minute]:[second]"))
		.init()
		.unwrap();

	warn!("sapemu version {}, licensed under BSD 2-clause", spcasm::buildinfo::PKG_VERSION);

	let mut memory = Box::new(Memory::new());
	let mut smp = Smp::new(&mut memory);

	let start_time = Instant::now();
	for _ in 0 .. arguments.cycles {
		smp.tick(&mut memory);
	}
	let end_time = Instant::now();
	info!(
		"Ran {} cycles in {:?}, {:.0} Kcycles/s",
		arguments.cycles,
		end_time - start_time,
		arguments.cycles as f64 / (end_time - start_time).as_secs_f64() / 1000.
	);
}