//! S-APU / SPC700 emulator.
#![feature(slice_as_chunks, generic_const_exprs, adt_const_params, let_chains, bigint_helper_methods)]
#![cfg_attr(test, feature(try_blocks))]

use std::fs;
use std::path::PathBuf;
use std::time::Instant;

use clap::Parser;
use log::{debug, info, warn, LevelFilter};
use time::macros::format_description;

use crate::memory::Memory;
use crate::smp::upload::Uploader;
use crate::smp::{Smp, CPU_RATE};

pub mod dsp;
pub mod memory;
pub mod smp;

#[cfg(test)] mod test;

#[derive(Clone, Debug, Parser)]
#[command(version = spcasm::buildinfo::PKG_VERSION, about, long_about = None)]
struct CliArguments {
	/// Input ELF to execute. This is always uploaded via a simulated CPU uploader at the moment.
	input:   PathBuf,
	/// Verbosity level to use.
	#[arg(long, short, action = clap::ArgAction::Count)]
	verbose: u8,
	/// CPU cycles to execute at maximum.
	#[arg(long)]
	cycles:  Option<usize>,
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
		.with_timestamp_format(format_description!(version = 2, "[hour]:[minute]:[second]"))
		.init()
		.unwrap();

	warn!("sapemu version {}, licensed under BSD 2-clause", spcasm::buildinfo::PKG_VERSION);

	let mut memory = Box::new(Memory::new());
	let mut smp = Smp::new(&mut memory);

	let mut uploader =
		Uploader::from_elf(&object::read::elf::ElfFile32::parse(&*fs::read(arguments.input).unwrap()).unwrap())
			.unwrap();

	let start_time = Instant::now();
	let mut ticks = 0;
	while !smp.is_halted() && arguments.cycles.map_or(true, |cycles| ticks < cycles) {
		uploader.perform_step(&mut smp.ports);
		smp.tick(&mut memory);
		ticks += 1;
		if uploader.is_finished() {
			break;
		}
	}

	debug!("Memory state after upload:\n{}", spcasm::pretty_hex(&memory.ram, None));

	while !smp.is_halted() && arguments.cycles.map_or(true, |cycles| ticks < cycles) {
		smp.tick(&mut memory);
		ticks += 1;
	}

	let end_time = Instant::now();
	let frequency = ticks as f64 / (end_time - start_time).as_secs_f64();
	info!(
		"Ran {} cycles in {:.2?}, {:6.0} kHz, {:5.2}× realtime",
		ticks,
		end_time - start_time,
		frequency / 1000.,
		frequency / CPU_RATE as f64
	);
}
