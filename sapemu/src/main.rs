//! S-APU / SPC700 emulator.
#![feature(slice_as_chunks, generic_const_exprs, adt_const_params, let_chains, bigint_helper_methods)]
#![cfg_attr(test, feature(try_blocks))]

use std::fs;
use std::path::PathBuf;
use std::time::Instant;

use ::log::{debug, info, warn, LevelFilter};
use anyhow::Result;
use clap::{Parser, ValueEnum};
use dsp::Dsp;
use smp::peripherals::{ProgramStatusWord, TestRegister};
use spcfile::parser::parse_from_bytes;
use time::macros::format_description;

use crate::memory::Memory;
use crate::smp::upload::Uploader;
use crate::smp::{Smp, CPU_RATE};

pub mod dsp;
pub mod memory;
pub mod smp;
#[macro_use]
mod log;

#[cfg(test)] mod test;

/// Input format the emulator can recognize.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum InputFormat {
	/// ELF binary.
	Elf,
	/// Standard SPC file (binary or text format).
	Spc,
}

#[derive(Clone, Debug, Parser)]
#[command(version = spcasm::buildinfo::PKG_VERSION, about, long_about = None)]
struct CliArguments {
	/// Input ELF or SPC file to execute. This is always uploaded via a simulated CPU uploader at the moment.
	input:   PathBuf,
	/// Force a certain input file format, in case it cannot be detected automatically.
	#[arg(long)]
	format:  Option<InputFormat>,
	/// Verbosity level to use.
	#[arg(long, short, action = clap::ArgAction::Count)]
	verbose: u8,
	/// CPU cycles to execute at maximum.
	#[arg(long)]
	cycles:  Option<usize>,
}

fn try_all_formats(
	file_data: &[u8],
	smp: &mut Smp,
	memory: &mut Memory,
	dsp: &mut Dsp,
	arguments: &CliArguments,
	ticks: &mut usize,
) -> Result<()> {
	if object::read::elf::ElfFile32::<object::LittleEndian>::parse(file_data).is_ok() {
		upload_from_elf(file_data, smp, memory, dsp, arguments, ticks)
	} else {
		upload_from_spc(file_data, smp, memory, dsp, arguments, ticks)
	}
}

fn upload_from_elf(
	file_data: &[u8],
	smp: &mut Smp,
	memory: &mut Memory,
	dsp: &mut Dsp,
	arguments: &CliArguments,
	ticks: &mut usize,
) -> Result<()> {
	let mut uploader = Uploader::from_elf(&object::read::elf::ElfFile32::parse(file_data)?)?;

	while !smp.is_halted() && arguments.cycles.map_or(true, |cycles| *ticks < cycles) {
		uploader.perform_step(&mut smp.ports);
		smp.tick(memory, &mut dsp.registers);
		dsp.tick(memory);
		*ticks += 1;
		if uploader.is_finished() {
			break;
		}
	}
	Ok(())
}

fn upload_from_spc(
	file_data: &[u8],
	smp: &mut Smp,
	memory: &mut Memory,
	dsp: &mut Dsp,
	_arguments: &CliArguments,
	_ticks: &mut usize,
) -> Result<()> {
	#[allow(clippy::redundant_closure_for_method_calls)]
	let file = parse_from_bytes(file_data).map_err(|e| e.to_owned())?;
	smp.a = file.header.a;
	smp.x = file.header.x;
	smp.y = file.header.y;
	smp.pc = file.header.pc;
	smp.sp = file.header.sp;
	smp.psw = ProgramStatusWord(file.header.psw);

	memory.ram = *file.memory.ram;
	dsp.load_register_bank(&file.memory.dsp_registers);

	smp.copy_mapped_registers_from_memory(memory);
	// SHENANIGANS! Many ROMs crash the SPC700 to be able to extract the ROM state easily.
	// Clear the initial test register crash state so that doesn’t stop us.
	smp.test = TestRegister::default();
	trace!("{}", smp.is_halted());

	Ok(())
}

#[allow(clippy::cast_precision_loss)]
fn main() -> Result<()> {
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
	let mut dsp = Dsp::new();

	let file_data = fs::read(&arguments.input)?;

	let start_time = Instant::now();
	let mut ticks = 0;

	match arguments.format {
		Some(InputFormat::Elf) => upload_from_elf(&file_data, &mut smp, &mut memory, &mut dsp, &arguments, &mut ticks),
		Some(InputFormat::Spc) => upload_from_spc(&file_data, &mut smp, &mut memory, &mut dsp, &arguments, &mut ticks),
		None => try_all_formats(&file_data, &mut smp, &mut memory, &mut dsp, &arguments, &mut ticks),
	}?;

	debug!("Memory state after upload:\n{}", spcasm::pretty_hex(&memory.ram, None));

	trace!("{}", smp.is_halted());

	while !smp.is_halted() && arguments.cycles.map_or(true, |cycles| ticks < cycles) {
		smp.tick(&mut memory, &mut dsp.registers);
		dsp.tick(&mut memory);
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

	Ok(())
}
