#![doc = include_str!("../README.md")]
#![feature(iterator_try_collect)]

use std::time::Duration;

use chrono::NaiveDate;

pub mod parser;

/// A parsed .spc file.
#[derive(Clone, Debug)]
pub struct SpcFile {
	/// Header data.
	pub header: SpcHeader,
	/// Memory contents.
	pub memory: SpcMemory,
}

/// Initial memory state; the largest chunk of data in a .spc file.
#[derive(Clone, Debug)]
pub struct SpcMemory {
	/// Initial RAM state.
	pub ram:           Box<[u8; 65536]>,
	/// Initial DSP register state.
	pub dsp_registers: Box<[u8; 128]>,
	/// Memory used in place of the IPL ROM.
	pub rom:           Box<[u8; 64]>,
}

/// Header of a .spc file.
#[derive(Clone, Debug)]
pub struct SpcHeader {
	/// Version of the SPC file format. This parser can only read minor version 30, as it's the only one in widespread
	/// use.
	pub version: u8,

	/// Initial state of the program counter (PC) register.
	pub pc:  u16,
	/// Initial state of the A register.
	pub a:   u8,
	/// Initial state of the X register.
	pub x:   u8,
	/// Initial state of the Y register.
	pub y:   u8,
	/// Initial state of the flags (PSW) register.
	pub psw: u8,
	/// Initial state of the stack pointer (SP) register.
	pub sp:  u8,

	/// Title of the track.
	pub title:            String,
	/// Name of the game that the track belongs to.
	pub game:             String,
	/// Artist or composer of the track.
	pub artist:           String,
	/// Dumper of this .spc file.
	pub dump_author:      String,
	/// Comments attached by the dumper.
	pub comments:         String,
	/// Date of the dump.
	pub dump_date:        Option<NaiveDate>,
	/// Duration the track should play for (before fadeout).
	pub duration:         Duration,
	/// Duration the track should fade out for (after the end).
	pub fade_duration:    Duration,
	/// From `SNESAmp`'s manual: "Voices checked will automatically be muted at the beginning of the song."
	pub channel_disables: bool,
	/// Emulator used to create the dump.
	pub emulator:         Emulator,
}

/// List of known emulator IDs.
///
/// Table from <https://dgrfactory.jp/spcplay/id666.html>
#[derive(Clone, Copy, Debug, Default)]
#[allow(non_camel_case_types)]
pub enum Emulator {
	/// Unknown emulator (0x00, 0x30)
	#[default]
	Unknown,
	/// 0x31, 0x01
	ZSNES,
	/// 0x32, 0x02
	Snes9x,
	/// 0x33, 0x03
	ZST2SPC,
	/// Other emulator (0x04, 0x34); for some reason this is distinct from Unknown.
	Other,
	/// 0x35, 0x05
	SNEShout,
	/// 0x36, 0x06
	ZSNES_W,
	/// 0x07, 0x37
	Snes9xpp,
	/// 0x38, 0x08
	SNESGT,
}
