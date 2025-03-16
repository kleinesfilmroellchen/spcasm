//! .spc file parsing functionality.
//!
//! See [the parent module](`crate`) for definitions of the Rust structures that represent the SPC file data.

#![allow(clippy::trivially_copy_pass_by_ref)]

use std::str::FromStr;
use std::time::Duration;

use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map_res, rest, verify};
use nom::error::{make_error, Error, ErrorKind};
use nom::number::complete::{le_u16, le_u8};
use nom::{Err, IResult, Parser};

use crate::{Emulator, SpcFile, SpcHeader, SpcMemory};

const MAGIC: &[u8] = b"SNES-SPC700 Sound File Data v0.30";
const MAGIC_LENGTH: usize = MAGIC.len();

/// Parse an SPC file from a byte slice.
///
/// # Errors
///
/// Any parser errors are passed on to the caller.
pub fn parse_from_bytes(bytes: &[u8]) -> Result<SpcFile, Err<Error<&[u8]>>> {
	let (_title, (_, header, memory, _rest)) = (tag(MAGIC), header, memory, rest).parse(bytes)?;
	Ok(SpcFile { header, memory })
}

/// Creates a string from a byte sequence that has any number of null bytes at the end. Assumes UTF-8 and discards
/// invalid characters.
fn null_terminated_string(input: &[u8]) -> String {
	// slightly less elegant than input.iter().take_while(|c| c != 0), but only makes one copy.
	let mut end = 0;
	while end < input.len() && input[end] != 0 {
		end += 1;
	}
	String::from_utf8_lossy(&input[.. end]).into_owned()
}

/// Parses the header byte that indicates whether there's an ID666 tag or not.
fn has_id666_info(input: u8) -> IResult<u8, bool> {
	match input {
		26 => Ok((input, true)),
		27 => Ok((input, false)),
		_ => Err(Err::Error(make_error(input, ErrorKind::OneOf))),
	}
}

/// Rest of the header that depends on binary vs. text format header.
#[derive(Debug)]
struct HeaderRest {
	pub dump_date:        Option<NaiveDate>,
	pub duration:         Duration,
	pub fade_duration:    Duration,
	pub channel_disables: bool,
	pub emulator:         Emulator,
	pub artist:           String,
}

// Initial date verification functions to quickly failover to the text header format.
// Complete verification is done by chrono's datetime parser.
const fn is_day(day: &u8) -> bool {
	*day >= 1 && *day <= 31
}
const fn is_month(month: &u8) -> bool {
	*month >= 1 && *month <= 12
}
const fn is_year(year: &u16) -> bool {
	*year >= 1 && *year <= 9999
}

fn binary_date(input: &[u8]) -> IResult<&[u8], Option<NaiveDate>> {
	let (rest, (day, month, year)) = alt((
		(verify(le_u8, is_day), verify(le_u8, is_month), verify(le_u16, is_year)),
		tag([0; 4].as_ref()).map(|_| (0, 0u8, 0u16)),
	))
	.parse(input)?;
	if day == 0 && month == 0 && year == 0 {
		Ok((rest, None))
	} else {
		NaiveDate::from_ymd_opt(i32::from(year), u32::from(month), u32::from(day))
			.map_or_else(|| Err(Err::Error(make_error(input, ErrorKind::Digit))), |v| Ok((rest, Some(v))))
	}
}

fn text_date(input: &[u8]) -> IResult<&[u8], Option<NaiveDate>> {
	let (rest, (month, day, year)) = alt((
		(
			take(2usize).and_then(parse_number::<u8>),
			tag(b"/".as_ref()),
			take(2usize).and_then(parse_number::<u8>),
			tag(b"/".as_ref()),
			take(4usize).and_then(parse_number::<u16>),
		)
			.map(|(m, _, d, _, y)| (m, d, y)),
		// default, if parsing fails
		tag([0; 11].as_ref()).map(|_| (0, 0, 0)),
	))
	.parse(input)?;

	if day == 0 && month == 0 && year == 0 {
		Ok((rest, None))
	} else {
		NaiveDate::from_ymd_opt(i32::from(year), u32::from(month), u32::from(day))
			.map_or_else(|| Err(Err::Error(make_error(input, ErrorKind::Digit))), |v| Ok((rest, Some(v))))
	}
}

fn emulator(input: u8) -> IResult<u8, Emulator> {
	match input {
		0x00 | 0x30 => Ok((input, Emulator::Unknown)),
		0x01 | 0x31 => Ok((input, Emulator::ZSNES)),
		0x02 | 0x32 => Ok((input, Emulator::Snes9x)),
		0x03 | 0x33 => Ok((input, Emulator::ZST2SPC)),
		0x04 | 0x34 => Ok((input, Emulator::Other)),
		0x05 | 0x35 => Ok((input, Emulator::SNEShout)),
		0x06 | 0x36 => Ok((input, Emulator::ZSNES_W)),
		0x07 | 0x37 => Ok((input, Emulator::Snes9xpp)),
		0x08 | 0x38 => Ok((input, Emulator::SNESGT)),
		_ => Err(Err::Error(make_error(input, ErrorKind::IsNot))),
	}
}

fn to_bool(input: u8) -> IResult<u8, bool> {
	match input {
		0 => Ok((input, false)),
		1 => Ok((input, true)),
		_ => Err(Err::Error(make_error(input, ErrorKind::IsNot))),
	}
}

fn parse_number<T: FromStr>(input: &[u8]) -> IResult<&[u8], T>
where
	<T as FromStr>::Err: std::fmt::Debug,
{
	null_terminated_string(input)
		.parse()
		.map_or_else(|_| Err(Err::Error(make_error(input, ErrorKind::Digit))), |v| Ok((input, v)))
}

fn rest_of_binary_header(input: &[u8]) -> IResult<&[u8], HeaderRest> {
	let (rest, (dump_date, _, duration, fade_duration, artist, (_, channel_disables), (_, emulator), _)) = (
		binary_date,
		take(7usize),
		// track length
		take(3usize).and_then(parse_number::<u64>).map(Duration::from_secs),
		// fade out length
		take(4usize).and_then(parse_number::<u64>).map(Duration::from_millis),
		// artist
		take(32usize).map(null_terminated_string),
		// channel disables
		map_res(le_u8, to_bool),
		map_res(le_u8, emulator),
		tag([0; 46].as_ref()),
	)
		.parse(input)?;
	Ok((rest, HeaderRest { dump_date, duration, fade_duration, channel_disables, emulator, artist }))
}

fn rest_of_text_header(input: &[u8]) -> IResult<&[u8], HeaderRest> {
	let (rest, (dump_date, duration, fade_duration, artist, (_, channel_disables), (_, emulator), _)) = (
		text_date,
		// track length
		take(3usize).and_then(parse_number::<u64>).map(Duration::from_secs),
		// fade out length
		take(5usize).and_then(parse_number::<u64>).map(Duration::from_millis),
		// artist
		take(32usize).map(null_terminated_string),
		// channel disables
		map_res(le_u8, to_bool),
		map_res(le_u8, emulator),
		tag([0; 45].as_ref()),
	)
		.parse(input)?;
	Ok((rest, HeaderRest { dump_date, duration, fade_duration, channel_disables, emulator, artist }))
}

fn header(bytes: &[u8]) -> IResult<&[u8], SpcHeader> {
	// TODO: actually respect the has_id666 indicator.
	let (rest, (_, (_, _has_id666), version, pc, a, x, y, psw, sp, _, title, game, dump_author, comments, header_rest)) =
		(
			tag([26, 26].as_ref()),
			map_res(le_u8, has_id666_info),
			alt((tag([30].as_ref()), tag([31].as_ref()))).map(|v: &[_]| v[0]),
			// PC
			le_u16,
			// A
			le_u8,
			// X
			le_u8,
			// Y
			le_u8,
			// PSW
			le_u8,
			// SP
			le_u8,
			// Reserved bytes
			take(2usize),
			// Song title
			take(32usize).map(null_terminated_string),
			// Game title
			take(32usize).map(null_terminated_string),
			// Dumper name
			take(16usize).map(null_terminated_string),
			// Comments
			take(32usize).map(null_terminated_string),
			// rest depends on the header type; the parsers try to verify that what they parsed is not bogus, but it
			// might still fail.
			alt((rest_of_binary_header, rest_of_text_header)),
		)
			.parse(bytes)?;
	let header = SpcHeader {
		version,
		pc,
		a,
		x,
		y,
		psw,
		sp,
		title,
		game,
		artist: header_rest.artist,
		dump_author,
		comments,
		dump_date: header_rest.dump_date,
		duration: header_rest.duration,
		fade_duration: header_rest.fade_duration,
		channel_disables: header_rest.channel_disables,
		emulator: header_rest.emulator,
	};
	let header_length = bytes.len() + MAGIC_LENGTH - rest.len();
	debug_assert!(header_length == 0x100, "header should be exactly 0x100 in length, but is {header_length:x}");
	Ok((rest, header))
}

#[allow(clippy::similar_names)]
fn memory(bytes: &[u8]) -> IResult<&[u8], SpcMemory> {
	let (rest, (ram, dsp_registers, _, rom)) =
		(take(65536usize), take(128usize), take(64usize), take(64usize)).parse(bytes)?;
	// length is ensured by the parser above, so these conversions are infallible
	Ok((rest, SpcMemory {
		ram:           Box::new(<[u8; _]>::try_from(ram).unwrap()),
		dsp_registers: Box::new(<[u8; _]>::try_from(dsp_registers).unwrap()),
		rom:           Box::new(<[u8; _]>::try_from(rom).unwrap()),
	}))
}
