//! Simple example that dumps the parsed results from a .spc file.

#![feature(iterator_try_collect)]

use std::env::args_os;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use spcfile::parser::parse_from_bytes;

fn main() {
	let file = PathBuf::from(args_os().nth(1).expect("usage: spcdump <file.spc>"));

	let open_file = File::open(&file).unwrap_or_else(|_| panic!("error while opening file {file:?}"));
	let bytes = open_file.bytes().try_collect::<Vec<_>>().expect("error while reading input");

	match parse_from_bytes(&bytes) {
		Ok(parsed) => println!("Parsed SPC file:\n{parsed:?}"),
		Err(why) => match why {
			nom::Err::Incomplete(needed) => eprintln!("error: not enough input, need {needed:?}"),
			nom::Err::Error(e) | nom::Err::Failure(e) => eprintln!(
				"error while parsing SPC file:\n  at input {:x?} ...\n  {:?}",
				&e.input[.. (10.min(e.input.len()))],
				e.code
			),
		},
	}
}
