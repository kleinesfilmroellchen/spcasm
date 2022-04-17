//! SPC700 assembler.

#![feature(let_chains, result_flattening, is_some_with, get_mut_unchecked)]
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![deny(missing_docs)]

use std::cmp::min;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::Write;

pub mod assembler;
pub mod lexer;
pub mod parser;

fn pretty_hex(bytes: &[u8]) -> String {
	let mut string = String::new();
	// need approximately high nibble + low nibble + ' ' per byte
	string.reserve(bytes.len() * 3);
	let mut index = 0;
	while index * 16 < bytes.len() {
		let section = &bytes[index * 16 .. min((index + 1) * 16, bytes.len())];
		for byte in section {
			string += &format!(" {:02X}", byte);
		}
		string.push('\n');
		index += 1;
	}
	string
}

fn main() {
	if args().nth(1).expect("No file name given") == "--help" {
		println!("Usage: spcasm [--help] INFILE OUTFILE");
		std::process::exit(0);
	}
	let filename = unsafe { args().nth(1).unwrap_unchecked() };
	let output = args().nth(2).expect("No output file given");

	let contents = read_to_string(filename).expect("Couldn't read file contents");
	let maybe_lexed = lexer::lex(&contents);
	// println!("{:?}", maybe_lexed);
	let lexed = maybe_lexed.unwrap();
	let mut env = parser::Environment::new();
	let parsed = env.parse(&lexed);
	// println!("{:#?}, {:?}", parsed, env);

	let assembled = assembler::assemble(&env, parsed.unwrap());
	match assembled {
		Ok(assembled) => {
			println!("{}", pretty_hex(&assembled));
			let mut outfile =
				File::options().create(true).truncate(true).write(true).open(output).expect("Couldn't open output file");
			outfile.write_all(&assembled).expect("I/O error while writing");
		},
		err => println!("{:?}", err),
	}
}
