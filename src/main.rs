//! SPC700 assembler.

#![feature(let_chains)]
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![deny(missing_docs)]

use std::cmp::min;
use std::env::args;
use std::fs::read_to_string;

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
	let filename = args().nth(1).expect("No file name given");
	let contents = read_to_string(filename).expect("Couldn't read file contents");
	let maybe_lexed = lexer::lex(&contents);
	println!("{:?}", maybe_lexed);
	let lexed = maybe_lexed.unwrap();
	let mut env = parser::Environment::new();
	let parsed = env.parse(&lexed);
	println!("{:#?}, {:?}", parsed, env);

	let assembled = assembler::assemble(&env, parsed.unwrap());
	println!("{}", match assembled {
		Ok(assembled) => pretty_hex(&assembled),
		err => format!("{:?}", err),
	});
}
