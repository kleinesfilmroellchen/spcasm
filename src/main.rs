//! SPC700 assembler.

#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![deny(missing_docs)]

use std::env::args;
use std::fs::read_to_string;

mod lexer;
mod parser;

fn main() {
	let filename = args().nth(1).expect("No file name given");
	let contents = read_to_string(filename).expect("Couldn't read file contents");
	let maybe_lexed = lexer::lex(&contents);
	println!("{:?}", maybe_lexed);
	let lexed = maybe_lexed.unwrap();
	let parsed = parser::parse(&lexed);
	println!("{:#?}", parsed);
}
