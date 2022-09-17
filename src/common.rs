//! Common includes and functions for both library and executable.

use std::cmp::min;

pub use super::error::{AssemblyCode, AssemblyError};
pub use super::mcro::Macro;
pub use super::parser::Environment;

/// Assembler result type.
pub type AssemblyResult = miette::Result<(std::sync::Arc<std::cell::RefCell<Environment>>, Vec<u8>)>;

/// Pretty-print byte data as hexadecimal, similar to hex editors.
#[must_use]
pub fn pretty_hex(bytes: &[u8]) -> String {
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

/// Run the assembler on a single file.
pub fn run_assembler(file_name: &str) -> AssemblyResult {
	let source_code = AssemblyCode::from_file(file_name).map_err(|os_error| crate::AssemblyError::FileNotFound {
		os_error,
		file_name: file_name.to_string(),
		src: std::sync::Arc::new(AssemblyCode {
			name: std::path::PathBuf::from("<<arguments>>"),
			text: file_name.to_string(),
			..Default::default()
		}),
		location: (0, file_name.len()).into(),
	})?;
	let mut env = crate::Environment::new();
	let tokens = crate::parser::lexer::lex(source_code.clone())?;
	let program = crate::Environment::parse(&env, tokens, &source_code)?;
	let assembled = crate::assembler::assemble(&program)?;
	Ok((env, assembled))
}
