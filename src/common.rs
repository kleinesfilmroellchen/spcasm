//! Common includes and functions for both library and executable.
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]

use std::cmp::min;
use std::sync::Arc;

pub use super::error::{AssemblyCode, AssemblyError};
pub use super::directive::Directive;
pub use super::parser::Environment;
use crate::cli::{default_backend_options, BackendOptions};

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

/// Run the assembler on a single file. No errors options are provided; this is mainly intended for non-clap builds
/// where that has no effect anyways.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler_with_default_options(file_name: &str) -> AssemblyResult {
	run_assembler(file_name, default_backend_options())
}

/// Run the assembler on a single file.
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler(file_name: &str, options: Arc<dyn BackendOptions>) -> AssemblyResult {
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
	run_assembler_on_source(&source_code, options)
}

/// Run the assembler on given source code. This method is intended to be used directly when the source code is not a
/// file on disk.
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler_on_source(source_code: &Arc<AssemblyCode>, options: Arc<dyn BackendOptions>) -> AssemblyResult {
	let mut env = crate::Environment::new();
	env.borrow_mut().set_error_options(options.clone());
	let tokens = crate::parser::lexer::lex(source_code.clone()).map_err(AssemblyError::from)?;
	let program = crate::Environment::parse(&env, tokens, source_code).map_err(AssemblyError::from)?;
	let assembled = crate::assembler::assemble(&program, options).map_err(AssemblyError::from)?;
	Ok((env, assembled))
}

/// Provides a name for enum variants.
pub trait VariantName {
	/// Returns the name of this variant.
	fn variant_name(&self) -> &'static str;
}
