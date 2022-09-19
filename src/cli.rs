//! Command-line interface related structures.

use std::mem::Discriminant;
use std::str::FromStr;

#[cfg(feature = "clap")] use clap::Args;

use crate::error::{AssemblyError, ErrorCodes};

/// Specification of which errors to include and which to not include.
#[derive(Debug, Clone, Eq, PartialEq, Default, Args)]
#[cfg(feature = "clap")]
pub struct ErrorOptions {
	/// Warnings to silence.
	#[clap(value_parser, multiple_occurrences = true, long, short = 'w')]
	pub(crate) ignore: Vec<ErrorCodeSpec>,
	/// Warnings to turn into a hard error.
	#[clap(value_parser, multiple_occurrences = true, long, short = 'W')]
	pub(crate) error:  Vec<ErrorCodeSpec>,
}

#[cfg(feature = "clap")]
impl ErrorOptions {
	/// Expands a marker "all" warning in the error or ignore list into all possible errors. This is so that the user
	/// can specify --error all or --ignore all and get this behavior, but we don't need special casing for it in the
	/// backend, which only works via matching discriminants.
	pub fn expand_all(&mut self) {
		let all_warnings_and_errors = AssemblyError::all_codes();
		for list in [&mut self.ignore, &mut self.error] {
			if list.contains(&std::mem::discriminant(&AssemblyError::AllMarker {}).into()) {
				list.append(&mut all_warnings_and_errors.clone().into_keys().map(ErrorCodeSpec).collect());
			}
		}
	}
}

/// Non-clap builds get this fake ErrorOptions struct which is a ZST and does nothing.
#[cfg(not(feature = "clap"))]
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
pub struct ErrorOptions {}

#[derive(Debug, Clone, Eq, PartialEq)]
#[repr(transparent)]
pub struct ErrorCodeSpec(Discriminant<AssemblyError>);

impl From<Discriminant<AssemblyError>> for ErrorCodeSpec {
	fn from(d: Discriminant<AssemblyError>) -> Self {
		Self(d)
	}
}

const error_prefix: &'static str = "spcasm::";

impl FromStr for ErrorCodeSpec {
	type Err = String;

	fn from_str(string_code: &str) -> Result<Self, Self::Err> {
		let code_map = AssemblyError::all_codes();
		let discriminant = code_map
			.iter()
			.find(|(_, value)| {
				(value == &string_code)
				// If the user provided an error code not starting with spcasm:: (very reasonable), just ignore the prefix.
					|| (!string_code.starts_with(error_prefix) && value[error_prefix.len() ..] == *string_code)
			})
			.map(|(key, _)| *key)
			.ok_or_else(|| "invalid error code".to_string())?;

		Ok(discriminant.into())
	}
}

#[cfg(feature = "clap")]
mod clap_dependent {
	use std::path::PathBuf;

	use clap::{Parser, ValueEnum};

	use super::ErrorOptions;

	/// SPC700 assembler.
	#[derive(Parser)]
	#[clap(author, version, about, long_about=None)]
	pub struct SpcasmCli {
		/// Assembly file to assemble.
		#[clap(value_parser)]
		pub input:         PathBuf,
		/// Binary output file.
		#[clap(value_parser)]
		pub output:        Option<PathBuf>,
		#[clap(flatten)]
		pub warning_flags: ErrorOptions,
		/// Format to output to.
		///
		/// - elf: Output the binary data within a .data section of an ELF file.
		///
		/// - plain: Output just the binary data.
		///
		/// - hexdump: Dump hexadecimal representation in a pretty format like in a hex editor.
		#[clap(value_parser, default_value = "elf", long, short = 'f')]
		pub output_format: OutputFormat,
	}

	#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
	#[repr(u8)]
	pub enum OutputFormat {
		Elf,
		Plain,
		HexDump,
	}
}

#[cfg(feature = "clap")]
pub use clap_dependent::*;
