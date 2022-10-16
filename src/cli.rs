//! Command-line interface related structures.

use std::mem::Discriminant;
use std::str::FromStr;
use std::sync::Arc;

#[cfg(feature = "binaries")]
use clap::Args;
use miette::Diagnostic;

use crate::error::{AssemblyError, ErrorCodes};

/// Interface for retrieving backend-relevant assembler options. The implementations are specific to the particular
/// frontend. Currently there are two frontend implementations in spcasm itself:
/// - ``CliOptions``: Only available and used in binary builds, created by clap.
/// - ``DummyOptions``: Always available (used e.g. in tests), returns defaults that are equivalent to not overriding
///   anything on the command line.
pub trait BackendOptions: std::fmt::Debug {
	/// Expands a marker "all" warning in the error or ignore list into all possible errors. This is so that the user
	/// can specify --error all or --ignore all and get this behavior, but we don't need special casing for it in the
	/// backend, which only works via matching discriminants.
	fn expand_all(&mut self);

	/// Returns whether the given warning is ignored, i.e. the user must not be informed of it.
	fn is_ignored(&self, warning: &AssemblyError) -> bool;
	/// Returns whether the given warning was turned into an error, i.e. it stops the assembler.
	fn is_error(&self, warning: &AssemblyError) -> bool;

	/// Returns the maximum macro expansion/recursion depth.
	fn maximum_macro_expansion_depth(&self) -> usize;
	/// Returns the maximum number of label resolution passes.
	fn maximum_label_resolution_passes(&self) -> usize;
}

/// Returns a ``BackendOptions`` implementation with default behavior.
pub fn default_backend_options() -> Arc<dyn BackendOptions> {
	Arc::new(DummyOptions {})
}

/// Backend options created by clap in binary builds.
#[derive(Debug, Clone, Eq, PartialEq, Default, Args)]
#[cfg(feature = "binaries")]
pub struct CliOptions {
	/// Warnings to silence.
	#[arg(num_args = 1, action = clap::ArgAction::Append, long, short = 'w')]
	pub(crate) ignore: Vec<ErrorCodeSpec>,
	/// Warnings to turn into a hard error.
	#[arg(num_args = 1, action = clap::ArgAction::Append, long, short = 'W')]
	pub(crate) error:  Vec<ErrorCodeSpec>,

	/// Limit for the number of label resolution passes spcasm will perform.
	///
	/// Usually 2-3 passes are enough and very high pass numbers often indicate infinite loops. If this number of
	/// passes is exceeded during label resolution, spcasm will report unresolved labels as normal.
	#[arg(long, short = 'l', default_value = "10")]
	pub(crate) label_pass_limit: usize,

	/// Limit for the number of recursive macro calls allowed by spcasm.
	///
	/// Increase this limit carefully; very high recursion amounts are usually caused by infinitely recursive macros.
	/// Any recursion exceeding this value will cause a specific error.
	#[arg(long, short = 'r', default_value = "1000")]
	pub(crate) macro_recursion_limit: usize,
}

#[cfg(feature = "binaries")]
impl BackendOptions for CliOptions {
	fn expand_all(&mut self) {
		let all_warnings_and_errors = AssemblyError::all_codes();
		for list in [&mut self.ignore, &mut self.error] {
			if list.contains(&std::mem::discriminant(&AssemblyError::AllMarker {}).into()) {
				list.append(&mut all_warnings_and_errors.clone().into_keys().map(ErrorCodeSpec).collect());
			}
		}
	}

	fn is_error(&self, warning: &AssemblyError) -> bool {
		// Always rethrow errors.
		warning.severity().is_some_and(|s| s == miette::Severity::Error) || {
			let discriminant = std::mem::discriminant(warning);
			self.error.contains(&discriminant.into())
		}
	}

	fn is_ignored(&self, warning: &AssemblyError) -> bool {
		let discriminant = std::mem::discriminant(warning);
		self.ignore.contains(&discriminant.into())
	}

	fn maximum_label_resolution_passes(&self) -> usize {
		self.label_pass_limit
	}

	fn maximum_macro_expansion_depth(&self) -> usize {
		self.macro_recursion_limit
	}
}

/// Dummy backend options that mirror command-line defaults.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
pub struct DummyOptions {}

impl BackendOptions for DummyOptions {
	fn expand_all(&mut self) {
		// noop
	}

	fn is_error(&self, warning: &AssemblyError) -> bool {
		false
	}

	fn is_ignored(&self, warning: &AssemblyError) -> bool {
		false
	}

	fn maximum_label_resolution_passes(&self) -> usize {
		10
	}

	fn maximum_macro_expansion_depth(&self) -> usize {
		1000
	}
}

#[derive(Debug, Clone, Eq, PartialEq)]
#[repr(transparent)]
pub(crate) struct ErrorCodeSpec(Discriminant<AssemblyError>);

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
					|| (!string_code.starts_with(error_prefix) && value.get(error_prefix.len()..).is_some_and(|latter_part| latter_part == string_code))
			})
			.map(|(key, _)| *key)
			.ok_or_else(|| "invalid error code".to_string())?;

		Ok(discriminant.into())
	}
}

#[cfg(feature = "binaries")]
mod clap_dependent {
	use std::path::PathBuf;

	use clap::{Parser, ValueEnum};

	use super::CliOptions;

	/// SPC700 assembler.
	#[derive(Parser)]
	#[command(version, about, long_about=None)]
	pub(crate) struct SpcasmCli {
		/// Assembly file to assemble.
		#[arg()]
		pub input:         PathBuf,
		/// Binary output file.
		#[arg()]
		pub output:        Option<PathBuf>,
		#[command(flatten)]
		pub warning_flags: CliOptions,
		/// Format to output to.
		///
		/// - elf: Output the binary data within a .data section of an ELF file.
		///
		/// - plain: Output just the binary data.
		///
		/// - hexdump: Dump hexadecimal representation in a pretty format like in a hex editor.
		#[arg(default_value = "elf", long, short = 'f')]
		pub output_format: OutputFormat,
	}

	#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
	#[repr(u8)]
	pub(crate) enum OutputFormat {
		Elf,
		Plain,
		HexDump,
	}
}

#[cfg(feature = "binaries")]
pub use clap_dependent::*;
