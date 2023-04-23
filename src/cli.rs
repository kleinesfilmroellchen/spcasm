//! Command-line interface related structures.

use std::mem::Discriminant;
use std::str::FromStr;
use std::sync::Arc;

#[cfg(feature = "binaries")]
use clap::Args;
#[cfg(feature = "binaries")]
use miette::Diagnostic;
use miette::Severity;
use parking_lot::RwLock;
#[allow(unused)]
use smartstring::alias::String;

use crate::error::{AssemblyError, ErrorCodes};

/// Interface between the assembler backend and any kind of frontend. There currently are three main frontends: the
/// spcasm compiler command line program, the spcasm-web website in Wasm, and the sals language server. Each frontend
/// has different requirements for how the backend should react in various situations, especially error conditions. The
/// backend passes a reference to the frontend around and calls into it during assembly.
///
/// `Frontend` need to additionally implement Send and Sync (since it's used in multi-threaded contexts), and
/// provide debug output.
pub trait Frontend: std::fmt::Debug + Send + Sync {
	/// Returns whether the given warning is ignored, i.e. the user must not be informed of it.
	fn is_ignored(&self, warning: &AssemblyError) -> bool;
	/// Returns whether the given warning was turned into an error, i.e. it stops the assembler.
	fn is_error(&self, warning: &AssemblyError) -> bool;

	/// Returns the maximum macro expansion/recursion depth.
	fn maximum_macro_expansion_depth(&self) -> usize;
	/// Returns the maximum number of reference resolution passes.
	fn maximum_reference_resolution_passes(&self) -> usize;

	/// Not for public use; this function forces the frontend to receive a diagnostic no matter what its ignore status
	/// is.
	fn report_diagnostic_impl(&self, diagnostic: AssemblyError);

	/// Signals a diagnostic to the frontend. The frontend can decide whether it wants to output the diagnostic
	/// directly, collect it internally, etc. This function will not pass ignored diagnostics on to
	/// ``report_diagnostic_impl``.
	fn report_diagnostic(&self, diagnostic: AssemblyError) {
		// Pass on anything that is either an error or not ignored.
		if diagnostic.severity().is_some_and(|severity| severity == Severity::Error) || self.is_error(&diagnostic) || !self.is_ignored(&diagnostic) {
			self.report_diagnostic_impl(diagnostic);
		}
	}
}

/// Returns a `Frontend` implementation with default behavior.
#[must_use]
pub fn default_backend_options() -> Arc<dyn Frontend> {
	Arc::new(DummyOptions {})
}

/// Backend options created by clap in binary builds.
#[derive(Debug, Default, Args)]
#[cfg(feature = "binaries")]
#[allow(clippy::module_name_repetitions)]
pub struct CliOptions {
	/// Warnings to silence.
	#[arg(num_args = 1, action = clap::ArgAction::Append, long, short = 'w')]
	pub(crate) ignore: Vec<ErrorCodeSpec>,
	/// Warnings to turn into a hard error.
	#[arg(num_args = 1, action = clap::ArgAction::Append, long, short = 'W')]
	pub(crate) error:  Vec<ErrorCodeSpec>,

	/// Limit for the number of reference resolution passes spcasm will perform.
	///
	/// Usually 2-3 passes are enough and very high pass numbers often indicate infinite loops. If this number of
	/// passes is exceeded during reference resolution, spcasm will report unresolved references as normal.
	#[arg(long, short = 'l', default_value = "10")]
	pub(crate) reference_pass_limit: usize,

	/// Limit for the number of recursive macro calls allowed by spcasm.
	///
	/// Increase this limit carefully; very high recursion amounts are usually caused by infinitely recursive macros.
	/// Any recursion exceeding this value will cause a specific error.
	#[arg(long, short = 'r', default_value = "1000")]
	pub(crate) macro_recursion_limit: usize,

	#[clap(skip = RwLock::new(false))]
	pub(crate) had_error: RwLock<bool>,
}

#[cfg(feature = "binaries")]
impl CliOptions {
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

#[cfg(feature = "binaries")]
impl Frontend for CliOptions {
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

	fn maximum_reference_resolution_passes(&self) -> usize {
		self.reference_pass_limit
	}

	fn maximum_macro_expansion_depth(&self) -> usize {
		self.macro_recursion_limit
	}

	fn report_diagnostic_impl(&self, diagnostic: AssemblyError) {
		if self.is_error(&diagnostic) {
			*self.had_error.write() = true;
		}
		println!("{:?}", miette::Report::new(diagnostic));
	}
}

/// Dummy backend options that mirror command-line defaults.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
pub struct DummyOptions {}

impl Frontend for DummyOptions {
	fn is_error(&self, _warning: &AssemblyError) -> bool {
		false
	}

	fn is_ignored(&self, _warning: &AssemblyError) -> bool {
		false
	}

	fn maximum_reference_resolution_passes(&self) -> usize {
		10
	}

	fn maximum_macro_expansion_depth(&self) -> usize {
		1000
	}

	fn report_diagnostic_impl(&self, _diagnostic: AssemblyError) {
		// noop
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

const error_prefix: &str = "spcasm::";

impl FromStr for ErrorCodeSpec {
	type Err = std::string::String;

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
			.ok_or_else(|| String::from("invalid error code"))?;

		Ok(discriminant.into())
	}
}

#[cfg(feature = "binaries")]
mod clap_dependent {
	use std::path::PathBuf;

	use clap::{Parser, ValueEnum};

	use super::CliOptions;
	use crate::buildinfo;

	/// SPC700 assembler.
	#[derive(Parser)]
	#[command(version=
		format!("{}\n{} {}, built {}", buildinfo::PKG_VERSION, buildinfo::RUST_VERSION, buildinfo::BUILD_TARGET, buildinfo::BUILD_TIME), about, long_about=None)]
	pub struct SpcasmCli {
		/// Assembly file to assemble.
		#[arg()]
		pub input:         PathBuf,
		/// Binary output file.
		#[arg()]
		pub output:        Option<PathBuf>,
		///
		#[command(flatten)]
		pub warning_flags: CliOptions,
		/// Format to output to.
		#[arg(default_value = "elf", long, short = 'f')]
		pub output_format: OutputFormat,

		/// Dump all references and their final values / locations.
		#[arg(long, short = 'd')]
		pub dump_references: bool,
		/// Dump the program's abstract syntax tree. This is a debugging feature and most likely not useful to the end
		/// user.
		///
		/// WARNING: This option will, in specific circumstances, loop forever trying to print recursive data
		/// structures. This can happen on well-formed programs.
		#[arg(long, short = 'a')]
		pub dump_ast:        bool,
	}

	/// Format to output to; see `SpcasmCli`.
	#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
	#[repr(u8)]
	pub enum OutputFormat {
		/// Output the binary data within a .data section of an ELF file.
		Elf,
		/// Output just the binary data.
		Plain,
		/// Dump hexadecimal representation in a pretty format like in a hex editor.
		HexDump,
	}
}

#[cfg(feature = "binaries")]
pub use clap_dependent::*;
