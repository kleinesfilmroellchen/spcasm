//! User-defined options specified through the web interface.

use miette::Diagnostic;
use serde::{Deserialize, Serialize};
use spcasm::cli::BackendOptions;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WebOptions {
	pub silence_all:                     bool,
	pub silenced:                        Vec<String>,
	pub max_reference_resolution_passes: usize,
	pub max_macro_expansion_depth:       usize,
}

impl BackendOptions for WebOptions {
	fn expand_all(&mut self) {}

	fn is_error(&self, _: &spcasm::AssemblyError) -> bool {
		false
	}

	fn is_ignored(&self, warning: &spcasm::AssemblyError) -> bool {
		self.silence_all || self.silenced.contains(&warning.code().unwrap().to_string())
	}

	fn maximum_reference_resolution_passes(&self) -> usize {
		self.max_reference_resolution_passes
	}

	fn maximum_macro_expansion_depth(&self) -> usize {
		self.max_macro_expansion_depth
	}
}
