//! User-defined options specified through the web interface.

use std::sync::RwLock;

use miette::{Diagnostic, Severity};
use serde::{Deserialize, Serialize};
use spcasm::AssemblyError;
use spcasm::cli::Frontend;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Serialize, Deserialize)]
pub struct WebOptions {
	pub silence_all:                     bool,
	pub silenced:                        Vec<String>,
	pub max_reference_resolution_passes: usize,
	pub max_macro_expansion_depth:       usize,
	#[serde(skip)]
	pub diagnostics:                     RwLock<Vec<AssemblyError>>,
}

impl Frontend for WebOptions {
	fn is_error(&self, _: &AssemblyError) -> bool {
		false
	}

	fn is_ignored(&self, warning: &AssemblyError) -> bool {
		warning.severity().unwrap() != Severity::Error
			&& (self.silence_all || self.silenced.contains(&warning.code().unwrap().to_string()))
	}

	fn maximum_reference_resolution_passes(&self) -> usize {
		self.max_reference_resolution_passes
	}

	fn maximum_macro_expansion_depth(&self) -> usize {
		self.max_macro_expansion_depth
	}

	fn report_diagnostic_impl(&self, diagnostic: AssemblyError) {
		super::log!("pushed new diagnostic: {:?}", diagnostic);
		self.diagnostics.write().unwrap().push(diagnostic);
	}
}
