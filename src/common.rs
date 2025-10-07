//! Common includes and functions for both library and executable.

use std::cmp::min;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use miette::SourceSpan;
use parking_lot::RwLock;

pub use super::directive::Directive;
pub use super::error::AssemblyError;
pub use super::sema::Environment;
use crate::assembler::EntryPoint;
use crate::cli::{Frontend, default_backend_options};
use crate::sema::ProgramElement;
use crate::sema::reference::Label;
use crate::{AssemblyCode, Segments};

/// Assembler result type.
pub type AssemblyResult = Result<AssemblyOutput, Box<AssemblyError>>;

/// Output data structure for the assembler.
///
/// This struct contains various pieces of data produced by the assembler, ranging from early support data useful for
/// debugging (like the [`Environment`]) to finalized output data.
#[non_exhaustive]
#[derive(Clone, Debug)]
pub struct AssemblyOutput {
	/// The original source code of the output.
	pub source_code:        Arc<AssemblyCode>,
	/// The abstract (AST) segments that were created before assembly.
	pub abstract_segments:  Segments<ProgramElement>,
	/// The final assembled segments.
	pub assembled_segments: Segments<u8>,
	/// The program entry point.
	pub entry_point:        EntryPoint,
	/// The options that were used to run the assembler.
	pub options:            Arc<dyn Frontend>,
	/// The environment that was used by the assembler.
	pub environment:        Arc<RwLock<Environment>>,
}

impl AssemblyOutput {
	/// Convert the assembled data into a flat binary that represents the final format in the address space.
	///
	/// # Errors
	/// If segments overlap.
	#[must_use = "This function performs expensive work and copies a lot of data."]
	pub fn flattened_binary(&self) -> Result<Vec<u8>, Box<AssemblyError>> {
		self.assembled_segments.flatten(&self.source_code)
	}
}

/// Pretty-print byte data as hexadecimal, similar to hex editors.
#[must_use]
pub fn pretty_hex(bytes: &[u8], emphasis: Option<usize>) -> SharedStr {
	let mut string = String::new();
	// need approximately high nibble + low nibble + ' ' per byte
	let mut index = 0;
	while index * 16 < bytes.len() {
		let section = &bytes[index * 16 .. min((index + 1) * 16, bytes.len())];
		for (column, byte) in section.iter().enumerate() {
			string.push_str(
				if let Some(emphasis) = emphasis
					&& index * 16 + column == emphasis
				{
					format!(" [{byte:02X}]")
				} else {
					format!(" {byte:02X}")
				}
				.as_ref(),
			);
		}
		string.push('\n');
		index += 1;
	}
	string.into()
}

/// Dumps the tree of references for debugging purposes
pub fn dump_reference_tree(global_references: &[Arc<RwLock<Label>>]) {
	dump_reference_tree_impl(&mut global_references.iter(), 0);
}

#[allow(clippy::significant_drop_tightening)]
fn dump_reference_tree_impl(references: &mut dyn Iterator<Item = &Arc<RwLock<Label>>>, level: usize) {
	for global in references {
		let global = global.read();
		let label_text = global
			.location
			.as_ref()
			.and_then(|location| location.try_value(global.source_span(), &Arc::new(AssemblyCode::new("", ""))).ok())
			.map_or_else(|| "(unknown)".to_string(), |location| format!("{location:04X}"));

		println!(
			"{:_<70}{:>8}",
			format!("{}{}{} ", if level > 0 { " " } else { "" }, ".".repeat(level), global.name),
			label_text
		);
		let mut locals = global.children.values().collect::<Vec<_>>();
		locals.sort_by_cached_key(|label| {
			label
				.read()
				.location
				.as_ref()
				.and_then(|location| {
					location.try_value(global.source_span(), &Arc::new(AssemblyCode::new("", ""))).ok()
				})
				.map_or_else(|| "(unknown)".to_string(), |location| format!("{location:04X}"))
		});
		dump_reference_tree_impl(&mut locals.into_iter(), level + 1);
	}
}

/// Dumps the program AST (abstract syntax tree) for debugging purposes.
pub fn dump_ast(ast: &[ProgramElement]) {
	println!("AST:\n{}", ast.iter().map(ProgramElement::to_string).intersperse("\n".into()).collect::<String>());
}

/// Pseudo-[`std::fmt::Display`] implementation for [`SourceSpan`](https://docs.rs/miette/latest/miette/struct.SourceSpan.html).
#[must_use]
pub fn span_to_string(span: SourceSpan) -> SharedStr {
	format!("({:<4}-{:<4})", span.offset(), span.offset().saturating_add(span.len())).into()
}

/// Pseudo-[`std::fmt::Display`] implementation for the expected output of a program element.
pub fn byte_vec_to_string(vec: &Option<Vec<u8>>) -> std::string::String {
	vec.as_ref().map_or_else(std::string::String::new, |expected_value| {
		format!(
			"(expected {})",
			expected_value
				.iter()
				.map(|element| format!("{element:02X}"))
				.intersperse(" ".to_string())
				.collect::<String>()
		)
	})
}

/// Run the assembler on the given source code and return the segments, both assembled and in AST form.
///
/// If no options are given, default options for the CLI are used.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler(source_code: Arc<AssemblyCode>, options: Option<Arc<dyn Frontend>>) -> AssemblyResult {
	let options: Arc<dyn Frontend> = options.unwrap_or_else(default_backend_options).clone();
	let result: Result<_, AssemblyError> = try {
		let environment = crate::Environment::new();
		environment.write().set_error_options(options.clone());
		let tokens = crate::parser::lex(source_code.clone(), &*options)?;
		let program = crate::Environment::parse(&environment, tokens, &source_code)?;
		let mut segmented_program = program.write().split_into_segments()?;
		let (assembled, entry_point) =
			crate::assembler::assemble_inside_segments(&mut segmented_program, &source_code, options.clone())
				.map_err(AssemblyError::from)?;
		AssemblyOutput {
			source_code,
			abstract_segments: segmented_program,
			assembled_segments: assembled,
			entry_point,
			options: options.clone(),
			environment,
		}
	};
	result.map_err(|why| {
		options.report_diagnostic(why.clone());
		why.into()
	})
}

/// Provides a name for enum variants.
pub trait VariantName {
	/// Returns the name of this variant.
	fn variant_name(&self) -> &'static str;
}

/// Derives the [`VariantName`] enum.
#[macro_export]
macro_rules! VariantName {
	derive() (
		$(#[$_m:meta])*
		$_p:vis enum $Enum:ident {
			$(
				$(#[$_v:meta])*
				$unit_variant_name:ident
			),* $(,)?
		}
	) => {
		#[automatically_derived]
		impl VariantName for $Enum {
			fn variant_name(&self) -> &'static str {
				match self {
					$( Self::$unit_variant_name => stringify!($unit_variant_name) ),*
				}
			}
		}
	};
}
