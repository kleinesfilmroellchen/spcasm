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
pub type AssemblyResult = miette::Result<(std::sync::Arc<RwLock<Environment>>, Vec<u8>)>;

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
			.and_then(|location| {
				location.try_value(global.source_span(), &Arc::new(AssemblyCode::new("", &String::new()))).ok()
			})
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
					location.try_value(global.source_span(), &Arc::new(AssemblyCode::new("", &String::new()))).ok()
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

/// Run the assembler on a single file. No errors options are provided; this is mainly intended for non-clap builds
/// where that has no effect anyways.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler_with_default_options(file_name: &str) -> AssemblyResult {
	run_assembler_on_file(file_name, default_backend_options())
}

/// Run the assembler on a single file.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler_on_file(file_name: &str, options: Arc<dyn Frontend>) -> AssemblyResult {
	let source_code = AssemblyCode::from_file_or_assembly_error(file_name).map_err(AssemblyError::from)?;
	run_assembler(&source_code, options)
}

/// Run the assembler on given source code. This method is intended to be used directly when the source code is not a
/// file on disk.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
#[allow(clippy::needless_pass_by_value)]
pub fn run_assembler(source_code: &Arc<AssemblyCode>, options: Arc<dyn Frontend>) -> AssemblyResult {
	let result: Result<_, AssemblyError> = try {
		let (env, mut segmented_program) = run_assembler_into_symbolic_segments(source_code, options.clone())?;
		let assembled = crate::assembler::assemble_from_segments(&mut segmented_program, source_code, options.clone())?;
		(env, assembled)
	};
	if let Err(ref why) = result {
		options.report_diagnostic_impl(why.clone());
	}
	result.map_err(miette::Report::from)
}

/// Run the assembler on the given source code and return the segmented AST as well as the environment.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
pub fn run_assembler_into_symbolic_segments(
	source_code: &Arc<AssemblyCode>,
	options: Arc<dyn Frontend>,
) -> Result<(Arc<RwLock<Environment>>, Segments<ProgramElement>), Box<AssemblyError>> {
	let env = crate::Environment::new();
	env.write().set_error_options(options.clone());
	let tokens = crate::parser::lex(source_code.clone(), &*options)?;
	let program = crate::Environment::parse(&env, tokens, source_code)?;
	let segmented_program = program.write().split_into_segments()?;
	Ok((env, segmented_program))
}

/// Run the assembler on the given source code and return the segments, both assembled and in AST form.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler_into_segments(
	source_code: &Arc<AssemblyCode>,
	options: Arc<dyn Frontend>,
) -> Result<(Segments<ProgramElement>, Segments<u8>, EntryPoint), Box<AssemblyError>> {
	let (_, mut segmented_program) = run_assembler_into_symbolic_segments(source_code, options.clone())?;
	let (assembled, entry_point) =
		crate::assembler::assemble_inside_segments(&mut segmented_program, source_code, options)
			.map_err(AssemblyError::from)?;
	Ok((segmented_program, assembled, entry_point))
}

/// Provides a name for enum variants.
pub trait VariantName {
	/// Returns the name of this variant.
	fn variant_name(&self) -> &'static str;
}
