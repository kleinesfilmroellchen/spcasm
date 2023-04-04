//! Common includes and functions for both library and executable.

use std::cmp::min;
use std::sync::Arc;

use parking_lot::RwLock;
#[allow(unused)]
use smartstring::alias::String;

pub use super::directive::Directive;
pub use super::error::AssemblyError;
pub use super::parser::Environment;
use crate::cli::{default_backend_options, BackendOptions};
use crate::parser::reference::Label;
use crate::parser::ProgramElement;
use crate::{AssemblyCode, Segments};

/// Assembler result type.
pub type AssemblyResult = miette::Result<(std::sync::Arc<RwLock<Environment>>, Vec<u8>)>;

/// Pretty-print byte data as hexadecimal, similar to hex editors.
#[must_use]
pub fn pretty_hex(bytes: &[u8], emphasis: Option<usize>) -> String {
	let mut string = String::new();
	// need approximately high nibble + low nibble + ' ' per byte
	let mut index = 0;
	while index * 16 < bytes.len() {
		let section = &bytes[index * 16 .. min((index + 1) * 16, bytes.len())];
		for (column, byte) in section.iter().enumerate() {
			string = string + if let Some(emphasis) = emphasis && index * 16 + column == emphasis {
				format!(" [{:02X}]", byte)
			} else {
				format!(" {:02X}", byte)
			};
		}
		string.push('\n');
		index += 1;
	}
	string
}

/// Dumps the tree of references for debugging purposes
pub fn dump_reference_tree(global_references: &[Arc<RwLock<Label>>]) {
	dump_reference_tree_impl(&mut global_references.iter(), 0);
}

fn dump_reference_tree_impl(references: &mut dyn Iterator<Item = &Arc<RwLock<Label>>>, level: usize) {
	for global in references {
		let global = global.read();
		let label_text = global
			.location
			.as_ref()
			.and_then(|location| location.try_value(global.span, Arc::new(AssemblyCode::new("", &String::new()))).ok())
			.map_or_else(|| "(unknown)".to_string(), |location| format!("{:04X}", location));

		println!(
			"{:<40} {:>8}",
			format!("{}{}{}", if level > 0 { " " } else { "" }, ".".repeat(level), global.name),
			label_text
		);
		let mut locals = global.children.values().collect::<Vec<_>>();
		locals.sort_by_cached_key(|label| {
			label
				.read()
				.location
				.as_ref()
				.and_then(|location| {
					location.try_value(global.span, Arc::new(AssemblyCode::new("", &String::new()))).ok()
				})
				.map_or_else(|| "(unknown)".to_string(), |location| format!("{:04X}", location))
		});
		dump_reference_tree_impl(&mut locals.into_iter(), level + 1);
	}
}

/// Dumps the program AST (abstract syntax tree) for debugging purposes.
pub fn dump_ast(ast: &[ProgramElement]) {
	println!("AST:\n{}", ast.iter().map(ProgramElement::to_string).intersperse("\n".into()).collect::<String>());
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
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler_on_file(file_name: &str, options: Arc<dyn BackendOptions>) -> AssemblyResult {
	let source_code = AssemblyCode::from_file_or_assembly_error(file_name).map_err(AssemblyError::from)?;
	run_assembler(&source_code, options)
}

/// Run the assembler on given source code. This method is intended to be used directly when the source code is not a
/// file on disk.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler(source_code: &Arc<AssemblyCode>, options: Arc<dyn BackendOptions>) -> AssemblyResult {
	let env = crate::Environment::new();
	env.write().set_error_options(options.clone());
	let tokens = crate::parser::lexer::lex(source_code.clone(), &*options).map_err(AssemblyError::from)?;
	let program = crate::Environment::parse(&env, tokens, source_code).map_err(AssemblyError::from)?;
	let mut segmented_program = program.write().split_into_segments().map_err(AssemblyError::from)?;
	let assembled = crate::assembler::assemble_from_segments(&mut segmented_program, source_code, options)
		.map_err(AssemblyError::from)?;
	Ok((env, assembled))
}

/// Run the assembler on the given source code and return the segments, both assembled and in AST form.
///
/// # Errors
/// Any assembler errors are propagated to the caller.
pub fn run_assembler_into_segments(
	source_code: &Arc<AssemblyCode>,
	options: Arc<dyn BackendOptions>,
) -> Result<(Segments<ProgramElement>, Segments<u8>), Box<AssemblyError>> {
	let env = crate::Environment::new();
	env.write().set_error_options(options.clone());
	let tokens = crate::parser::lexer::lex(source_code.clone(), &*options).map_err(AssemblyError::from)?;
	let program = crate::Environment::parse(&env, tokens, source_code).map_err(AssemblyError::from)?;
	let mut segmented_program = program.write().split_into_segments().map_err(AssemblyError::from)?;
	let assembled = crate::assembler::assemble_inside_segments(&mut segmented_program, source_code, options)
		.map_err(AssemblyError::from)?;
	Ok((segmented_program, assembled))
}

/// Provides a name for enum variants.
pub trait VariantName {
	/// Returns the name of this variant.
	fn variant_name(&self) -> &'static str;
}
