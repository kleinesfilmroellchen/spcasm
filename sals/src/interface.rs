//! Interface logic for converting spcasm data to LSP data and vice-versa.

use miette::Diagnostic;
use spcasm::AssemblyError;
use tower_lsp::lsp_types::*;

// FIXME: VSCode and the LSP insist on using line and column positioning instead of raw offsets.
//        Some reasoning is given in https://github.com/Microsoft/language-server-protocol/issues/96, but I'm not convinced.
//        These converter functions have linear runtime in the number of lines and could benefit from cached line
//        storage, but I don't want to put that inside spcasm itself which doesn't need it.

/// Convert the source code offset in the given source code to an LSP position. This function does not return anything
/// in the following cases:
/// - The offset is past the end of the text.
/// - The offset(s) overflow a 32-bit unsigned integer, in which case LSP could not handle the Position.
pub fn source_offset_to_lsp_position(offset: usize, text: &str) -> Option<Position> {
	// Classic use of prefix sums for line offsets.
	let (line_number, line_end_offset, line) = text
		.lines()
		.scan(0, |sum, line| {
			*sum += u32::try_from(line.chars().count()).ok()? + 1;
			Some((*sum, line))
		})
		.enumerate()
		.find_map(|(line_index, (line_end_offset, line))| {
			if line_end_offset as usize > offset {
				Some((u32::try_from(line_index).ok()?, line_end_offset, line))
			} else {
				None
			}
		})?;

	let column = u32::try_from(line.chars().count()).ok()? + 1 - (line_end_offset - u32::try_from(offset).ok()?);
	Some(Position::new(line_number, column))
}

pub fn lsp_position_to_source_offset(position: Position, text: &str) -> usize {
	text.lines().take(position.line as usize).map(|line| line.chars().count() + 1).sum::<usize>()
		+ (position.character as usize)
}

/// Note that since every miette-based assembly error can contain multiple labeled spans, it can map to multiple
/// diagnostics for each span.
pub fn assembly_error_to_lsp_diagnostics(error: AssemblyError, text: &str) -> Vec<tower_lsp::lsp_types::Diagnostic> {
	error
		.labels()
		// Poor miette design; an iterator can be empty so why isn't the type of `labels()` just `Box<dyn Iterator<Item = LabeledSpan>>`?
		.unwrap_or(Box::new(Vec::new().into_iter()))
		.filter_map(|label| {
			// FIXME: Fit the extended help message in somewhere.
			Some(tower_lsp::lsp_types::Diagnostic {
				range: Range::new(
					source_offset_to_lsp_position(label.offset(), text)?,
					source_offset_to_lsp_position(label.offset() + label.len(), text)?,
				),
				severity:            Some(miette_severity_to_lsp_severity(error.severity().unwrap_or_else(|| panic!("spcasm bug: error {error:?} without severity")))),
				code:                Some(NumberOrString::String(error.code().unwrap_or_else(|| panic!("spcasm bug: error {error:?} without error code")).to_string())),
				code_description:    None,
				source:              Some("spcasm".into()),
				message:             error.to_string(),
				related_information: None,
				tags:                None,
				data:                None,
			})
		})
		.collect()
}

pub fn miette_severity_to_lsp_severity(severity: miette::Severity) -> DiagnosticSeverity {
	match severity {
		miette::Severity::Advice => DiagnosticSeverity::INFORMATION,
		miette::Severity::Warning => DiagnosticSeverity::WARNING,
		miette::Severity::Error => DiagnosticSeverity::ERROR,
	}
}
