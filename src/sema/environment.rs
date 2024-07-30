//! [`Environment`].

use std::collections::HashMap;
use std::path::PathBuf;
use std::result::Result;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};
use miette::SourceSpan;
use parking_lot::RwLock;

use super::reference::Label;
use super::{AssemblyFile, LabelUsageKind};
use crate::cli::{default_backend_options, Frontend};
use crate::error::AssemblyError;
use crate::parser::{lalrpop_adaptor, Token};
use crate::AssemblyCode;

/// Environment object for parsing. Holds the list of references.
#[derive(Debug)]
pub struct Environment {
	/// The list of global labels.
	pub globals: HashMap<SharedStr, Arc<RwLock<Label>>>,
	/// The files included in this "tree" created by include statements.
	pub files:   HashMap<PathBuf, Arc<RwLock<AssemblyFile>>>,
	/// Error and warning options passed on the command line.
	pub options: Arc<dyn Frontend>,
}

impl Environment {
	/// Creates an empty environment.
	#[must_use]
	pub fn new() -> Arc<RwLock<Self>> {
		Arc::new(RwLock::new(Self {
			globals: HashMap::new(),
			files:   HashMap::new(),
			options: default_backend_options(),
		}))
	}

	/// Sets the user-provided error options.
	pub fn set_error_options(&mut self, options: Arc<dyn Frontend>) {
		self.options = options;
	}

	/// Searches for an existing parsed file in this environment given that file's source code.
	/// Note that the source code does not have to be the identical object in memory, it just has to compare equal.
	/// See [`crate::AssemblyCode::eq`] for the equality semantics of the source code objects.
	pub(crate) fn find_file_by_source(
		&self,
		source_code: &Arc<AssemblyCode>,
	) -> Result<Option<Arc<RwLock<AssemblyFile>>>, Box<AssemblyError>> {
		self.files
			.get(&source_code.name)
			// Keep around a tuple with the original Arc so we can return it at the end.
			.map(|file|
				file.try_read_recursive().map(|borrowed_file| (file, borrowed_file)))
			.map(|maybe_file| {
				maybe_file.map(|(file, _)| file.clone())
				.ok_or_else(|| AssemblyError::IncludeCycle {
					cycle_trigger_file: source_code.file_name(),
					src:                source_code.clone(),
					include:            (0, 0).into(),
				}.into())
			}).transpose()
	}

	/// Parse a program given a set of tokens straight from the lexer.
	/// The parser makes sure that all pre-processing of the token stream and the final reference resolutions are
	/// performed.
	///
	/// In terms of multi-file behavior, the resulting file is added to this environment's list of source files. If the
	/// source code already was parsed in this environment, that parsed data is returned instead. If, however, the
	/// source code was parsed but didn't have its includes fully resolved yet, that constitutes an include cycle and an
	/// error is returned.
	///
	/// # Errors
	/// Whenever something goes wrong in parsing.
	pub fn parse(
		this: &Arc<RwLock<Self>>,
		tokens: Vec<Token>,
		source_code: &Arc<AssemblyCode>,
	) -> Result<Arc<RwLock<AssemblyFile>>, Box<AssemblyError>> {
		#[allow(clippy::significant_drop_in_scrutinee)]
		if let Some(already_parsed_file) = this.read_recursive().find_file_by_source(source_code)? {
			// If we're in a cycle, the already parsed file still has unresolved references.
			// I'm not sure whether this can happen in the first place given that find_file_by_source can't borrow such
			// a file and therefore won't return it, but let's better be safe than sorry.
			return if already_parsed_file.try_read_recursive().is_some_and(|file| file.has_unresolved_source_includes())
			{
				Err(AssemblyError::IncludeCycle {
					cycle_trigger_file: source_code.file_name(),
					src:                source_code.clone(),
					include:            (0, 0).into(),
				}
				.into())
			} else {
				Ok(already_parsed_file)
			};
		}

		let lexed = lalrpop_adaptor::preprocess_token_stream(tokens);
		let lalrpop_lexed = lalrpop_adaptor::LalrpopAdaptor::from(lexed.clone());
		let program = crate::parser::ProgramParser::new()
			.parse(this, source_code, lalrpop_lexed)
			.map_err(|err| AssemblyError::from_lalrpop(err, source_code.clone()))?;

		let rc_file = Arc::new(RwLock::new(AssemblyFile {
			content:     program,
			source_code: source_code.clone(),
			parent:      Arc::downgrade(this),
			tokens:      lexed,
		}));
		let mut file = rc_file.write();

		file.resolve_user_macro_arguments()?;

		drop(file);
		// Insert the file into the list of source files so that we can detect cycles...
		this.write().files.insert(source_code.name.clone(), rc_file.clone());

		// ...once we start including source files here.
		let mut file = rc_file.write();
		file.resolve_source_includes()?;

		file.expand_user_macros()?;
		file.fill_in_reference_links()?;
		file.coerce_to_direct_page_addressing();
		drop(file);

		Ok(rc_file)
	}

	/// Lookup a global label in this environment, and create it if necessary.
	///
	/// # Errors
	/// If the label is already defined, and the label usage kind is for a definition, a redefinition error is returned.
	#[allow(clippy::result_large_err)] // simplifies lalrpop action code
	pub fn get_global_label(
		&mut self,
		name: &'_ str,
		span: SourceSpan,
		usage_kind: LabelUsageKind,
		source_code: &Arc<AssemblyCode>,
	) -> Result<Arc<RwLock<Label>>, AssemblyError> {
		if let Some(matching_reference) = self.globals.get(name) {
			let mut mutable_matching_reference = matching_reference.write();
			// If the caller flags this use of the reference as its definition, we check that this is the first
			// definition.
			if usage_kind == LabelUsageKind::AsDefinition {
				if mutable_matching_reference.has_definition() {
					return Err(AssemblyError::RedefinedReference {
						redefine_location:  span,
						reference_location: mutable_matching_reference.source_span(),
						reference:          mutable_matching_reference.to_string().into(),
						src:                source_code.clone(),
					});
				}
				mutable_matching_reference.definition_span = Some(span);
			} else {
				mutable_matching_reference.usage_spans.push(span);
			}
			drop(mutable_matching_reference);
			Ok(matching_reference.clone())
		} else {
			let new_reference = if usage_kind == LabelUsageKind::AsDefinition {
				Label::new_with_definition(name.into(), span)
			} else {
				Label::new_with_use(name.into(), span)
			};
			self.globals.insert(name.into(), new_reference.clone());
			Ok(new_reference)
		}
	}
}
