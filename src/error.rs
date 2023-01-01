use std::collections::HashMap;
use std::fmt::Display;
use std::mem::Discriminant;
use std::num::ParseIntError;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use lalrpop_util::ParseError;
use miette::{Diagnostic, MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use spcasm_derive::ErrorCodes;
use thiserror::Error;

use crate::cli::BackendOptions;
use crate::directive::DirectiveSymbol;
use crate::parser::instruction::{MemoryAddress, Mnemonic};
use crate::parser::Token;

/// The source code for an assembly error.
#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct AssemblyCode {
	pub(crate) text:         String,
	/// The source code location must be canonicalized!
	pub(crate) name:         PathBuf,
	pub(crate) include_path: Vec<PathBuf>,
}

impl AssemblyCode {
	/// Create a new source code struct by loading a file's contents.
	///
	/// # Errors
	/// If reading the file fails (doesn't exist, permissions wrong, I/O error etc.)
	pub fn from_file(filename: &str) -> Result<Arc<Self>, std::io::Error> {
		let mut path = PathBuf::from(filename);
		if path.is_relative() {
			path = std::env::current_dir()?.join(path);
		}
		path = uniform_canonicalize(&path)?;
		let contents = std::fs::read_to_string(&path)?.chars().filter(|c| c != &'\r').collect();
		Ok(Arc::new(Self { name: path, text: contents, include_path: Vec::new() }))
	}

	/// Create a new source code struct by loading a file's contents, and immediately create an assembler error if that
	/// fails.
	///
	/// # Errors
	/// If reading the file fails (doesn't exist, permissions wrong, I/O error etc.)
	pub fn from_file_or_assembly_error(file_name: &str) -> Result<Arc<Self>, Box<AssemblyError>> {
		Self::from_file(file_name).map_err(|os_error| {
			AssemblyError::FileNotFound {
				os_error,
				file_name: file_name.to_string(),
				src: std::sync::Arc::new(Self {
					name: std::path::PathBuf::from("<<arguments>>"),
					text: file_name.to_string(),
					..Default::default()
				}),
				location: (0, file_name.len()).into(),
			}
			.into()
		})
	}

	/// Create a new source code struct from source code text and a (possibly fake) name.
	#[must_use]
	#[allow(clippy::missing_const_for_fn)]
	pub fn new(text: &str, name: String) -> Self {
		Self {
			text:         text.chars().filter(|c| c != &'\r').collect(),
			name:         PathBuf::from(name),
			include_path: Vec::new(),
		}
	}

	/// Returns a pretty-printed variant of the file name of this source code.
	///
	/// The pretty-printing rules are as follows:
	/// - If the file is relative to the working directory, print a relative file name without leading `./`.
	/// - If the file is not relative, i.e. its canonical path does not contain the working directory, print an absolute
	///   file name. On Windows, extended path length syntax (`\\?\`) is omitted.
	///
	/// # Panics
	/// Programming bugs.
	#[must_use]
	pub fn file_name(&self) -> String {
		Self::file_name_for(&self.name)
	}

	/// Returns a pretty-printed variant of the given path.
	///
	/// The pretty-printing rules are as follows:
	/// - If the file is relative to the working directory, print a relative file name without leading `./`.
	/// - If the file is not relative, i.e. its canonical path does not contain the working directory, print an absolute
	///   file name. On Windows, extended path length syntax (`\\?\`) is omitted.
	///
	/// # Panics
	/// Programming bugs.
	#[must_use]
	pub fn file_name_for(path: &Path) -> String {
		let cwd = uniform_canonicalize(&PathBuf::from(".")).unwrap();
		if path.starts_with(&cwd) {
			path.strip_prefix(cwd).unwrap().to_string_lossy().to_string()
		} else {
			path.as_os_str().to_string_lossy().to_string()
		}
	}
}

/// Implements a more uniform canonicalization. The main difference to ``std::fs::canonicalize`` is that it doesn't
/// create the extended length path syntax on Windows. This is for better compatibility with file link-supporting
/// terminals and the `trycmd` integration tests.
#[cfg(windows)]
#[inline]
pub fn uniform_canonicalize(path: &Path) -> std::io::Result<PathBuf> {
	// All extended length paths start with '\\?\' (length 4), see https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#maximum-path-length-limitation
	Ok(PathBuf::from(path.canonicalize()?.into_os_string().to_string_lossy()[4 ..].to_owned()))
}

/// Implements a more uniform canonicalization. The main difference to ``std::fs::canonicalize`` is that it doesn't
/// create the extended length syntax on Windows. This is for better compatibility with file link-supporting terminals
/// and the `trycmd` integration tests.
#[cfg(not(any(windows, target_family = "wasm")))]
#[inline]
pub fn uniform_canonicalize(path: &Path) -> std::io::Result<PathBuf> {
	path.canonicalize()
}

#[cfg(target_family = "wasm")]
#[inline]
pub fn uniform_canonicalize(path: &Path) -> std::io::Result<PathBuf> {
	Ok(path.to_owned())
}

impl SourceCode for AssemblyCode {
	fn read_span<'a>(
		&'a self,
		span: &SourceSpan,
		context_lines_before: usize,
		context_lines_after: usize,
	) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
		let result = self.text.read_span(span, context_lines_before, context_lines_after)?;
		let retval = Box::new(MietteSpanContents::new_named(
			self.file_name(),
			result.data(),
			*result.span(),
			result.line(),
			result.column(),
			result.line_count(),
		));
		Ok(retval)
	}
}

#[allow(clippy::module_name_repetitions)]
pub trait ErrorCodes {
	fn all_codes() -> HashMap<Discriminant<AssemblyError>, String>;
}

/// All types of errors that the assembler can report to the user.
#[derive(Error, Debug, Diagnostic, ErrorCodes)]
#[allow(clippy::module_name_repetitions, missing_docs)]
pub enum AssemblyError {
	/// Marker error for allowing the user to pass --ignore all or --error all on the CLI.
	#[error("FIXME: This should never appear.")]
	#[diagnostic(code(all), severity(Error))]
	AllMarker {},

	#[error("Legal architecture directive ignored")]
	#[diagnostic(
		code(spcasm::arch::valid),
		severity(Advice),
		help(
			"spcasm supports `arch` directives for compatibility with the Asar multi-architecture assembler. This \
			 arch directive points to the spc700 architecture and is therefore safely ignored."
		)
	)]
	ArchitectureDirectiveIgnored {
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("`arch` directive")]
		location: SourceSpan,
	},

	#[error("Unknown architecture `{arch}` specified")]
	#[diagnostic(
		code(spcasm::arch::invalid),
		severity(Error),
		help(
			"spcasm supports `arch` directives for compatibility with the Asar multi-architecture assembler. This \
			 directive specifies that the architecture of the assembly source is not (completely) SPC700, therefore \
			 spcasm cannot assemble this file."
		)
	)]
	InvalidArchitectureDirective {
		arch:     String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("`arch` directive")]
		location: SourceSpan,
	},

	#[error("Assigning a value to the macro argument '<{name}>' is not possible")]
	#[diagnostic(
		code(spcasm::user_macro::assign_to_argument),
		severity(Error),
		help(
			"Arguments of macros are given a value when the macro is called. Therefore, it does not make sense to \
			 assign them a value. If you need a label with a specific value inside a macro, use a local label under \
			 the macro's special '\\@' label instead"
		)
	)]
	AssigningToMacroArgument {
		name:     String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Assignment happens here")]
		location: SourceSpan,
	},

	#[error("Assigning a value to the special macro label '\\@' is not possible")]
	#[diagnostic(
		code(spcasm::user_macro::assign_to_global),
		severity(Error),
		help(
			"The special macro label '\\@' can be used for creating a unique global label per user macro call. It can \
			 therefore only be assigned a value by using it as the label for an instruction. If you need to compute \
			 values based on macro arguments, there is currently no non-repetitive way to do this."
		)
	)]
	AssigningToMacroGlobal {
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Assignment happens here")]
		location: SourceSpan,
	},

	#[error("Using the user defined macro argument '<{name}>' outside a macro is not possible")]
	#[diagnostic(code(spcasm::user_macro::argument_outside_macro), severity(Error))]
	UsingMacroArgumentOutsideMacro {
		name:     String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Used here")]
		location: SourceSpan,
	},

	#[error("User macro '{name}' is defined inside another user macro")]
	#[diagnostic(
		code(spcasm::user_macro::recursive_definition),
		severity(Error),
		help(
			"User-defined macros can only be defined at the top level of a file, as inner definition does not make \
			 sense. If you want to avoid name collisions, use a more distinctive name."
		)
	)]
	RecursiveMacroDefinition {
		name:     String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Inner macro defined here")]
		location: SourceSpan,
		#[label("Outer macro defined here")]
		outer:    SourceSpan,
	},

	#[error("Maximum recursion depth {depth} was exceeded while expanding user macro '{name}'")]
	#[diagnostic(
		code(spcasm::user_macro::recursive_use),
		severity(Error),
		help(
			"This is most likely caused by an infinitely recursive macro definition. On the command line, use \
			 `--macro-recursion-limit` to increase the limit."
		)
	)]
	RecursiveMacroUse {
		name:     String,
		depth:    usize,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("While trying to expand this macro")]
		location: SourceSpan,
	},

	#[error("Macro argument '{name}' has not been defined in this macro")]
	#[diagnostic(
		code(spcasm::user_macro::undefined_argument),
		severity(Error),
		help("The available arguments are: {}. Did you misspell the macro argument's name?",
			available_names.iter().map(|name| format!("'{}'", name)).collect::<Vec<_>>().join(", "))
	)]
	UnknownMacroArgument {
		name:            String,
		available_names: Vec<String>,
		#[source_code]
		src:             Arc<AssemblyCode>,
		#[label("Macro argument used here")]
		location:        SourceSpan,
	},

	#[error("Macro '{name}' is not defined")]
	#[diagnostic(
		code(spcasm::user_macro::undefined),
		severity(Error),
		help("The available macros are: {}.",
			available_macros.iter().map(|name| format!("'{}'", name)).collect::<Vec<_>>().join(", "))
	)]
	UndefinedUserMacro {
		name:             String,
		available_macros: Vec<String>,
		#[source_code]
		src:              Arc<AssemblyCode>,
		#[label("Macro used here")]
		location:         SourceSpan,
	},

	#[error("Macro '{name}' takes {expected_number} arguments, but {actual_number} were supplied")]
	#[diagnostic(
		code(spcasm::user_macro::incorrect_number_of_arguments),
		severity(Error),
		help("{} arguments", if expected_number > actual_number { "Add" } else { "Remove" })
	)]
	IncorrectNumberOfMacroArguments {
		name:            String,
		expected_number: usize,
		actual_number:   usize,
		#[source_code]
		src:             Arc<AssemblyCode>,
		#[label("In this macro call")]
		location:        SourceSpan,
		#[label("'{name}' defined here with {expected_number} arguments")]
		definition:      SourceSpan,
	},

	#[error("File \"{file_name}\" was not found")]
	#[diagnostic(code(spcasm::io::file_not_found), severity(Error))]
	FileNotFound {
		#[source]
		os_error:  std::io::Error,
		file_name: String,
		#[source_code]
		src:       Arc<AssemblyCode>,
		#[label("File was requested here")]
		location:  SourceSpan,
	},

	#[error("Include cycle detected while trying to include \"{cycle_trigger_file}\"")]
	#[diagnostic(
		code(spcasm::include_cycle),
		severity(Error),
		help(
			"The file \"{cycle_trigger_file}\" was included:\n{}", src.include_path.iter().map(|path| format!("from {}", AssemblyCode::file_name_for(path))).intersperse("\n".to_string()).collect::<String>()
		)
	)]
	IncludeCycle {
		cycle_trigger_file: String,
		#[source_code]
		src:                Arc<AssemblyCode>,
		#[label("This file's inclusion causes an include cycle")]
		include:            SourceSpan,
	},

	#[error("{error_text}")]
	#[diagnostic(
		code(spcasm::io::audio_processing_error),
		severity(Error),
		help(
			"This error is caused by the malformed input audio file \"{file_name}\". If your audio player understands \
			 this file just fine, please file an spcasm bug."
		)
	)]
	AudioProcessingError {
		error_text: String,
		file_name:  String,
		#[source_code]
		src:        Arc<AssemblyCode>,
		#[label("While processing audio here")]
		location:   SourceSpan,
	},

	#[error("Segment at {segment_start:04x} starts before the end of the previous one, which is {segment_end:04x}")]
	#[diagnostic(code(spcasm::segment::mismatch), severity(Error))]
	SegmentMismatch {
		segment_start: MemoryAddress,
		segment_end:   MemoryAddress,
		#[label("Unexpected")]
		location:      SourceSpan,
		#[source_code]
		src:           Arc<AssemblyCode>,
	},

	#[error("There is no active segment here")]
	#[diagnostic(
		code(spcasm::segment::missing),
		severity(Error),
		help("Start a new segment with `org <memory address>`")
	)]
	MissingSegment {
		#[label("This requires that there be a segment")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("There is no segment on the stack")]
	#[diagnostic(
		code(spcasm::segment::empty_stack),
		severity(Error),
		help("Directives like `pullpc` require that you push a segment to the stack beforehand with `pushpc`.")
	)]
	NoSegmentOnStack {
		#[label("Segment stack access here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Reference '{reference}' can not be resolved to a value")]
	#[diagnostic(
		code(spcasm::reference::unresolved),
		severity(Error),
		help(
			"Any symbolic reference must be defined somewhere. Did you misspell the reference's name?\nThis error is \
			 sometimes caused by too few reference resolution passes. Use `--reference-pass-limit` to increase the \
			 limit on the number of passes."
		)
	)]
	UnresolvedReference {
		reference:          String,
		#[label("'{reference}' defined here")]
		reference_location: Option<SourceSpan>,
		#[label("Used here")]
		usage_location:     SourceSpan,
		#[source_code]
		src:                Arc<AssemblyCode>,
	},

	#[error("Reference '\\@' can not be resolved to a value")]
	#[diagnostic(
		code(spcasm::reference::unresolved),
		severity(Error),
		help("The special macro global '\\@' is only usable inside user defined macros.")
	)]
	UnresolvedMacroGlobal {
		#[label("Used here")]
		usage_location: SourceSpan,
		#[source_code]
		src:            Arc<AssemblyCode>,
	},

	#[error("Invalid addressing mode `{mode}` as first operand for `{mnemonic}`")]
	#[diagnostic(
		code(spcasm::instruction::invalid_addressing_mode),
		severity(Error),
		help("The instruction `{mnemonic}` accepts the modes {} as first operands", 
			.legal_modes.iter().map(|mode| format!("{}, ", mode)).collect::<String>().strip_suffix(", ").unwrap_or_default()),
	)]
	InvalidFirstAddressingMode {
		mode:        String,
		mnemonic:    Mnemonic,
		legal_modes: Vec<String>,
		#[source_code]
		src:         Arc<AssemblyCode>,
		#[label("For this instruction")]
		location:    SourceSpan,
	},

	#[error("Invalid addressing mode `{mode}` as second operand for `{mnemonic}`")]
	#[diagnostic(
		code(spcasm::instruction::invalid_addressing_mode),
		severity(Error),
		help("The instruction `{mnemonic}`, with the first operand `{first_mode}`, accepts the modes {} as second operands", 
			.legal_modes.iter().map(|mode| format!("{}, ", mode)).collect::<String>().strip_suffix(", ").unwrap_or_default()),
	)]
	InvalidSecondAddressingMode {
		mode:        String,
		mnemonic:    Mnemonic,
		first_mode:  String,
		legal_modes: Vec<String>,
		#[source_code]
		src:         Arc<AssemblyCode>,
		#[label("For this instruction")]
		location:    SourceSpan,
	},

	#[error("Two operands are not allowed for `{mnemonic}`")]
	#[diagnostic(
		code(spcasm::instruction::two_operands_not_allowed),
		help("Remove the second operand"),
		severity(Error)
	)]
	TwoOperandsNotAllowed {
		mnemonic: Mnemonic,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Only takes 1 operand")]
		location: SourceSpan,
	},

	#[error("`{mnemonic}` doesn't take any operands")]
	#[diagnostic(
		code(spcasm::instruction::operand_not_allowed),
		help("Remove the operands of this instruction"),
		severity(Error)
	)]
	OperandNotAllowed {
		mnemonic: Mnemonic,
		#[label("Takes 0 operands")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("`{mnemonic}` takes at least one operand")]
	#[diagnostic(
		code(spcasm::instruction::missing_operand),
		help("Add any of the operands {} to this instruction",
			.legal_modes.iter().map(|mode| format!("{}, ", mode)).collect::<String>().strip_suffix(", ").unwrap_or_default()),
		severity(Error)
	)]
	MissingOperand {
		mnemonic:    Mnemonic,
		legal_modes: Vec<String>,
		#[label("Takes at least one operand")]
		location:    SourceSpan,
		#[source_code]
		src:         Arc<AssemblyCode>,
	},

	#[error("`{mnemonic}` takes two operands")]
	#[diagnostic(
		code(spcasm::instruction::missing_second_operand),
		help("Add any of the operands {} to this instruction",
			.legal_modes.iter().map(|mode| format!("{}, ", mode)).collect::<String>().strip_suffix(", ").unwrap_or_default()),
		severity(Error)
	)]
	MissingSecondOperand {
		mnemonic:    Mnemonic,
		legal_modes: Vec<String>,
		#[label("Takes two operands")]
		location:    SourceSpan,
		#[source_code]
		src:         Arc<AssemblyCode>,
	},

	#[error("`{constant}` is not valid for {typename}")]
	#[diagnostic(
		code(spcasm::instruction::invalid_constant),
		help("Remove the operands of this instruction"),
		severity(Error)
	)]
	InvalidConstant {
		constant: String,
		typename: String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Takes 0 operands")]
		location: SourceSpan,
	},

	#[error("Invalid use of labels in an argument for `{directive}`")]
	#[diagnostic(
		code(spcasm::directive::references_as_argument),
		help(
			"Because the directive argument can determine a reference's position, resolving the argument value is not \
			 generally possible. For this reason, references are not allowed to be used in a directive argument."
		)
	)]
	ReferencesInDirectiveArgument {
		directive: DirectiveSymbol,
		#[source_code]
		src:       Arc<AssemblyCode>,
		#[label("This directive")]
		location:  SourceSpan,
		// TODO: reintroduce when numbers have source locations
		// #[label("This directive argument")]
		// argument_location: SourceSpan,
	},

	#[error("There is no global label defined before the local label '{local_label}'")]
	#[diagnostic(
		code(spcasm::reference::missing_global),
		help("Add a global label before defining this local label"),
		severity(Error)
	)]
	MissingGlobalLabel {
		local_label: String,
		#[source_code]
		src:         Arc<AssemblyCode>,
		#[label("Local label defined here")]
		location:    SourceSpan,
	},

	#[error("{start} is greater than {end}")]
	#[diagnostic(
		code(spcasm::directive::invalid_range),
		help("Switch the range limits around: `{end}-{start}`"),
		severity(Error)
	)]
	StartAboveEnd {
		start:    usize,
		end:      usize,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("In this range")]
		location: SourceSpan,
	},

	#[error("The range {start}-{end} is out of bounds for the input file \"{file}\"")]
	#[diagnostic(
		code(spcasm::directive::range_out_of_bounds),
		help("The input's length is {file_len}"),
		severity(Error)
	)]
	RangeOutOfBounds {
		start:    usize,
		end:      usize,
		file:     String,
		file_len: usize,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Out of bounds range defined here")]
		location: SourceSpan,
	},

	#[error("Expected {expected}")]
	#[diagnostic(code(spcasm::syntax::expected_token), severity(Error))]
	ExpectedToken {
		expected: TokenOrString,
		actual:   Token,
		#[label("This {actual} is invalid here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Expected any of {}", expected.iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(", "))]
	#[diagnostic(code(spcasm::syntax::expected_token), severity(Error))]
	ExpectedTokens {
		expected: Vec<TokenOrString>,
		actual:   Token,
		#[label("This {actual} is invalid here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Invalid number: {error}")]
	#[diagnostic(code(spcasm::syntax::invalid_number), severity(Error))]
	InvalidNumber {
		error:    ParseIntError,
		#[label("{error}")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Expected any of {}", expected.iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(", "))]
	#[diagnostic(code(spcasm::syntax::missing_token), severity(Error))]
	UnexpectedEndOfTokens {
		expected: Vec<TokenOrString>,
		#[label("There should be a token here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Unexpected character {chr}")]
	#[diagnostic(code(spcasm::syntax::unexpected_character), severity(Error))]
	UnexpectedCharacter {
		chr:      char,
		#[label("Unexpected")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Invalid bit index `{index}`")]
	#[diagnostic(
		code(spcasm::syntax::invalid_bit_index),
		help("Use a bit index between 0 and 7 inclusive"),
		severity(Error)
	)]
	InvalidBitIndex {
		index:    u8,
		#[label("Bit index is invalid")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("There's dangling tokens after this")]
	#[diagnostic(code(spcasm::syntax::dangling_tokens), help("Remove these tokens"), severity(Error))]
	DanglingTokens {
		#[label("Dangling tokens here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[cfg(test)]
	#[error("Test comment has invalid format")]
	#[diagnostic(
		code(spcasm::syntax::invalid_test_comment),
		help(
			"Test comments consist of a series of space-delimited bytes, given as hexadecimal, for example `;= 0F AA \
			 B8` for three bytes"
		),
		severity(Error)
	)]
	InvalidTestComment {
		#[label("This ';=' comment is invalid: {}", basis.clone().or_else(|| "".parse::<u8>().err()).unwrap())]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
		basis:    Option<ParseIntError>,
	},

	#[error(
		"The value {value:02X} is being used as a {size}-bit operand here, but it is larger than this. The extra \
		 upper bits are truncated."
	)]
	#[diagnostic(code(spcasm::value_too_large), help("Remove these upper bits"), severity(Warning))]
	ValueTooLarge {
		value:    MemoryAddress,
		size:     u8,
		#[label("{size}-bit operand")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("This reference \"{name}\" has an 8-bit value, did you want to use it in direct page addressing?")]
	#[diagnostic(
		code(spcasm::reference::non_direct_page),
		help("Use a forced direct page addressing mnemonic by suffixing `.b`"),
		severity(Advice)
	)]
	NonDirectPageReference {
		name:                 String,
		address:              MemoryAddress,
		#[label("Might point at a direct page address")]
		reference_definition: SourceSpan,
		#[label("Memory address {address:02X}")]
		usage_location:       SourceSpan,
		#[source_code]
		src:                  Arc<AssemblyCode>,
	},
}

impl AssemblyError {
	pub(crate) fn from_lalrpop(error: ParseError<usize, Token, Self>, src: Arc<AssemblyCode>) -> Self {
		match error {
			ParseError::InvalidToken { location } => Self::UnexpectedCharacter {
				chr: src.text.chars().nth(location).unwrap_or_default(),
				location: (location, 1).into(),
				src,
			},
			ParseError::UnrecognizedEOF { location, expected } => Self::UnexpectedEndOfTokens {
				expected: expected.into_iter().map(TokenOrString::String).collect(),
				location: (location, 1).into(),
				src,
			},
			ParseError::UnrecognizedToken { token: (start, token, end), expected } if expected.len() > 1 =>
				Self::ExpectedTokens {
					expected: expected.into_iter().map(TokenOrString::String).collect(),
					actual: token,
					location: (start, end - start).into(),
					src,
				},
			ParseError::UnrecognizedToken { token: (start, token, end), expected } if expected.len() == 1 =>
				Self::ExpectedToken {
					expected: expected[0].clone().into(),
					actual: token,
					location: (start, end - start).into(),
					src,
				},
			ParseError::UnrecognizedToken { .. } => unreachable!(),
			ParseError::ExtraToken { token: (start, _, end) } =>
				Self::DanglingTokens { location: (start, end - start).into(), src },
			ParseError::User { error } => error,
		}
	}

	/// Report or throw this warning (or error), depending on what the user specified on the command line. On non-clap
	/// builds, this always reports the error.
	#[allow(clippy::trivially_copy_pass_by_ref)]
	pub(crate) fn report_or_throw(self, options: &dyn BackendOptions) -> Result<(), Box<Self>> {
		if options.is_error(&self) {
			return Err(self.into());
		} else if !options.is_ignored(&self) {
			println!("{:?}", miette::Report::new(self));
		}
		Ok(())
	}

	pub(crate) fn from_number_error(location: SourceSpan, src: Arc<AssemblyCode>) -> Self {
		// HACK: Create an integer parsing error that looks somewhat like the error which integer conversion would give.
		Self::InvalidNumber {
			error: format!("{}", usize::MAX as u128 + 1).parse::<usize>().unwrap_err(),
			location,
			src,
		}
	}
}

impl From<Box<Self>> for AssemblyError {
	fn from(boxed: Box<Self>) -> Self {
		*boxed
	}
}

#[derive(Clone, Debug)]
pub enum TokenOrString {
	Token(Token),
	String(String),
}

impl Display for TokenOrString {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", match self {
			Self::Token(token) => format!("{}", token),
			Self::String(string) => string.clone(),
		})
	}
}

impl From<String> for TokenOrString {
	fn from(string: String) -> Self {
		Self::String(string)
	}
}

impl From<&str> for TokenOrString {
	fn from(string: &str) -> Self {
		Self::String(string.to_owned())
	}
}

impl From<Token> for TokenOrString {
	fn from(string: Token) -> Self {
		Self::Token(string)
	}
}
