#![deny(clippy::all, clippy::pedantic, clippy::nursery)]

use std::collections::HashMap;
use std::fmt::Display;
use std::mem::Discriminant;
use std::num::ParseIntError;
use std::path::PathBuf;
use std::sync::Arc;

use lalrpop_util::ParseError;
use miette::{Diagnostic, MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use spcasm_derive::ErrorCodes;
use thiserror::Error;

use crate::cli::ErrorOptions;
use crate::mcro::MacroSymbol;
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
	/// # Errors
	/// If reading the file fails (doesn't exist, permissions wrong, I/O error etc.)
	pub fn from_file(filename: &str) -> Result<Arc<Self>, std::io::Error> {
		let mut path = PathBuf::from(filename);
		if path.is_relative() {
			path = std::env::current_dir()?.join(path);
		}
		path = path.canonicalize()?;
		let contents = std::fs::read_to_string(&path)?;
		Ok(Arc::new(Self { name: path, text: contents, include_path: Vec::new() }))
	}

	/// Returns a copy of the file name of this source code.
	#[must_use]
	pub fn file_name(&self) -> String {
		self.name.as_os_str().to_string_lossy().to_string()
	}
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

	#[error("Legal architecture macro ignored")]
	#[diagnostic(
		code(spcasm::valid_arch_macro),
		severity(Advice),
		help(
			"spcasm supports `arch` macros for compatibility with the Asar multi-architecture assembler. This arch \
			 directive points to the spc700 architecture and is therefore safely ignored."
		)
	)]
	ArchitectureMacroIgnored {
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("`arch` macro")]
		location: SourceSpan,
	},

	#[error("Unknown architecture `{arch}` specified")]
	#[diagnostic(
		code(spcasm::invalid_arch_macro),
		severity(Error),
		help(
			"spcasm supports `arch` macros for compatibility with the Asar multi-architecture assembler. This macro \
			 specifies that the architecture of the assembly source is not (completely) SPC700, therefore spcasm \
			 cannot assemble this file."
		)
	)]
	InvalidArchitectureMacro {
		arch:     String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("`arch` macro")]
		location: SourceSpan,
	},

	//#region Semantic errors: detected while parsing or assembling
	#[error("File \"{file_name}\" was not found")]
	#[diagnostic(code(spcasm::file_not_found), severity(Error))]
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
			"The file {cycle_trigger_file} was included:\n{}", src.include_path.iter().map(|path| format!("from {}", path.to_string_lossy())).intersperse("\n".to_string()).collect::<String>()
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
		code(spcasm::audio_processing_error),
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

	#[error("Invalid addressing mode `{mode}` as {} operand for `{mnemonic}`", if *.is_first_operand { "first" } else { "second" })]
	#[diagnostic(
		code(spcasm::invalid_addressing_mode),
		severity(Error),
		help("The instruction `{mnemonic}` accepts the modes {} here", 
			.legal_modes.iter().map(|mode| format!("{}, ", mode)).collect::<String>().strip_suffix(", ").unwrap()),
	)]
	InvalidAddressingMode {
		mode:             String,
		is_first_operand: bool,
		mnemonic:         Mnemonic,
		legal_modes:      Vec<String>,
		#[source_code]
		src:              Arc<AssemblyCode>,
		#[label("For this instruction")]
		location:         SourceSpan,
	},

	#[error("Segment at {segment_start:04x} starts before the end of the previous one, which is {segment_end:04x}")]
	#[diagnostic(code(spcasm::segment_mismatch), severity(Error))]
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
		code(spcasm::missing_segment),
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
		code(spcasm::empty_segment_stack),
		severity(Error),
		help("Macros like `pullpc` require that you push a segment to the stack beforehand with `pushpc`.")
	)]
	NoSegmentOnStack {
		#[label("Segment stack access here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Label '{label}' can not be resolved to a value")]
	#[diagnostic(
		code(spcasm::unresolved_label),
		severity(Error),
		help("Any symbolic label must be defined somewhere. Did you misspell the label's name?")
	)]
	UnresolvedLabel {
		label:          String,
		#[label("'{label}' defined here")]
		label_location: SourceSpan,
		#[label("Used here")]
		usage_location: SourceSpan,
		#[source_code]
		src:            Arc<AssemblyCode>,
	},

	#[error("Invalid addressing mode combination: `{first_mode}` with `{second_mode}` for `{mnemonic}`")]
	#[diagnostic(code(spcasm::invalid_addressing_mode_combination), severity(Error))]
	InvalidAddressingModeCombination {
		first_mode:  String,
		second_mode: String,
		mnemonic:    Mnemonic,
		#[source_code]
		src:         Arc<AssemblyCode>,
		#[label("Addressing mode invalid")]
		location:    SourceSpan,
	},

	#[error("Two operands are not allowed for `{mnemonic}`")]
	#[diagnostic(code(spcasm::two_operands_not_allowed), help("Remove the second operand"), severity(Error))]
	TwoOperandsNotAllowed {
		mnemonic: Mnemonic,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Only takes 1 operand")]
		location: SourceSpan,
	},

	#[error("`{mnemonic}` doesn't take any operands")]
	#[diagnostic(code(spcasm::operand_not_allowed), help("Remove the operands of this instruction"), severity(Error))]
	OperandNotAllowed {
		mnemonic: Mnemonic,
		#[label("Takes 0 operands")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("`{mnemonic}` takes at least one operand")]
	#[diagnostic(code(spcasm::missing_operand), help("Add an operand to this instruction"), severity(Error))]
	MissingOperand {
		mnemonic: Mnemonic,
		#[label("Takes at least one operand")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("`{constant}` is not valid for {typename}")]
	#[diagnostic(code(spcasm::invalid_constant), help("Remove the operands of this instruction"), severity(Error))]
	InvalidConstant {
		constant: String,
		typename: String,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Takes 0 operands")]
		location: SourceSpan,
	},

	#[error("Invalid use of labels in an argument for `{mcro}`")]
	#[diagnostic(
		code(spcasm::labels_in_macro_argument),
		help(
			"Because the macro argument can determine a label's position, resolving the argument value is not \
			 generally possible. For this reason, labels are not allowed to be used in a macro argument."
		)
	)]
	LabelsInMacroArgument {
		mcro:     MacroSymbol,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("This macro")]
		location: SourceSpan,
		// TODO: reintroduce when numbers have source locations
		// #[label("This macro argument")]
		// argument_location: SourceSpan,
	},

	#[error("There is no global label defined before the local label '{local_label}'")]
	#[diagnostic(
		code(spcasm::missing_global),
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
		code(spcasm::invalid_range),
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
	#[diagnostic(code(spcasm::range_out_of_bounds), help("The input's length is {file_len}"), severity(Error))]
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

	#[error("This label \"{name}\" has an 8-bit value, did you want to use it in direct page addressing?")]
	#[diagnostic(
		code(spcasm::non_direct_page_label),
		help("Use a forced direct page addressing mnemonic by suffixing `.b`"),
		severity(Advice)
	)]
	NonDirectPageLabel {
		name:             String,
		address:          MemoryAddress,
		#[label("Might point at a direct page address")]
		label_definition: SourceSpan,
		#[label("Memory address {address:02X}")]
		location:         SourceSpan,
		#[source_code]
		src:              Arc<AssemblyCode>,
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
			ParseError::UnrecognizedToken { token: (start, token, end), expected } => unreachable!(),
			ParseError::ExtraToken { token: (start, _, end) } =>
				Self::DanglingTokens { location: (start, end - start).into(), src },
			ParseError::User { error } => error,
		}
	}

	/// Report or throw this warning (or error), depending on what the user specified on the command line. On non-clap
	/// builds, this always reports the error.
	#[allow(clippy::trivially_copy_pass_by_ref)]
	pub(crate) fn report_or_throw(self, options: &ErrorOptions) -> Result<(), Box<Self>> {
		// Always rethrow errors.
		if self.severity().is_some_and(|s| s == &miette::Severity::Error) {
			return Err(self.into());
		}
		#[cfg(feature = "clap")]
		{
			let discriminant = std::mem::discriminant(&self);
			if options.error.contains(&discriminant.into()) {
				return Err(self.into());
			} else if !options.ignore.contains(&discriminant.into()) {
				println!("{:?}", miette::Report::new(self));
			}
			Ok(())
		}
		#[cfg(not(feature = "clap"))]
		{
			println!("{:?}", miette::Report::new(self));
			Ok(())
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
