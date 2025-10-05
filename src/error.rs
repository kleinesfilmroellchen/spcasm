use std::collections::HashMap;
use std::fmt::Write;
use std::mem::Discriminant;
use std::num::ParseIntError;
use std::sync::Arc;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use lalrpop_util::ParseError;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::AssemblyCode;
use crate::directive::DirectiveSymbol;
use crate::parser::Token;
use crate::sema::AssemblyTimeValue;
use crate::sema::instruction::{MemoryAddress, Mnemonic};
use crate::sema::reference::Reference;

#[allow(clippy::module_name_repetitions)]
#[allow(unused)]
pub trait ErrorCodes {
	fn all_codes() -> HashMap<Discriminant<AssemblyError>, SharedStr>;
}

macro_rules! ErrorCodes {
	derive() (
		$(#[$_m:meta])*
		$_p:vis enum $Enum:ident {
		$(
			$(#[$_v:meta])*
			$variant_name:ident {
				$(
					$(#[$_f:meta])*
					$field_name:ident : $_t:ty
				),* $(,)?
			}
		),* $(,)?
	}) => {
		#[automatically_derived]
		impl ErrorCodes for $Enum {
			fn all_codes() -> HashMap<Discriminant<AssemblyError>, SharedStr> {
				let mut map = std::collections::HashMap::new();
				$(
					let error_instance = &Self::$variant_name {
						$( $field_name: crate::default_hacks::FakeDefaultForIgnoredValues::default() ),*
					};
					map.insert(std::mem::discriminant(error_instance), error_instance.code().expect("error must have a code").to_string().into());
				)*
				return map;
			}
		}
	};
}

/// All types of errors that the assembler can report to the user.
#[derive(Error, Debug, Clone, Diagnostic, ErrorCodes)]
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

	#[error("Duplicate startpos directive")]
	#[diagnostic(
		code(spcasm::directive::duplicate_startpos),
		severity(Error),
		help(
			"the `startpos` directive defines the execution entry point of the ROM after it was loaded. There can \
			 only be one entry point."
		)
	)]
	DuplicateStartpos {
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("`startpos` directive")]
		location: SourceSpan,
	},

	#[error("Missing startpos directive")]
	#[diagnostic(
		code(spcasm::directive::missing_startpos),
		severity(Error),
		help(
			"the `startpos` directive defines the execution entry point of the ROM after it was loaded. This is \
			 required for ELF binary output."
		)
	)]
	MissingStartpos {
		#[source_code]
		src: Arc<AssemblyCode>,
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
		arch:     SharedStr,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("`arch` directive")]
		location: SourceSpan,
	},

	#[error("`math pri` is not supported by spcasm")]
	#[diagnostic(
		code(spcasm::directive::math_pri_unsupported),
		severity(Error),
		help(
			"This directive switches between normal operator precedence and \"dumb\" left-to-right precedence in the \
			 Asar multi-architecture assembler. This feature is intended for xkas compatibility, but spcasm does not \
			 consider this an important feature. Therefore, spcasm cannot switch math priority mode. Please check \
			 your math expressions manually if you are using `math pri off`."
		)
	)]
	MathPriForbidden {
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("`math pri` directive")]
		location: SourceSpan,
	},

	#[error("Assigning a value to {} '{name}' is not possible", .kind.name())]
	#[diagnostic(
		code(spcasm::reference::assign_invalid),
		severity(Error),
		help("{}", match .kind {
				UnassignableReferenceType::MacroArgument => "Arguments of macros are given a value when the macro is called. Therefore, it does not make sense to \
				                                 assign them a value. If you need a label with a specific value inside a macro, use a local label under \
				                                 the macro's special '\\@' label instead",
				UnassignableReferenceType::MacroGlobal => "The special macro label '\\@' can be used for creating a unique global label per user macro call. It can \
				                               therefore only be assigned a value by using it as the label for an instruction. If you need to compute \
				                               values based on macro arguments, there is currently no non-repetitive way to do this.",
			}
		)
	)]
	AssigningToReference {
		name:     SharedStr,
		kind:     UnassignableReferenceType,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Assignment happens here")]
		location: SourceSpan,
	},

	#[error("Using the user defined macro argument '<{name}>' outside a macro is not possible")]
	#[diagnostic(code(spcasm::user_macro::argument_outside_macro), severity(Error))]
	UsingMacroArgumentOutsideMacro {
		name:     SharedStr,
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
		name:     SharedStr,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Inner macro defined here")]
		location: SourceSpan,
		#[label("Outer macro defined here")]
		outer:    SourceSpan,
	},

	#[error("Maximum recursion depth {depth} was exceeded while expanding '{name}'")]
	#[diagnostic(
		code(spcasm::directive::recursion_limit),
		severity(Error),
		help(
			"This is most likely caused by an infinite recursion in a macro calling itself. On the command line, use \
			 `--macro-recursion-limit` to increase the limit."
		)
	)]
	DirectiveRecursionLimit {
		name:     SharedStr,
		depth:    usize,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("While trying to expand this")]
		location: SourceSpan,
	},

	#[error("Macro argument '{name}' has not been defined in this macro")]
	#[diagnostic(
		code(spcasm::user_macro::undefined_argument),
		severity(Error),
		help("The available arguments are: {}. Did you misspell the macro argument's name?",
			available_names.iter().map(|name| format!("'{name}'")).collect::<Vec<_>>().join(", "))
	)]
	UnknownMacroArgument {
		name:            SharedStr,
		available_names: Vec<SharedStr>,
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
			available_macros.iter().map(|name| format!("'{name}'")).collect::<Vec<_>>().join(", "))
	)]
	UndefinedUserMacro {
		name:             SharedStr,
		available_macros: Vec<SharedStr>,
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
		name:            SharedStr,
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
		/// `std::io::Error` is not clonable for performance and implementation detail reasons[rustissue].
		///
		/// [rustissue]: <https://github.com/rust-lang/rust/issues/24135>
		#[source]
		os_error:  Arc<std::io::Error>,
		file_name: SharedStr,
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
		cycle_trigger_file: SharedStr,
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
		error_text: SharedStr,
		file_name:  SharedStr,
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

	#[error("'{reference}' was defined outside a segment and will have no value")]
	#[diagnostic(
		code(spcasm::reference::outside_segment),
		severity(Warning),
		help("If you want this reference to have a value, start a segment before it with `org <memory address>`.")
	)]
	ReferenceOutsideSegment {
		reference: SharedStr,
		#[label("Reference defined here, outside segments")]
		location:  SourceSpan,
		#[source_code]
		src:       Arc<AssemblyCode>,
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
		reference:          SharedStr,
		#[label("'{reference}' defined here")]
		reference_location: Option<SourceSpan>,
		#[label("Used here")]
		usage_location:     SourceSpan,
		#[source_code]
		src:                Arc<AssemblyCode>,
	},

	#[error("Reference '{reference}' was defined more than once")]
	#[diagnostic(
		code(spcasm::reference::redefine),
		severity(Error),
		help("Local references are a convenient way to safely reuse common names like '.loop' or '.end'.")
	)]
	RedefinedReference {
		reference:          SharedStr,
		#[label("'{reference}' first defined here…")]
		reference_location: SourceSpan,
		#[label("… and later redefined here")]
		redefine_location:  SourceSpan,
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

	#[error("'repeatcount' can not be resolved to a value")]
	#[diagnostic(
		code(spcasm::reference::unresolved),
		severity(Error),
		help("The special symbol 'repeatcount' is only usable inside repeat directives.")
	)]
	RepeatCountOutsideRepeat {
		#[label("Used here, outside of a repeat directive")]
		usage_location: SourceSpan,
		#[source_code]
		src:            Arc<AssemblyCode>,
	},

	#[error("Repetition count is negative and will be clamped to zero")]
	#[diagnostic(
		code(spcasm::directive::negative_repeatcount),
		severity(Warning),
		help("Use repeat count zero to not repeat this block at all")
	)]
	NegativeRepeatCount {
		#[label("Repeat count resolves to {resolved_repeatcount}")]
		repeatcount_location: SourceSpan,
		resolved_repeatcount: MemoryAddress,
		#[label("While expanding this repeat directive")]
		directive_location:   SourceSpan,
		#[source_code]
		src:                  Arc<AssemblyCode>,
	},

	#[error("Invalid addressing mode `{mode}` as first operand for `{mnemonic}`")]
	#[diagnostic(
		code(spcasm::instruction::invalid_addressing_mode),
		severity(Error),
		help("The instruction `{mnemonic}` accepts the modes {} as first operands", 
			.legal_modes.iter().fold(String::new(), |mut output, mode| { let _ = write!(output, "{mode}, "); output }).strip_suffix(", ").unwrap_or_default()),
	)]
	InvalidFirstAddressingMode {
		mode:        SharedStr,
		mnemonic:    Mnemonic,
		legal_modes: Vec<SharedStr>,
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
			.legal_modes.iter().fold(String::new(), |mut output, mode| { let _ = write!(output, "{mode}, "); output }).strip_suffix(", ").unwrap_or_default()),
	)]
	InvalidSecondAddressingMode {
		mode:        SharedStr,
		mnemonic:    Mnemonic,
		first_mode:  SharedStr,
		legal_modes: Vec<SharedStr>,
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
			.legal_modes.iter().fold(String::new(), |mut output, mode| { let _ = write!(output, "{mode}, "); output }).strip_suffix(", ").unwrap_or_default()),
		severity(Error)
	)]
	MissingOperand {
		mnemonic:    Mnemonic,
		legal_modes: Vec<SharedStr>,
		#[label("Takes at least one operand")]
		location:    SourceSpan,
		#[source_code]
		src:         Arc<AssemblyCode>,
	},

	#[error("`{mnemonic}` takes two operands")]
	#[diagnostic(
		code(spcasm::instruction::missing_second_operand),
		help("Add any of the operands {} to this instruction",
			.legal_modes.iter().fold(String::new(), |mut output, mode| { let _ = write!(output, "{mode}, "); output }).strip_suffix(", ").unwrap_or_default()),
		severity(Error)
	)]
	MissingSecondOperand {
		mnemonic:    Mnemonic,
		legal_modes: Vec<SharedStr>,
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
		constant: SharedStr,
		typename: SharedStr,
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
		),
		severity(Error)
	)]
	ReferencesInDirectiveArgument {
		directive:         DirectiveSymbol,
		#[source_code]
		src:               Arc<AssemblyCode>,
		#[label("This directive")]
		location:          SourceSpan,
		#[label("This directive argument")]
		argument_location: SourceSpan,
	},

	#[error("There is no global label defined before the local label '{local_label}'")]
	#[diagnostic(
		code(spcasm::reference::missing_global),
		help("Add a global label before defining this local label."),
		severity(Error)
	)]
	MissingGlobalLabel {
		local_label: SharedStr,
		#[source_code]
		src:         Arc<AssemblyCode>,
		#[label("Local label defined here")]
		location:    SourceSpan,
	},

	#[error("{start} is greater than {end}")]
	#[diagnostic(
		code(spcasm::directive::invalid_range),
		help("Switch the range limits around: `{end}-{start}`."),
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
		help("The input's length is {file_len}."),
		severity(Error)
	)]
	RangeOutOfBounds {
		start:    usize,
		end:      usize,
		file:     SharedStr,
		file_len: usize,
		#[source_code]
		src:      Arc<AssemblyCode>,
		#[label("Out of bounds range defined here")]
		location: SourceSpan,
	},

	#[error("Invalid option `{option}` for directive `{directive}`")]
	#[diagnostic(
		code(spcasm::directive::invalid_directive_option),
		help("The valid options are {}.",
			.valid_options.iter().fold(String::new(), |mut output, option| { let _ = write!(output, "`{option}`, "); output }).strip_suffix(", ").unwrap_or_default()),
		severity(Error)
	)]
	InvalidDirectiveOption {
		option:             SharedStr,
		directive:          SharedStr,
		valid_options:      Vec<SharedStr>,
		#[source_code]
		src:                Arc<AssemblyCode>,
		#[label("`{directive}` directive defined here")]
		directive_location: SourceSpan,
		#[label("Invalid option specified here")]
		option_location:    SourceSpan,
	},

	#[error("No value specified for `{operation}`")]
	#[diagnostic(
		code(spcasm::directive::missing_fill_pad_parameter),
		help("`{operation}` needs a value, which is specified separately via another directive like `{}`. Such a directive was not found.",
			if *.is_fill { "fillbyte" } else { "padbyte" }),
		severity(Error)
	)]
	MissingFillParameter {
		operation: SharedStr,
		is_fill:   bool,
		#[label("`{operation}` defined here")]
		location:  SourceSpan,
		#[source_code]
		src:       Arc<AssemblyCode>,
	},

	#[error("BRR sample table has more than 256 entries")]
	#[diagnostic(
		code(spcasm::directive::sample_table_too_large),
		help("BRR sample tables cannot contain more than 256 entries. The autogenerated table contains all BRR samples without the `nodirectory` option, which includes {entry_count} samples in this case. To reduce the number of samples in the table, use the `nodirectory` option for at least {} `brr` directives.", entry_count - 256),
		severity(Error)
	)]
	SampleTableTooLarge {
		entry_count:        usize,
		#[source_code]
		src:                Arc<AssemblyCode>,
		#[label("automatic sample table generated here with {entry_count} entries")]
		directive_location: SourceSpan,
	},

	#[error("Sample table at address `{memory_address:04X}` is not correctly aligned")]
	#[diagnostic(
		code(spcasm::directive::unaligned_sample_table),
		help(
			"The BRR sample table must be aligned to 256-byte-pages (lowest eight bits of address are zero) in order \
			 to be properly usable by hardware. To automatically perform this alignment, remove the `noalign` option."
		),
		severity(Error)
	)]
	UnalignedSampleTable {
		memory_address: MemoryAddress,
		#[source_code]
		src:            Arc<AssemblyCode>,
		#[label("automatic sample table generated at address {memory_address:04X}")]
		location:       SourceSpan,
	},

	#[error("Expected \"{expected}\"")]
	#[diagnostic(code(spcasm::syntax::expected_token), severity(Error))]
	ExpectedToken {
		expected: SharedStr,
		actual:   Token,
		#[label("This {actual} is invalid here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Expected any of {}", expected.iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(", "))]
	#[diagnostic(code(spcasm::syntax::expected_token), severity(Error))]
	ExpectedTokens {
		expected: Vec<SharedStr>,
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

	#[error("\"{text}\" was expected to be a number, but it is parsed as an identifier instead")]
	#[diagnostic(
		code(spcasm::syntax::number_identifier),
		severity(Warning),
		help(
			"Identifiers starting with numbers is an Asar compatibility feature, but it is not recommended since it \
			 can lead to weird errors later on."
		)
	)]
	NumberIdentifier {
		text:     SharedStr,
		error:    ParseIntError,
		#[label("Parse error: {error}")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("An implicit default bit index of 1 will be used for the `{mnemonic}` instruction")]
	#[diagnostic(
		code(spcasm::instruction::implicit_bit_index),
		severity(Warning),
		help(
			"Implicit bit indices are an Asar compatibility feature, but it is not recommended since the default of 1 \
			 may be surprising."
		)
	)]
	ImplicitBitIndex {
		mnemonic: Mnemonic,
		#[label("Should contain a bit index")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Expected any of {}", expected.iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(", "))]
	#[diagnostic(code(spcasm::syntax::missing_token), severity(Error))]
	UnexpectedEndOfTokens {
		expected: Vec<SharedStr>,
		#[label("There should be a token here")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("Unexpected character \"{chr}\"")]
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
		#[label("This ';=' comment is invalid: {basis}")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
		basis:    ParseIntError,
	},

	#[error("`{value:04X}` cannot be resolved to a static value")]
	#[diagnostic(
		code(spcasm::unresolvable),
		severity(Error),
		help("This error is not caused by a reference, but no further information is available.")
	)]
	UnresolvableValue {
		value:          AssemblyTimeValue,
		#[label("`{value:04X}` used here")]
		value_location: SourceSpan,
		#[source_code]
		src:            Arc<AssemblyCode>,
	},

	#[error(
		"The value `{value:02X}` is being used as a {size}-bit operand here, but it is larger than this. The extra \
		 upper bits are truncated."
	)]
	#[diagnostic(
		code(spcasm::value_too_large),
		help("If this was intentional, explicitly truncate the value."),
		severity(Warning)
	)]
	ValueTooLarge {
		value:    MemoryAddress,
		size:     u8,
		#[label("{size}-bit operand")]
		location: SourceSpan,
		#[source_code]
		src:      Arc<AssemblyCode>,
	},

	#[error("The relative offset to address `{target:04X}` is out of range, the result will be wrong.")]
	#[diagnostic(
		code(spcasm::relative_offset_too_large),
		help(
			"The current address is `{address:04X}` and therefore the difference is {}. This difference exceeds the \
			 range [-128, 127] and will wrap around, probably leading to incorrect code. If you are using a relative \
			 jump, consider using a trampoline to cover larger jump distances.", target-(address+1)
		),
		severity(Warning)
	)]
	RelativeOffsetTooLarge {
		target:          MemoryAddress,
		address:         MemoryAddress,
		#[label("Difference of {} to current address", target-(address+1))]
		location:        SourceSpan,
		#[label("Target address `{target:04X}`")]
		target_location: Option<SourceSpan>,
		#[source_code]
		src:             Arc<AssemblyCode>,
	},

	#[error("This reference '{name}' has an 8-bit value, did you want to use it in direct page addressing?")]
	#[diagnostic(
		code(spcasm::reference::non_direct_page),
		help("Use a forced direct page addressing mnemonic by suffixing `.b`"),
		severity(Advice)
	)]
	NonDirectPageReference {
		name:                 SharedStr,
		address:              MemoryAddress,
		#[label("Might point at a direct page address")]
		reference_definition: SourceSpan,
		#[label("Memory address {address:02X}")]
		usage_location:       SourceSpan,
		#[source_code]
		src:                  Arc<AssemblyCode>,
	},

	#[error(transparent)]
	#[diagnostic(code(spcasm::io_error), severity(Error))]
	OtherIoError {
		#[from]
		inner: Arc<std::io::Error>,
	},
}

impl AssemblyError {
	pub(crate) fn from_lalrpop(error: ParseError<usize, Token, Self>, src: Arc<AssemblyCode>) -> Self {
		match error {
			ParseError::UnrecognizedEof { location, expected } => Self::UnexpectedEndOfTokens {
				expected: expected.into_iter().map(SharedStr::from).collect(),
				location: (location, 1).into(),
				src,
			},
			ParseError::UnrecognizedToken { token: (start, token, end), expected } if expected.len() > 1 =>
				Self::ExpectedTokens {
					expected: expected.into_iter().map(SharedStr::from).collect(),
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
			ParseError::InvalidToken { .. } => unreachable!("not using the internal lexer"),
			ParseError::UnrecognizedToken { token: (start, token, end), .. } =>
				Self::ExpectedToken { expected: "".into(), actual: token, location: (start, end - start).into(), src },
			ParseError::ExtraToken { .. } => unreachable!("parser should not report extra tokens"),
			ParseError::User { error } => error,
		}
	}
}

impl From<Box<Self>> for AssemblyError {
	fn from(boxed: Box<Self>) -> Self {
		*boxed
	}
}

impl From<std::io::Error> for AssemblyError {
	fn from(value: std::io::Error) -> Self {
		Self::OtherIoError { inner: Arc::new(value) }
	}
}

/// Reference type for keeping track of what error text to print for different references in errors.
#[repr(u8)]
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum UnassignableReferenceType {
	#[default]
	MacroGlobal,
	MacroArgument,
}

impl UnassignableReferenceType {
	pub const fn name(self) -> &'static str {
		match self {
			Self::MacroGlobal => "macro global label",
			Self::MacroArgument => "macro argument",
		}
	}
}

impl TryInto<UnassignableReferenceType> for &Reference {
	type Error = ();

	fn try_into(self) -> Result<UnassignableReferenceType, Self::Error> {
		match self {
			Reference::Label(_)
			| Reference::Relative { .. }
			| Reference::RepeatCount { .. }
			| Reference::UnresolvedLabel { .. } => Err(()),
			Reference::MacroArgument { .. } => Ok(UnassignableReferenceType::MacroArgument),
			Reference::MacroGlobal { .. } => Ok(UnassignableReferenceType::MacroGlobal),
		}
	}
}
