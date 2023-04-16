extern crate test;
use std::cmp::min;
use std::sync::Arc;

use reqwest;
#[allow(unused)]
use smartstring::alias::String;
use test::Bencher;

use crate::cli::default_backend_options;
use crate::parser::instruction::MemoryAddress;
use crate::parser::ProgramElement;
use crate::{dump_reference_tree, pretty_hex, AssemblyError, Segments};

#[bench]
fn all_opcodes(bencher: &mut Bencher) {
	bencher.iter(|| test_file("tests/opcodes.s"));
}

#[test]
fn boot_rom() {
	test_file("include/bootrom.s");
}

#[test]
fn assembler() {
	let sources = std::fs::read_dir("tests").unwrap();
	for source in sources {
		let source = source.unwrap().path();
		let source = &*source.to_string_lossy();
		if source.ends_with(".spcasmtest") {
			println!("assembling {} ...", source);
			test_file(source);
		} else {
			println!("skipping file {} (not a test)", source);
		}
	}
}

#[test]
fn errors() {
	let error_sources = std::fs::read_dir("tests/errors").unwrap();
	for error_source in error_sources {
		let error_source = error_source.unwrap().path();
		let error_source = &*error_source.to_string_lossy();
		if error_source.ends_with(".spcasmtest") {
			let result = super::run_assembler_with_default_options(error_source);
			let _ = super::run_assembler_into_segments(
				&crate::AssemblyCode::from_file_or_assembly_error(error_source).unwrap(),
				default_backend_options(),
			);
			println!("checking {} for errors ...\n{:?}", error_source, result);
			assert!(result.is_err());
		} else {
			println!("skipping file {} (not an error test)", error_source);
		}
	}
}

#[bench]
fn brr_integration(bencher: &mut Bencher) {
	bencher.iter(|| test_file("tests/brr.spcasmtest"));
}

#[test]
fn clis() {
	trycmd::TestCases::new().case("tests/cli/*.trycmd");
}

#[test]
fn documented_cli() {
	trycmd::TestCases::new().case("doc/src/usage.md");
	trycmd::TestCases::new().case("README.md");
}

#[test]
fn documented_errors() {
	trycmd::TestCases::new().case("doc/src/errors.md");
}

#[test]
fn asar_opcode_test() -> Result<(), Box<AssemblyError>> {
	const test_file_url: &str = "https://raw.githubusercontent.com/RPGHacker/asar/master/tests/arch-spc700.asm";
	const file_name: &str = "asar-arch-spc700.asm";
	let maybe_test_file: Result<_, reqwest::Error> = try { reqwest::blocking::get(test_file_url)?.text()? };
	match maybe_test_file {
		Ok(test_file) => {
			let (_, mut assembled) = super::run_assembler_into_segments(
				&Arc::new(crate::AssemblyCode {
					name: file_name.into(),
					text: test_file.clone().into(),
					..Default::default()
				}),
				default_backend_options(),
			)?;
			let only_segment = assembled.segments.first_entry().expect("no segment present").remove();
			let expected_output = extract_asar_expected_output(&test_file).into_iter().map(Some).collect::<Vec<_>>();
			assert_segments_are_equal(0, &expected_output, 0, &only_segment, file_name);
		},
		Err(why) => {
			println!("Warning: couldn't download the Asar opcode test: {}", why);
		},
	}
	Ok(())
}

fn test_file(file: &str) {
	let (parsed, assembled) = super::run_assembler_into_segments(
		&crate::AssemblyCode::from_file_or_assembly_error(file).unwrap(),
		default_backend_options(),
	)
	.unwrap();
	let (environment, _) = super::run_assembler_with_default_options(file).unwrap();

	dump_reference_tree(&environment.read_recursive().globals);

	let expected_binary = assemble_expected_binary(parsed);
	for ((parsed_segment_start, expected_segment), (assembled_segment_start, assembled)) in
		expected_binary.segments.iter().zip(assembled.segments.iter())
	{
		assert_segments_are_equal(*parsed_segment_start, expected_segment, *assembled_segment_start, assembled, file);
	}
}

fn assert_segments_are_equal(
	parsed_segment_start: MemoryAddress,
	expected_segment: &[Option<u8>],
	assembled_segment_start: MemoryAddress,
	assembled: &Vec<u8>,
	file: &str,
) {
	assert_eq!(
		parsed_segment_start, assembled_segment_start,
		"Assembly and AST differ in segments; something has gone wrong!"
	);
	for (byte, (expected, actual)) in expected_segment.iter().zip(assembled.iter()).enumerate() {
		if let Some(expected) = expected {
			assert_eq!(
				expected,
				actual,
				"In file {} segment {:04X}: Expected and actual assembly differ at byte {:04X}:\n\texpected: \
				 {:02X}\n\tactual:   {:02X}\nhint: the bytes before and after are:\n\t{}",
				file,
				assembled_segment_start,
				byte as MemoryAddress + assembled_segment_start,
				expected,
				actual,
				pretty_hex(&assembled[byte.saturating_sub(4) .. min(assembled.len(), byte + 5)], Some(4))
			);
		}
	}
}

/// Assembles the contents of the expected value comments, which is what the file should assemble to.
fn assemble_expected_binary(instructions: Segments<ProgramElement>) -> Segments<Option<u8>> {
	instructions
		.try_map_segments(|_, program_elements| {
			Ok::<_, ()>(
				program_elements
					.into_iter()
					.flat_map(|program_element| {
						match program_element {
							ProgramElement::Instruction(ref instruction) => instruction.expected_value.clone(),
							ProgramElement::Directive(ref directive) => directive.expected_value.clone(),
							_ => None,
						}
						.map_or_else(
							|| vec![None; program_element.assembled_size()],
							|value| value.iter().map(|b| Some(*b)).collect(),
						)
					})
					.collect(),
			)
		})
		.unwrap() // safe because we can never fail in the mapper function
}

fn extract_asar_expected_output(file: &str) -> Vec<u8> {
	let mut expected_output = Vec::new();
	for line in file.lines() {
		if let Some(expected_outputs) = line.strip_prefix(";`") {
			expected_output.append(
				&mut expected_outputs
					.split_whitespace()
					.map(|byte_string| u8::from_str_radix(byte_string, 16))
					.try_collect()
					.unwrap(),
			);
		} else {
			break;
		}
	}

	expected_output
}

#[test]
#[allow(clippy::redundant_clone, clippy::too_many_lines)]
fn coverage() {
	use std::collections::BTreeMap;
	use std::sync::Weak;

	use crate::default_hacks::FakeDefaultForIgnoredValues;
	use crate::parser::value::BinaryOperator;
	use crate::parser::Token;

	<i64 as FakeDefaultForIgnoredValues>::default();
	miette::SourceSpan::default();
	std::io::Error::default();
	std::num::ParseIntError::default();
	crate::parser::Token::default();
	crate::directive::DirectiveSymbol::default();
	crate::parser::instruction::Mnemonic::default();
	crate::error::TokenOrString::default();

	let code = crate::AssemblyCode::new("\r\n", &"hello".into()).clone();
	println!("{:?}", code);
	assert!(crate::AssemblyCode::from_file_or_assembly_error("does-not-exist").is_err());
	assert!(crate::AssemblyCode::from_file_or_assembly_error("/does-not-exist").is_err());
	let _ = crate::AssemblyCode::file_name_for(std::path::Path::new("C:/Some/Totally/Nonexistent/Path"));

	let _ = format!(
		"{:X}, {:X}, {1:?}",
		crate::parser::AssemblyTimeValue::from(34),
		crate::parser::AssemblyTimeValue::BinaryOperation(Box::new(32.into()), Box::new(7.into()), BinaryOperator::And,),
	);

	let label = crate::parser::reference::Reference::Label(std::sync::Arc::new(
		crate::parser::reference::Label {
			children:        BTreeMap::new(),
			location:        None,
			name:            "example".into(),
			used_as_address: true,
			span:            (0, 0).into(),
			parent:          Weak::new(),
		}
		.into(),
	));
	let macro_parent = crate::parser::reference::MacroParent::new_formal(None, (0, 0).into());
	let macro_parameter = crate::parser::reference::Reference::MacroArgument {
		name:         "test".into(),
		value:        None,
		span:         (0, 0).into(),
		macro_parent: macro_parent.clone(),
	};

	format!("{}, {}, {0:?}, {1:?}, {2:?}", label, macro_parameter, macro_parent);
	let mut resolved_global = label.clone();
	resolved_global.set_location(7.into());
	let mut resolved_macro_parameter = macro_parameter.clone();
	resolved_macro_parameter.set_location(9.into());
	format!("{}, {}", resolved_global, resolved_macro_parameter);

	for operator in [
		BinaryOperator::Add,
		BinaryOperator::And,
		BinaryOperator::Divide,
		BinaryOperator::Exponentiation,
		BinaryOperator::LeftShift,
		BinaryOperator::RightShift,
		BinaryOperator::Modulus,
		BinaryOperator::Multiply,
		BinaryOperator::Or,
		BinaryOperator::RightShift,
		BinaryOperator::Subtract,
		BinaryOperator::Xor,
	] {
		let _ = format!(
			"{:X}",
			crate::parser::AssemblyTimeValue::BinaryOperation(
				Box::new(crate::parser::AssemblyTimeValue::Reference(label.clone())),
				Box::new(crate::parser::AssemblyTimeValue::Reference(label.clone())),
				operator,
			),
		);
	}

	assert_eq!(crate::parser::AssemblyTimeValue::from(34), crate::parser::AssemblyTimeValue::from(34));

	for token in [
		Token::Ampersand(0.into()),
		Token::Caret(0.into()),
		Token::CloseAngleBracket(0.into()),
		Token::CloseIndexingParenthesis(0.into()),
		Token::OpenIndexingParenthesis(0.into()),
		Token::OpenAngleBracket(0.into()),
		Token::OpenParenthesis(0.into()),
		Token::CloseParenthesis(0.into()),
		Token::Comma(0.into()),
		Token::Colon(0.into()),
		Token::DoubleCloseAngleBracket(0.into()),
		Token::DoubleOpenAngleBracket(0.into()),
		Token::DoubleStar(0.into()),
		Token::Directive(crate::directive::DirectiveSymbol::Arch, 0.into()),
		Token::Equals(0.into()),
		Token::ExplicitDirectPage(0.into()),
		Token::Hash(0.into()),
		Token::Identifier("something".into(), 0.into()),
		Token::Minus(0.into()),
		Token::Newline(0.into()),
		Token::Mnemonic(crate::parser::instruction::Mnemonic::Adc, 0.into()),
		Token::Percent(0.into()),
		Token::Period(0.into()),
		Token::Pipe(0.into()),
		Token::Plus(0.into()),
		Token::PlusRegister(crate::parser::Register::A, 0.into()),
		Token::Slash(0.into()),
		Token::Star(0.into()),
		Token::String(Vec::new(), 0.into()),
		Token::TestComment(Vec::new(), 0.into()),
		Token::Tilde(0.into()),
		Token::Number(0, "897".into(), 0.into()),
		Token::Register(crate::parser::Register::X, 0.into()),
	] {
		assert_eq!(token, token);
		assert_ne!(token, Token::TestComment(vec![5, 6, 7], 0.into()));
		let _ = format!("{0} {0:?}", token);
	}

	assert_eq!(crate::parser::LabelUsageKind::AsAddress, crate::parser::LabelUsageKind::AsAddress);
	let _ = format!(
		"{:?} {:?} {:?} {:?} {:?} {:?} {:?}",
		crate::Environment::new(),
		crate::parser::LabelUsageKind::AsAddress.clone(),
		crate::parser::AssemblyFile {
			tokens:      Vec::new(),
			content:     Vec::new(),
			parent:      std::sync::Weak::new(),
			source_code: code.into(),
		},
		crate::assembler::sample_table::SampleTable::default(),
		crate::assembler::sample_table::SampleTable::default()
			== crate::assembler::sample_table::SampleTable::default(),
		crate::assembler::sample_table::SampleEntry { start_address: 0.into() }.clone(),
		crate::assembler::sample_table::SampleEntry { start_address: 0.into() }.clone()
			== crate::assembler::sample_table::SampleEntry { start_address: 0.into() }.clone(),
	);

	assert_eq!(crate::parser::ProgramElement::Directive(crate::Directive::default()).span(), (0, 0).into());
	let macro_call = crate::parser::ProgramElement::UserDefinedMacroCall {
		macro_name: "".into(),
		arguments:  Vec::new(),
		span:       (0, 5).into(),
	};
	let include_source = crate::parser::ProgramElement::IncludeSource { file: "".into(), span: (0, 0).into() };
	format!("{:?} {:?}", macro_call, include_source);
	assert_eq!(macro_call.span(), (0, 5).into());
	assert_eq!(macro_call.assembled_size(), 0);
	assert_eq!(include_source.assembled_size(), 0);
}
