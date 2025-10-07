//! Semantics module unit tests.

use crate::parser::Token;
use crate::{AssemblyCode, run_assembler};

#[test]
fn file_token_reference_lookup() {
	let source = AssemblyCode::from_file("tests/references.spcasmtest").unwrap();
	let output = run_assembler(source.clone(), None).unwrap();
	let file = output.environment.read().find_file_by_source(&source).unwrap().unwrap();

	let mut offset_of_zero_definition = None;
	for i in 0 .. source.text.chars().count() {
		let token = file.read().token_at(i);
		if let Some(Token::Identifier(text, _)) = token
			&& text == "zero"
		{
			offset_of_zero_definition = Some(i);
			break;
		}
	}
	let offset_of_zero_definition = offset_of_zero_definition.unwrap();

	let reference = file.read().reference_at(offset_of_zero_definition).unwrap();
	assert_eq!(reference.name(), "zero");

	let all_zero_definitions = file.read().get_definition_spans_of("zero");
	assert_eq!(all_zero_definitions.len(), 1);
	let zero_definition = all_zero_definitions.first().unwrap();
	assert!(zero_definition.offset() <= offset_of_zero_definition);
	assert!(zero_definition.offset() + zero_definition.len() >= offset_of_zero_definition);

	assert!(!file.read().get_definition_spans_of("local_cool").is_empty());
	assert!(!file.read().get_definition_spans_of("thing").is_empty());
	assert!(!file.read().get_definition_spans_of("some_macro").is_empty());
}
