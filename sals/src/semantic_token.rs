//! Semantic token support of sals.
//! Semantic tokens are the main syntax highlighting mechanism of the SPC700 language support.

use std::ops::Deref;

use spcasm::parser::Token;
use tower_lsp::lsp_types::SemanticTokenType;

#[repr(transparent)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpcasmTokenType(SemanticTokenType);

pub const SEMANTIC_TOKEN_TYPES: [SpcasmTokenType; 10] = [
	SpcasmTokenType(SemanticTokenType::NUMBER),
	SpcasmTokenType(SemanticTokenType::PARAMETER),
	SpcasmTokenType(SemanticTokenType::VARIABLE),
	SpcasmTokenType(SemanticTokenType::FUNCTION),
	SpcasmTokenType(SemanticTokenType::OPERATOR),
	SpcasmTokenType(SemanticTokenType::MACRO),
	SpcasmTokenType(SemanticTokenType::KEYWORD),
	SpcasmTokenType(SemanticTokenType::COMMENT),
	SpcasmTokenType(SemanticTokenType::STRING),
	SpcasmTokenType(SemanticTokenType::new("punctuation")),
];

impl From<&Token> for SpcasmTokenType {
	fn from(token: &Token) -> Self {
		match token {
			Token::Mnemonic(_, _) => SemanticTokenType::FUNCTION,
			Token::Identifier(_, _) => SemanticTokenType::FUNCTION,
			Token::SpecialIdentifier(_, _) => SemanticTokenType::PARAMETER,
			Token::Register(_, _) => SemanticTokenType::KEYWORD,
			Token::PlusRegister(_, _) => SemanticTokenType::KEYWORD,
			Token::Directive(_, _) => SemanticTokenType::FUNCTION,
			Token::Number(_, _, _) => SemanticTokenType::NUMBER,
			Token::String(_, _) => SemanticTokenType::STRING,
			Token::Hash(_) => SemanticTokenType::OPERATOR,
			Token::Comma(_) => SemanticTokenType::new("punctuation"),
			Token::Plus(_) => SemanticTokenType::OPERATOR,
			Token::RelativeLabelPlus(_, _) => SemanticTokenType::OPERATOR,
			Token::Minus(_) => SemanticTokenType::OPERATOR,
			Token::RelativeLabelMinus(_, _) => SemanticTokenType::OPERATOR,
			Token::RangeMinus(_) => SemanticTokenType::OPERATOR,
			Token::Star(_) => SemanticTokenType::OPERATOR,
			Token::DoubleStar(_) => SemanticTokenType::OPERATOR,
			Token::Slash(_) => SemanticTokenType::OPERATOR,
			Token::Pipe(_) => SemanticTokenType::OPERATOR,
			Token::Ampersand(_) => SemanticTokenType::OPERATOR,
			Token::Tilde(_) => SemanticTokenType::OPERATOR,
			Token::Caret(_) => SemanticTokenType::OPERATOR,
			Token::OpenParenthesis(_) => SemanticTokenType::new("punctuation"),
			Token::CloseParenthesis(_) => SemanticTokenType::new("punctuation"),
			Token::OpenIndexingParenthesis(_) => SemanticTokenType::OPERATOR,
			Token::CloseIndexingParenthesis(_) => SemanticTokenType::OPERATOR,
			Token::Colon(_) => SemanticTokenType::new("punctuation"),
			Token::Period(_) => SemanticTokenType::new("punctuation"),
			Token::OpenAngleBracket(_) => SemanticTokenType::OPERATOR,
			Token::DoubleOpenAngleBracket(_) => SemanticTokenType::OPERATOR,
			Token::CloseAngleBracket(_) => SemanticTokenType::OPERATOR,
			Token::DoubleCloseAngleBracket(_) => SemanticTokenType::OPERATOR,
			Token::Percent(_) => SemanticTokenType::new("punctuation"),
			Token::ExplicitDirectPage(_) => SemanticTokenType::OPERATOR,
			Token::Equals(_) => SemanticTokenType::OPERATOR,
			Token::Newline(_) => SemanticTokenType::COMMENT,
			Token::TestComment(_, _) => SemanticTokenType::COMMENT,
		}
		.into()
	}
}

impl From<SemanticTokenType> for SpcasmTokenType {
	fn from(value: SemanticTokenType) -> Self {
		Self(value)
	}
}

impl From<SpcasmTokenType> for SemanticTokenType {
	fn from(value: SpcasmTokenType) -> Self {
		value.0
	}
}

impl Deref for SpcasmTokenType {
	type Target = SemanticTokenType;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl From<SpcasmTokenType> for u32 {
	fn from(typ: SpcasmTokenType) -> Self {
		SEMANTIC_TOKEN_TYPES.iter().position(|global_type| **global_type == *typ).unwrap() as u32
	}
}
