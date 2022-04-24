//! Parsing and AST.
#![allow(clippy::use_self)]

use std::fmt::{Display, Error, Formatter, UpperHex};
use std::result::Result;
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};
use serde::Serialize;
use serde_variant::to_variant_name;
use spcasm_derive::Parse;

use crate::error::{AssemblyCode, AssemblyError};
use crate::lexer::{Register, Token};
/// Types for representing data and memory addresses (this is overkill).
pub type MemoryAddress = i64;

/// One CPU instruction.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
	/// Label of this instruction, if any.
	pub label:          Option<Arc<Label>>,
	/// Opcode of this instruction (slightly misnamed)
	pub opcode:         Opcode,
	pub(crate) span:    SourceSpan,
	/// Only used for testing purposes: this is the data that the instruction should assemble to according to the test
	/// file.
	#[cfg(test)]
	pub expected_value: Vec<u8>,
}

/// A textual label that refers to some location in memory and resolves to a numeric value at some point.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
	/// User-given label name.
	pub name:     String,
	/// Resolved memory location of the label, if any.
	pub location: Option<MemoryAddress>,
}

impl Label {
	/// Whether this label has already been resolved to a memory location.
	#[must_use]
	pub const fn is_resolved(&self) -> bool {
		self.location.is_some()
	}

	/// Resolves the label to the given memory location.
	pub fn resolve_to(&mut self, location: MemoryAddress) {
		self.location = Some(location);
	}
}

/// An instruction's core data that's used to generate machine code.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Opcode {
	/// Instruction mnemonic.
	pub mnemonic:       Mnemonic,
	/// First operand, usually instruction target.
	pub first_operand:  Option<AddressingMode>,
	/// Second operand, usually instruction source. This is unused on many instructions.
	pub second_operand: Option<AddressingMode>,
}

/// Anything that can be primitively parsed from a string into an enum variant.
/// This trait is intended to be derived with the macro from ``spcasm_derive``.
pub trait Parse
where
	Self: Sized,
{
	/// Parse this enum from the string representation.
	/// # Errors
	/// If the string doesn't correspond with any enum variant.
	fn parse(value: &str, location: SourceSpan, src: Arc<AssemblyCode>) -> Result<Self, AssemblyError>;
}

/// Instruction mnemonics of the SPC700.
#[allow(missing_docs, clippy::use_self)]
#[derive(Clone, Debug, Copy, Eq, PartialEq, Parse, Serialize)]
pub enum Mnemonic {
	Mov,
	Adc,
	Sbc,
	Cmp,
	And,
	Or,
	Eor,
	Inc,
	Dec,
	Asl,
	Lsr,
	Rol,
	Ror,
	Xcn,
	Movw,
	Incw,
	Decw,
	Addw,
	Subw,
	Cmpw,
	Mul,
	Div,
	Daa,
	Das,
	Bra,
	Beq,
	Bne,
	Bcs,
	Bcc,
	Bvs,
	Bvc,
	Bmi,
	Bpl,
	Bbs,
	Bbc,
	Cbne,
	Dbnz,
	Jmp,
	Call,
	Pcall,
	Tcall,
	Brk,
	Ret,
	Ret1,
	Push,
	Pop,
	Set1,
	Clr1,
	Tset1,
	Tclr1,
	And1,
	Or1,
	Eor1,
	Not1,
	Mov1,
	Clrc,
	Setc,
	Notc,
	Clrv,
	Clrp,
	Setp,
	Ei,
	Di,
	Nop,
	Sleep,
	Stop,
}

impl Display for Mnemonic {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", to_variant_name(self).unwrap().to_uppercase())
	}
}

/// Any number, either a literal or a label that's resolved to a number later.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Number {
	/// A literal number.
	Literal(MemoryAddress),
	/// A label that will resolve to a number later.
	Label(Arc<Label>),
	// TODO: support assembly-time calculations
}

impl Number {
	/// Extracts the actual value of this number.
	/// # Panics
	/// If the label was not yet resolved, this function panics as it assumes that that has already happened.
	#[must_use]
	pub fn value(&self) -> MemoryAddress {
		match self {
			Self::Literal(value) => *value,
			// necessary because matching through an Rc is not possible right now (would be super dope though).
			Self::Label(label) => match **label {
				Label { location: Some(value), .. } => value,
				_ => panic!("Unresolved label {:?}", label),
			},
		}
	}
}

impl From<MemoryAddress> for Number {
	fn from(address: MemoryAddress) -> Self {
		Self::Literal(address)
	}
}

impl UpperHex for Number {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		match self {
			Self::Literal(numeric_address) => write!(f, "{:X}", numeric_address),
			Self::Label(ref unresolved_label) => match &**unresolved_label {
				Label { location: Some(numeric_address), .. } => write!(f, "{:X}", numeric_address),
				Label { name, .. } => write!(f, "<{}>", name),
			},
		}
	}
}

/// Addressing modes of the SPC700. Not all of these are supported everywhere (in fact, most aren't).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AddressingMode {
	/// #immediate
	Immediate(Number),
	/// (X)
	IndirectX,
	/// (Y)
	IndirectY,
	/// (X) with automatic X++
	IndirectXAutoIncrement,
	/// (dp)
	DirectPage(Number),
	/// dp+X
	DirectPageXIndexed(Number),
	/// dp+Y
	DirectPageYIndexed(Number),
	/// abs
	Address(Number),
	/// abs+X
	XIndexed(Number),
	/// abs+Y
	YIndexed(Number),
	/// (dp+X)
	DirectPageXIndexedIndirect(Number),
	/// (dp)+Y
	DirectPageIndirectYIndexed(Number),
	// ...
	/// A, X, Y, SP, ...
	Register(Register),
}

impl Display for AddressingMode {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
		write!(f, "{}", match self {
			Self::Immediate(number) => format!("#${:02X}", number),
			Self::IndirectX => "(X)".to_owned(),
			Self::IndirectY => "(Y)".to_owned(),
			Self::IndirectXAutoIncrement => "(X)+".to_owned(),
			Self::DirectPage(address) => format!("${:02X}", address),
			Self::DirectPageXIndexed(address) => format!("${:02X}+X", address),
			Self::DirectPageYIndexed(address) => format!("${:02X}+Y", address),
			Self::DirectPageXIndexedIndirect(address) => format!("(${:02X}+X)", address),
			Self::DirectPageIndirectYIndexed(address) => format!("(${:02X})+Y", address),
			Self::Address(address) => format!("${:04X}", address),
			Self::XIndexed(address) => format!("${:04X}+X", address),
			Self::YIndexed(address) => format!("${:04X}+Y", address),
			Self::Register(register) => format!("{}", register),
		})
	}
}

/// Environment object for parsing. Holds the list of labels.
#[derive(Debug)]
pub struct Environment {
	/// The list of labels.
	pub labels:      Vec<Arc<Label>>,
	/// The source code of the assembly code.
	pub source_code: Arc<AssemblyCode>,
}

impl Environment {
	/// Creates an empty environment.
	#[must_use]
	pub const fn new(source_code: Arc<AssemblyCode>) -> Self {
		Self { labels: Vec::new(), source_code }
	}

	/// Parses the token stream into a list of instructions while keeping track of labels internally. Note that no label
	/// resolution is actually done.
	///
	/// # Errors
	/// Any parser error is returned as a string.
	/// # Panics
	/// All the panics are programming bugs.
	pub fn parse(&mut self, tokens: &[Token]) -> Result<Vec<Instruction>, AssemblyError> {
		let mut tokens: std::iter::Peekable<std::slice::Iter<Token>> = tokens.iter().peekable();
		let mut instructions = Vec::new();
		let mut current_label = None;

		while let Some(token) = tokens.next().cloned() {
			match &token {
				Token::Identifier(identifier, location) => {
					let location_span = SourceOffset::from(location.offset());
					if Self::is_valid_mnemonic(identifier) {
						let mut tokens_for_instruction: Vec<Token> = Vec::new();
						while tokens
							.peek()
							.and_then(|token| token.expect(Token::Newline(location_span), self.source_code.clone()).err())
							.is_some()
						{
							tokens_for_instruction.push(tokens.next().unwrap().clone());
						}
						instructions.push(self.create_instruction(&token, &tokens_for_instruction, current_label)?);
						current_label = None;
						// is Ok() if there's no further token due to EOF
						tokens
							.next()
							.map(|token| token.expect(Token::Newline(location_span), self.source_code.clone()))
							.transpose()?;
					} else {
						current_label = Some(self.get_label(identifier));
						tokens
							.next()
							.map(|token| token.expect(Token::Colon(location_span), self.source_code.clone()))
							.ok_or_else(|| AssemblyError::ExpectedToken {
								expected: Token::Colon(location_span),
								actual:   Token::Newline(location_span),
								src:      self.source_code.clone(),
								location: *location,
							})
							.flatten()?;
					}
				},
				Token::Newline(..) => {},
				actual =>
					return Err(AssemblyError::ExpectedToken {
						expected: Token::Identifier("identifier".to_owned(), token.source_span()),
						actual:   actual.clone(),
						location: token.source_span(),
						src:      self.source_code.clone(),
					}),
			}
		}

		Ok(instructions)
	}

	fn is_valid_mnemonic(identifier: &str) -> bool {
		[
			"mov", "adc", "sbc", "cmp", "and", "or", "eor", "inc", "dec", "asl", "lsr", "rol", "ror", "xcn", "movw",
			"incw", "decw", "addw", "subw", "cmpw", "mul", "div", "daa", "das", "bra", "beq", "bne", "bcs", "bcc", "bvs",
			"bvc", "bmi", "bpl", "bbs", "bbc", "cbne", "dbnz", "jmp", "call", "pcall", "tcall", "brk", "ret", "ret1",
			"push", "pop", "set1", "clr1", "tset1", "tclr1", "and1", "or1", "eor1", "not1", "mov1", "clrc", "setc",
			"notc", "clrv", "clrp", "setp", "ei", "di", "nop", "sleep", "stop",
		]
		.contains(&identifier.to_lowercase().as_str())
	}

	fn create_instruction<'a>(
		&mut self,
		identifier: &'a Token,
		tokens: &'a [Token],
		label: Option<Arc<Label>>,
	) -> Result<Instruction, AssemblyError> {
		let identifier_name = match identifier {
			Token::Identifier(identifier, ..) => identifier.to_lowercase(),
			_ => unreachable!(),
		};
		let mnemonic = Mnemonic::parse(&identifier_name, identifier.source_span(), self.source_code.clone())?;
		match mnemonic {
			Mnemonic::Mov
			| Mnemonic::Adc
			| Mnemonic::Sbc
			| Mnemonic::And
			| Mnemonic::Or
			| Mnemonic::Eor
			| Mnemonic::Cmp
			| Mnemonic::Movw
			| Mnemonic::Addw
			| Mnemonic::Subw
			| Mnemonic::Cmpw
			| Mnemonic::Div
			| Mnemonic::Bbs
			| Mnemonic::Bbc
			| Mnemonic::Cbne
			| Mnemonic::Dbnz => self.make_two_operand_instruction(mnemonic, tokens, label, identifier.source_span()),
			Mnemonic::Inc
			| Mnemonic::Dec
			| Mnemonic::Asl
			| Mnemonic::Lsr
			| Mnemonic::Rol
			| Mnemonic::Ror
			| Mnemonic::Incw
			| Mnemonic::Decw
			| Mnemonic::Daa
			| Mnemonic::Das
			| Mnemonic::Mul
			| Mnemonic::Bra
			| Mnemonic::Beq
			| Mnemonic::Bne
			| Mnemonic::Bcs
			| Mnemonic::Bcc
			| Mnemonic::Bvs
			| Mnemonic::Bvc
			| Mnemonic::Bmi
			| Mnemonic::Bpl
			| Mnemonic::Jmp
			| Mnemonic::Push
			| Mnemonic::Pop => self.make_single_operand_instruction(mnemonic, tokens, label, identifier.source_span()),
			Mnemonic::Brk
			| Mnemonic::Ret
			| Mnemonic::Ret1
			| Mnemonic::Clrc
			| Mnemonic::Setc
			| Mnemonic::Notc
			| Mnemonic::Clrv
			| Mnemonic::Clrp
			| Mnemonic::Setp
			| Mnemonic::Ei
			| Mnemonic::Di
			| Mnemonic::Nop
			| Mnemonic::Sleep
			| Mnemonic::Stop => self.make_zero_operand_instruction(mnemonic, tokens, label, identifier.source_span()),
			_ => unimplemented!("Handle other instructions"),
		}
	}

	fn make_two_operand_instruction(
		&mut self,
		mnemonic: Mnemonic,
		tokens: &[Token],
		label: Option<Arc<Label>>,
		mnemonic_token_location: SourceSpan,
	) -> Result<Instruction, AssemblyError> {
		let source_code_copy = self.source_code.clone();
		let mut addressing_modes = tokens.split(|token| {
			token.expect(Token::Comma(SourceOffset::from(token.source_span().offset())), source_code_copy.clone()).is_ok()
		});
		let first_addressing_mode = self.parse_addressing_mode(addressing_modes.next().ok_or_else(|| {
			AssemblyError::UnexpectedEndOfTokens {
				expected: "addressing mode".into(),
				location: mnemonic_token_location,
				src:      self.source_code.clone(),
			}
		})?)?;
		let second_addressing_mode = self.parse_addressing_mode(addressing_modes.next().ok_or_else(|| {
			AssemblyError::UnexpectedEndOfTokens {
				expected: "addressing mode".into(),
				location: mnemonic_token_location,
				src:      self.source_code.clone(),
			}
		})?)?;
		#[cfg(test)]
		let expected_value = tokens
			.iter()
			.find_map(|token| match token {
				Token::TestComment(expected_value, ..) => Some(expected_value),
				_ => None,
			})
			.ok_or(AssemblyError::MissingTestResult {
				location: mnemonic_token_location,
				src:      self.source_code.clone(),
			})?
			.clone();
		let final_span = tokens.last().unwrap().source_span();
		let instruction = Instruction {
			opcode: Opcode::make_two_operand_instruction(mnemonic, first_addressing_mode, second_addressing_mode),
			label,
			span: (mnemonic_token_location.offset(), (final_span.offset() + final_span.len()) - mnemonic_token_location.offset()).into(),
			#[cfg(test)]
			expected_value,
		};
		Ok(instruction)
	}

	fn make_single_operand_instruction<'a>(
		&'a mut self,
		mnemonic: Mnemonic,
		tokens: &'a [Token],
		label: Option<Arc<Label>>,
		mnemonic_token_location: SourceSpan,
	) -> Result<Instruction, AssemblyError> {
		let unparsed_addressing_mode = tokens
			.split(|token| match token {
				Token::Newline(..) => true,
				#[cfg(test)]
				Token::TestComment(..) => true,
				_ => false,
			})
			.next()
			.ok_or_else(|| AssemblyError::UnexpectedEndOfTokens {
				expected: "addressing mode".into(),
				location: mnemonic_token_location,
				src:      self.source_code.clone(),
			})?;
		let addressing_mode = self.parse_addressing_mode(unparsed_addressing_mode)?;
		#[cfg(test)]
		let expected_value = tokens
			.iter()
			.find_map(|token| match token {
				Token::TestComment(expected_value, ..) => Some(expected_value),
				_ => None,
			})
			.ok_or(AssemblyError::MissingTestResult {
				location: mnemonic_token_location,
				src:      self.source_code.clone(),
			})?
			.clone();
		let final_span = tokens.last().unwrap().source_span();
		let instruction = Instruction {
			opcode: Opcode::make_single_operand_instruction(mnemonic, addressing_mode),
			label,
			span: (mnemonic_token_location.offset(), (final_span.offset() + final_span.len()) - mnemonic_token_location.offset()).into(),
			#[cfg(test)]
			expected_value,
		};
		Ok(instruction)
	}

	fn make_zero_operand_instruction<'a>(
		&'a self,
		mnemonic: Mnemonic,
		tokens: &'a [Token],
		label: Option<Arc<Label>>,
		mnemonic_token_location: SourceSpan,
	) -> Result<Instruction, AssemblyError> {
		if tokens
			.iter()
			.filter(|token| match token {
				Token::Newline(..) => false,
				#[cfg(test)]
				Token::TestComment(..) => false,
				_ => true,
			})
			.count() == 0
		{
			#[cfg(test)]
			let expected_value = tokens
				.iter()
				.find_map(|token| match token {
					Token::TestComment(expected_value, ..) => Some(expected_value),
					_ => None,
				})
				.ok_or(AssemblyError::MissingTestResult {
					location: mnemonic_token_location,
					src:      self.source_code.clone(),
				})?
				.clone();
			let instruction = Instruction {
				opcode: Opcode { mnemonic, first_operand: None, second_operand: None },
				label,
				span: mnemonic_token_location,
				#[cfg(test)]
				expected_value,
			};
			Ok(instruction)
		} else {
			Err(AssemblyError::OperandNotAllowed {
				mnemonic,
				location: mnemonic_token_location,
				src: self.source_code.clone(),
			})
		}
	}

	#[allow(clippy::too_many_lines)]
	fn parse_addressing_mode(&mut self, token_span: &[Token]) -> Result<AddressingMode, AssemblyError> {
		let mut tokens: std::slice::Iter<Token> = token_span.iter();

		let source_code_copy = self.source_code.clone();
		let missing_token_error = |expected| {
			|| AssemblyError::UnexpectedEndOfTokens {
				expected,
				location: token_span.last().map_or((0, 0).into(), Token::source_span),
				src: source_code_copy.clone(),
			}
		};

		match tokens.next().cloned().ok_or_else(missing_token_error("addressing mode".into()))? {
			Token::Register(name, ..) => Ok(AddressingMode::Register(name)),
			Token::Hash(location) => Ok(AddressingMode::Immediate(self.create_number(tokens.next().ok_or(
				AssemblyError::SingleHashInvalid { location: (location).into(), src: self.source_code.clone() },
			)?)?)),
			literal_token @ (Token::Number(..) | Token::Identifier(..)) => {
				let token_start = SourceOffset::from(literal_token.source_span().offset());
				let literal = self.create_number(&literal_token)?;
				let is_direct_page = match literal {
					Number::Literal(address) => address <= 0xFF,
					Number::Label(_) => false,
				};
				// Indirect addressing with '+X' or '+Y'
				Ok(
					if let Some(previous_token) =
						tokens.next().and_then(|token| token.expect(Token::Plus(token_start), self.source_code.clone()).ok())
					{
						match tokens.next().ok_or_else(missing_token_error(
							Token::Register(Register::X, previous_token.source_span()).into(),
						))? {
							Token::Register(Register::X, ..) =>
								if is_direct_page {
									AddressingMode::DirectPageXIndexed(literal)
								} else {
									AddressingMode::XIndexed(literal)
								},
							Token::Register(Register::Y, ..) =>
								if is_direct_page {
									AddressingMode::DirectPageYIndexed(literal)
								} else {
									AddressingMode::YIndexed(literal)
								},
							reg =>
								return Err(AssemblyError::InvalidIndexingToken {
									token:    reg.clone(),
									location: reg.source_span(),
									src:      self.source_code.clone(),
								}),
						}
					} else if is_direct_page {
						AddressingMode::DirectPage(literal)
					} else {
						AddressingMode::Address(literal)
					},
				)
			},
			Token::OpenParenthesis(location) =>
				match tokens.next().ok_or_else(missing_token_error("indirect argument inside brackets".into()))? {
					register_token @ Token::Register(name, ..) => {
						tokens
							.next()
							.ok_or_else(missing_token_error(Token::CloseParenthesis(location).into()))?
							.expect(Token::CloseParenthesis(location), self.source_code.clone())?;
						Ok(match name {
							Register::X => {
								if tokens
									.next()
									.and_then(|token| token.expect(Token::Plus(location), self.source_code.clone()).ok())
									.is_some()
								{
									// '+' after closing bracket
									AddressingMode::IndirectXAutoIncrement
								} else {
									AddressingMode::IndirectX
								}
							},
							Register::Y => AddressingMode::IndirectY,
							_ =>
								return Err(AssemblyError::InvalidIndexingToken {
									token:    register_token.clone(),
									location: register_token.source_span(),
									src:      self.source_code.clone(),
								}),
						})
					},
					literal_token @ (Token::Number(..) | Token::Identifier(..)) => {
						let literal = self.create_number(literal_token)?;
						match tokens.next().ok_or_else(missing_token_error("'+' or ')'".into()))? {
							Token::Plus(second_location) => {
								tokens
									.next()
									.and_then(|token| match token {
										Token::Register(Register::X, ..) => Some(()),
										_ => None,
									})
									.ok_or_else(missing_token_error(
										Token::Register(Register::X, (location, *second_location).into()).into(),
									))?;
								Ok(AddressingMode::DirectPageXIndexedIndirect(literal))
							},
							Token::CloseParenthesis(second_location) => {
								let span = (location, *second_location).into();
								tokens
									.next()
									.ok_or_else(missing_token_error(Token::Plus(location).into()))?
									.expect(Token::Plus(location), self.source_code.clone())?;
								tokens
									.next()
									.ok_or_else(missing_token_error(Token::Register(Register::Y, span).into()))?
									.expect(Token::Register(Register::Y, span), self.source_code.clone())
									.map(|_| AddressingMode::DirectPageIndirectYIndexed(literal))
							},
							wrong_token => Err(AssemblyError::ExpectedToken {
								expected: Token::CloseParenthesis(wrong_token.source_span().offset().into()),
								actual:   wrong_token.clone(),
								location: wrong_token.source_span(),
								src:      self.source_code.clone(),
							}),
						}
					},
					_ => unimplemented!(),
				},
			_ => unimplemented!(),
		}
	}

	fn get_label(&mut self, name: &'_ str) -> Arc<Label> {
		match self.labels.iter().find(|label| label.name == name) {
			Some(matching_label) => matching_label.clone(),
			None => {
				let new_label = Arc::new(Label { name: name.to_owned(), location: None });
				self.labels.push(new_label.clone());
				new_label
			},
		}
	}

	fn create_number<'a>(&'a mut self, token: &'a Token) -> Result<Number, AssemblyError> {
		match token {
			Token::Number(number, ..) => Ok(Number::Literal(*number)),
			Token::Identifier(label, ..) => Ok(Number::Label(self.get_label(label))),
			_ => Err(AssemblyError::ExpectedToken {
				expected: Token::Number(0, token.source_span()),
				actual:   token.clone(),
				location: token.source_span(),
				src:      self.source_code.clone(),
			}),
		}
	}
}

impl Opcode {
	/// Create an instruction with two operands. It might not actually be valid with some combinations of addressing
	/// modes.
	#[must_use]
	pub const fn make_two_operand_instruction(
		mnemonic: Mnemonic,
		destination: AddressingMode,
		source: AddressingMode,
	) -> Self {
		Self { mnemonic, first_operand: Some(destination), second_operand: Some(source) }
	}

	/// Create an instruction with one operand. It might not actually be valid with some combinations of addressing
	/// modes.
	#[must_use]
	pub const fn make_single_operand_instruction(mnemonic: Mnemonic, destination: AddressingMode) -> Self {
		Self { mnemonic, first_operand: Some(destination), second_operand: None }
	}
}
