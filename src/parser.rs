//! Parsing and AST.
#![allow(clippy::use_self)]
use std::result::Result;
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};

use crate::error::{AssemblyCode, AssemblyError};
use crate::instruction::{AddressingMode, Instruction, Label, Mnemonic, Number, Opcode};
use crate::{Register, Token};
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
						current_label = Some(self.get_label(identifier, token.source_span(), false));
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
			| Mnemonic::Call
			| Mnemonic::Pcall
			| Mnemonic::Tcall
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
			span: (
				mnemonic_token_location.offset(),
				(final_span.offset() + final_span.len()) - mnemonic_token_location.offset(),
			)
				.into(),
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
			span: (
				mnemonic_token_location.offset(),
				(final_span.offset() + final_span.len()) - mnemonic_token_location.offset(),
			)
				.into(),
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
			Token::Hash(location) => Ok(AddressingMode::Immediate(self.create_number(
				tokens.next().ok_or(AssemblyError::SingleHashInvalid {
					location: (location).into(),
					src:      self.source_code.clone(),
				})?,
				false,
			)?)),
			literal_token @ (Token::Number(..) | Token::Identifier(..)) => {
				let token_start = SourceOffset::from(literal_token.source_span().offset());
				let literal = self.create_number(&literal_token, true)?;
				let is_direct_page = match literal {
					Number::Literal(address) => address <= 0xFF,
					Number::Label(_) => false,
				};
				let next_token_or_none = tokens.next();
				Ok(match next_token_or_none {
					Some(Token::Plus(token_start)) => {
						match tokens
							.next()
							.ok_or_else(missing_token_error(Token::Register(Register::X, (*token_start).into()).into()))?
						{
							// Indirect addressing with '+X' or '+Y'
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
					},
					#[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
					Some(Token::Period(token_start)) =>
						if let Some(Token::Number(bit, location)) = tokens.next() {
							if *bit < 0 || *bit >= 8 {
								return Err(AssemblyError::InvalidBitIndex {
									index:    *bit as u8,
									location: *location,
									src:      self.source_code.clone(),
								});
							}
							if is_direct_page {
								AddressingMode::DirectPageBit(literal, (bit & 0x07) as u8)
							} else {
								AddressingMode::AddressBit(literal, (bit & 0x07) as u8)
							}
						} else {
							return Err(missing_token_error(Token::Number(0, (*token_start).into()).into())());
						},
					None | Some(Token::Newline(..)) =>
						if is_direct_page {
							AddressingMode::DirectPage(literal)
						} else {
							AddressingMode::Address(literal)
						},
					#[cfg(test)]
					Some(Token::TestComment(..)) =>
						if is_direct_page {
							AddressingMode::DirectPage(literal)
						} else {
							AddressingMode::Address(literal)
						},
					Some(other_token) =>
						return Err(AssemblyError::ExpectedToken {
							expected: Token::Plus(other_token.source_span().offset().into()),
							actual:   other_token.clone(),
							location: other_token.source_span(),
							src:      self.source_code.clone(),
						}),
				})
			},
			Token::OpenParenthesis(location) =>
				match tokens.next().ok_or_else(missing_token_error("indirect argument inside brackets".into()))? {
					register_token @ Token::Register(name, ..) => {
						tokens
							.next()
							.ok_or_else(missing_token_error(Token::CloseParenthesis(location).into()))?
							.expect(Token::CloseParenthesis(location), self.source_code.clone())?;
						Ok(match name {
							#[allow(clippy::branches_sharing_code)]
							Register::X => {
								if tokens
									.next()
									.and_then(|token| token.expect(Token::Plus(location), self.source_code.clone()).ok())
									.is_some()
								{
									if let Some(further_token) = tokens.next() {
										println!(
											"{:?}",
											miette::Report::new(AssemblyError::DanglingTokens {
												src:      self.source_code.clone(),
												location: further_token.source_span(),
											})
										);
									}
									// '+' after closing bracket
									AddressingMode::IndirectXAutoIncrement
								} else {
									if let Some(further_token) = tokens.next() {
										println!(
											"{:?}",
											miette::Report::new(AssemblyError::DanglingTokens {
												src:      self.source_code.clone(),
												location: further_token.source_span(),
											})
										);
									}
									AddressingMode::IndirectX
								}
							},
							Register::Y => {
								if let Some(further_token) = tokens.next() {
									println!(
										"{:?}",
										miette::Report::new(AssemblyError::DanglingTokens {
											src:      self.source_code.clone(),
											location: further_token.source_span(),
										})
									);
								}
								AddressingMode::IndirectY
							},
							_ =>
								return Err(AssemblyError::InvalidIndexingToken {
									token:    register_token.clone(),
									location: register_token.source_span(),
									src:      self.source_code.clone(),
								}),
						})
					},
					literal_token @ (Token::Number(..) | Token::Identifier(..)) => {
						let literal = self.create_number(literal_token, true)?;
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

								if let Some(further_token) = tokens.next() {
									println!(
										"{:?}",
										miette::Report::new(AssemblyError::DanglingTokens {
											src:      self.source_code.clone(),
											location: further_token.source_span(),
										})
									);
								}
								Ok(AddressingMode::DirectPageXIndexedIndirect(literal))
							},
							Token::CloseParenthesis(second_location) => {
								let span = (location, *second_location).into();
								tokens
									.next()
									.ok_or_else(missing_token_error(Token::Plus(location).into()))?
									.expect(Token::Plus(location), self.source_code.clone())?;
								let result = tokens
									.next()
									.ok_or_else(missing_token_error(Token::Register(Register::Y, span).into()))?
									.expect(Token::Register(Register::Y, span), self.source_code.clone())
									.map(|_| AddressingMode::DirectPageIndirectYIndexed(literal));
								if let Some(further_token) = tokens.next() {
									println!(
										"{:?}",
										miette::Report::new(AssemblyError::DanglingTokens {
											src:      self.source_code.clone(),
											location: further_token.source_span(),
										})
									);
								}
								result
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

	/// TODO: We're setting the label's position wrong if we reference it before it has been defined.
	fn get_label(&mut self, name: &'_ str, span: SourceSpan, used_as_address: bool) -> Arc<Label> {
		match self.labels.iter_mut().find(|label| label.name == name) {
			Some(matching_label) => {
				if used_as_address && !matching_label.used_as_address {
					unsafe { Arc::get_mut_unchecked(matching_label).used_as_address = true };
				}
				matching_label.clone()
			},
			None => {
				let new_label = Arc::new(Label { name: name.to_owned(), location: None, span, used_as_address });
				self.labels.push(new_label.clone());
				new_label
			},
		}
	}

	fn create_number<'a>(&'a mut self, token: &'a Token, used_as_address: bool) -> Result<Number, AssemblyError> {
		match token {
			Token::Number(number, ..) => Ok(Number::Literal(*number)),
			Token::Identifier(label, ..) => Ok(Number::Label(self.get_label(label, token.source_span(), used_as_address))),
			_ => Err(AssemblyError::ExpectedToken {
				expected: Token::Number(0, token.source_span()),
				actual:   token.clone(),
				location: token.source_span(),
				src:      self.source_code.clone(),
			}),
		}
	}
}
