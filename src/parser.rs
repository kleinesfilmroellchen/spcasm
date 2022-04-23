//! Parsing and AST.

use std::rc::Rc;
use std::result::Result;

use spcasm_derive::Parse;

use crate::lexer::{Register, Token};
/// Types for representing data and memory addresses (this is overkill).
pub type MemoryAddress = i64;

/// One CPU instruction.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
	/// Label of this instruction, if any.
	pub label:          Option<Rc<Label>>,
	/// Opcode of this instruction (slightly misnamed)
	pub opcode:         Opcode,
	/// Memory location of the instruction, if any.
	pub location:       Option<MemoryAddress>,
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
	fn parse(value: &str) -> Result<Self, String>;
}

/// Instruction mnemonics of the SPC700.
#[derive(Clone, Debug, Copy, Eq, PartialEq, Parse)]
#[allow(missing_docs)]
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

/// Any number, either a literal or a label that's resolved to a number later.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Number {
	/// A literal number.
	Literal(MemoryAddress),
	/// A label that will resolve to a number later.
	Label(Rc<Label>),
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

/// Environment object for parsing. Holds the list of labels.
#[derive(Clone, Debug)]
pub struct Environment {
	/// The list of labels.
	pub labels: Vec<Rc<Label>>,
}

impl Environment {
	/// Creates an empty environment.
	#[must_use]
	pub const fn new() -> Self {
		Self { labels: Vec::new() }
	}

	/// Parses the token stream into a list of instructions while keeping track of labels internally. Note that no label
	/// resolution is actually done.
	///
	/// # Errors
	/// Any parser error is returned as a string.
	pub fn parse(&mut self, tokens: &[Token]) -> Result<Vec<Instruction>, String> {
		let mut tokens = tokens.iter().peekable();

		let mut instructions = Vec::new();

		let mut current_label = None;

		while let Some(token) = tokens.next().cloned() {
			match token {
				Token::Identifier(identifier) => {
					if Self::is_valid_mnemonic(&identifier) {
						let mut tokens_for_instruction: Vec<Token> = Vec::new();
						while tokens.peek().and_then(|token| token.expect(Token::Newline).err()).is_some() {
							tokens_for_instruction.push(tokens.next().cloned().ok_or("Expected instruction")?);
						}
						instructions.push(self.create_instruction(
							&identifier.to_lowercase(),
							&tokens_for_instruction,
							current_label,
						)?);
						current_label = None;
						// is Ok() if there's no further token due to EOF
						tokens.next().map(|token| token.expect(Token::Newline)).transpose()?;
					} else {
						current_label = Some(self.get_label(&identifier));
						tokens
							.next()
							.map(|token| token.expect(Token::Colon))
							.ok_or_else(|| "Expected ':'".to_owned())
							.flatten()?;
					}
				},
				Token::Newline => {},
				_ => return Err(format!("Unexpected token {:?}", token)),
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

	fn create_instruction(
		&mut self,
		mnemonic: &'_ str,
		tokens: &[Token],
		label: Option<Rc<Label>>,
	) -> Result<Instruction, String> {
		let mnemonic = Mnemonic::parse(mnemonic)?;
		println!("{:?} {:?}", mnemonic, tokens);
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
			| Mnemonic::Dbnz => self.make_two_operand_instruction(mnemonic, tokens, label),
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
			| Mnemonic::Jmp => self.make_single_operand_instruction(mnemonic, tokens, label),
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
			| Mnemonic::Stop => Self::make_zero_operand_instruction(mnemonic, tokens, label),
			_ => unimplemented!("Handle other instructions"),
		}
	}

	fn make_two_operand_instruction(
		&mut self,
		mnemonic: Mnemonic,
		tokens: &[Token],
		label: Option<Rc<Label>>,
	) -> Result<Instruction, String> {
		let mut addressing_modes = tokens.split(|token| token.expect(Token::Comma).is_ok());
		let first_addressing_mode =
			self.parse_addressing_mode(addressing_modes.next().ok_or("Expected addressing mode before ','")?)?;
		let second_addressing_mode =
			self.parse_addressing_mode(addressing_modes.next().ok_or("Expected addressing mode after ','")?)?;
		#[cfg(test)]
		let expected_value = tokens
			.iter()
			.find_map(|token| match token {
				Token::TestComment(expected_value) => Some(expected_value),
				_ => None,
			})
			.ok_or("Test assembly doesn't have an expected output comment")?
			.clone();
		let instruction = Instruction {
			opcode: Opcode::make_two_operand_instruction(mnemonic, first_addressing_mode, second_addressing_mode),
			label,
			location: None,
			#[cfg(test)]
			expected_value,
		};
		println!("{:?}", instruction);
		Ok(instruction)
	}

	fn make_single_operand_instruction(
		&mut self,
		mnemonic: Mnemonic,
		tokens: &[Token],
		label: Option<Rc<Label>>,
	) -> Result<Instruction, String> {
		let unparsed_addressing_mode = tokens
			.split(|token| match token {
				Token::Newline => true,
				#[cfg(test)]
				Token::TestComment(_) => true,
				_ => false,
			})
			.next()
			.ok_or("Expected an addressing mode")?;
		let addressing_mode = self.parse_addressing_mode(unparsed_addressing_mode)?;
		#[cfg(test)]
		let expected_value = tokens
			.iter()
			.find_map(|token| match token {
				Token::TestComment(expected_value) => Some(expected_value),
				_ => None,
			})
			.ok_or("Test assembly doesn't have an expected output comment")?
			.clone();
		let instruction = Instruction {
			opcode: Opcode::make_single_operand_instruction(mnemonic, addressing_mode),
			label,
			location: None,
			#[cfg(test)]
			expected_value,
		};
		Ok(instruction)
	}

	fn make_zero_operand_instruction(
		mnemonic: Mnemonic,
		tokens: &[Token],
		label: Option<Rc<Label>>,
	) -> Result<Instruction, String> {
		if tokens
			.iter()
			.filter(|token| match token {
				Token::Newline => false,
				#[cfg(test)]
				Token::TestComment(_) => false,
				_ => true,
			})
			.count() == 0
		{
			#[cfg(test)]
			let expected_value = tokens
				.iter()
				.find_map(|token| match token {
					Token::TestComment(expected_value) => Some(expected_value),
					_ => None,
				})
				.ok_or("Test assembly doesn't have an expected output comment")?
				.clone();
			let instruction = Instruction {
				opcode: Opcode { mnemonic, first_operand: None, second_operand: None },
				label,
				location: None,
				#[cfg(test)]
				expected_value,
			};
			Ok(instruction)
		} else {
			Err(format!("Didn't expect an addressing mode for mnemonic {:?}", mnemonic))
		}
	}

	fn parse_addressing_mode(&mut self, tokens: &[Token]) -> Result<AddressingMode, String> {
		let mut tokens = tokens.iter();
		match tokens.next().ok_or("Expected addressing mode")? {
			Token::Register(name) => Ok(AddressingMode::Register(*name)),
			Token::Hash => Ok(AddressingMode::Immediate(
				self.create_number(tokens.next().ok_or("Single '#' is not a valid addressing mode")?)?,
			)),
			literal_token @ (Token::Number(_) | Token::Identifier(_)) => {
				let literal = self.create_number(literal_token)?;
				let is_direct_page = match literal {
					Number::Literal(address) => address <= 0xFF,
					Number::Label(_) => false,
				};
				// Indirect addressing with '+X' or '+Y'
				Ok(if tokens.next().and_then(|token| token.expect(Token::Plus).ok()).is_some() {
					match tokens.next().ok_or("Expected indexing register")? {
						Token::Register(Register::X) =>
							if is_direct_page {
								AddressingMode::DirectPageXIndexed(literal)
							} else {
								AddressingMode::XIndexed(literal)
							},
						Token::Register(Register::Y) =>
							if is_direct_page {
								AddressingMode::DirectPageYIndexed(literal)
							} else {
								AddressingMode::YIndexed(literal)
							},
						reg => return Err(format!("Illegal token {:?} for indexing", reg)),
					}
				} else if is_direct_page {
					AddressingMode::DirectPage(literal)
				} else {
					AddressingMode::Address(literal)
				})
			},
			Token::OpenParenthesis => match tokens.next().ok_or("Expected indirect argument inside brackets")? {
				Token::Register(name) => {
					tokens.next().ok_or("Expected ')'")?.expect(Token::CloseParenthesis)?;
					Ok(match name {
						Register::X => {
							if tokens.next().and_then(|token| token.expect(Token::Plus).ok()).is_some() {
								// '+' after closing bracket
								AddressingMode::IndirectXAutoIncrement
							} else {
								AddressingMode::IndirectX
							}
						},
						Register::Y => AddressingMode::IndirectY,
						_ => return Err(format!("Invalid register {:?} for indirect addressing", name)),
					})
				},
				literal_token @ (Token::Number(_) | Token::Identifier(_)) => {
					let literal = self.create_number(literal_token)?;
					match tokens.next().ok_or("Expected '+' or ')'")? {
						Token::Plus => {
							tokens
								.next()
								.and_then(|token| match token {
									Token::Register(Register::X) => Some(()),
									_ => None,
								})
								.ok_or("Expected register X")?;
							Ok(AddressingMode::DirectPageXIndexedIndirect(literal))
						},
						Token::CloseParenthesis => {
							tokens.next().ok_or("Expected '+'")?.expect(Token::Plus)?;
							match tokens.next().ok_or("Expected 'Y'")? {
								Token::Register(Register::Y) => Ok(AddressingMode::DirectPageIndirectYIndexed(literal)),
								_ => Err("Expected 'Y'".to_owned()),
							}
						},
						_ => Err("Expected either (dp+X) or (dp)+Y indexing".to_owned()),
					}
				},
				_ => unimplemented!(),
			},
			_ => unimplemented!(),
		}
	}

	fn get_label(&mut self, name: &'_ str) -> Rc<Label> {
		match self.labels.iter().find(|label| label.name == name) {
			Some(matching_label) => matching_label.clone(),
			None => {
				let new_label = Rc::new(Label { name: name.to_owned(), location: None });
				self.labels.push(new_label.clone());
				new_label
			},
		}
	}

	fn create_number(&mut self, token: &Token) -> Result<Number, String> {
		match token {
			Token::Number(number) => Ok(Number::Literal(*number)),
			Token::Identifier(label) => Ok(Number::Label(self.get_label(label))),
			_ => Err("Expected number".to_string()),
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
