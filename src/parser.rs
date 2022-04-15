use std::num::NonZeroU8;

use crate::lexer::RegisterName;
use crate::lexer::Token;
use std::rc::Rc;

type MemoryAddress = i64;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
	pub label: Option<Label>,
	pub opcode: Opcode,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
	pub name: String,
	pub location: Option<MemoryAddress>,
}

impl Label {
	pub const fn is_resolved(&self) -> bool {
		self.location.is_some()
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Opcode {
	pub mnemonic: String,
	pub first_operand: Option<AddressingMode>,
	pub second_operand: Option<AddressingMode>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Number {
	Literal(i64),
	Label(Rc<Label>),
	// TODO: support assembly-time calculations
}

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
	/// abs)
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
	Register(RegisterName),
}

impl AddressingMode {
	pub fn opcode_size(&self) -> NonZeroU8 {
		unsafe {
			NonZeroU8::new_unchecked(match self {
				Self::IndirectX | Self::IndirectXAutoIncrement | Self::IndirectY => 1,
				Self::Immediate(_)
				| Self::DirectPage(_)
				| Self::DirectPageXIndexed(_)
				| Self::DirectPageYIndexed(_)
				| Self::DirectPageXIndexedIndirect(_)
				| Self::DirectPageIndirectYIndexed(_) => 2,
				Self::Address(_) | Self::XIndexed(_) | Self::YIndexed(_) => 3,
				Self::Register(_) => unimplemented!(),
			})
		}
	}
}

#[derive(Clone, Debug)]
pub struct Environment {
	pub labels: Vec<Rc<Label>>,
}

impl Environment {
	pub const fn new() -> Self {
		Self { labels: Vec::new() }
	}

	pub fn parse(&mut self, tokens: &[Token]) -> Result<Vec<Instruction>, String> {
		let mut tokens = tokens.iter().peekable();

		let mut instructions = Vec::new();

		while let Some(token) = tokens.next().cloned() {
			match token {
				Token::Identifier(identifier) => {
					if Self::is_valid_mnemonic(&identifier) {
						let mut tokens_for_instruction: Vec<Token> = Vec::new();
						while tokens.peek().and_then(|token| token.expect(Token::Newline).err()).is_some() {
							tokens_for_instruction.push(tokens.next().cloned().ok_or("Expected instruction")?);
						}
						instructions.push(self.create_instruction(&identifier.to_lowercase(), &tokens_for_instruction)?);
						tokens
							.next()
							.and_then(|token| token.expect(Token::Newline).ok())
							.ok_or("Expected newline or end of file")?;
					} else {
						unimplemented!("Handle labels");
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

	fn create_instruction(&mut self, mnemonic: &'_ str, tokens: &[Token]) -> Result<Instruction, String> {
		println!("{} {:?}", mnemonic, tokens);
		match mnemonic {
			"mov" => self.create_mov(tokens),
			_ => unimplemented!("Handle other instructions"),
		}
	}

	fn create_mov(&mut self, tokens: &[Token]) -> Result<Instruction, String> {
		let mut addressing_modes = tokens.split(|token| token.expect(Token::Comma).is_ok());
		let first_addressing_mode =
			self.parse_addressing_mode(addressing_modes.next().ok_or("Expected addressing mode before ','")?)?;
		let second_addressing_mode =
			self.parse_addressing_mode(addressing_modes.next().ok_or("Expected addressing mode after ','")?)?;
		let instruction =
			Instruction { opcode: Opcode::make_mov(first_addressing_mode, second_addressing_mode), label: None };
		println!("{:?}", instruction);
		Ok(instruction)
	}

	fn parse_addressing_mode(&mut self, tokens: &[Token]) -> Result<AddressingMode, String> {
		let mut tokens = tokens.iter();
		match tokens.next().ok_or("Expected addressing mode")? {
			Token::Register(name) => Ok(AddressingMode::Register(*name)),
			Token::Hash => {
				Ok(AddressingMode::Immediate(match tokens.next().ok_or("Single '#' is not a valid addressing mode")? {
					Token::Number(immediate) => Number::Literal(*immediate),
					_ => return Err("Expected number for immediate argument".to_string()),
				}))
			},
			Token::Number(address) => {
				let is_direct_page = *address <= 0xFF;
				let literal_address = Number::Literal(*address);
				// Indirect addressing with '+X' or '+Y'
				Ok(if tokens.next().and_then(|token| token.expect(Token::Plus).ok()).is_some() {
					match tokens.next().ok_or("Expected indexing register")? {
						Token::Register(RegisterName::X) => {
							if is_direct_page {
								AddressingMode::DirectPageXIndexed(literal_address)
							} else {
								AddressingMode::XIndexed(literal_address)
							}
						},
						Token::Register(RegisterName::Y) => {
							if is_direct_page {
								AddressingMode::DirectPageYIndexed(literal_address)
							} else {
								AddressingMode::YIndexed(literal_address)
							}
						},
						reg => return Err(format!("Illegal token {:?} for indexing", reg)),
					}
				} else if is_direct_page {
					AddressingMode::DirectPage(literal_address)
				} else {
					AddressingMode::Address(literal_address)
				})
			},
			Token::Identifier(label) => Ok(AddressingMode::Address(Number::Label(self.get_label(label)))),
			Token::OpenParenthesis => match tokens.next().ok_or("Expected indirect argument inside brackets")? {
				Token::Register(name) => {
					tokens.next().ok_or("Expected ')'")?.expect(Token::CloseParenthesis)?;
					Ok(match name {
						RegisterName::X => {
							if tokens.next().and_then(|token| token.expect(Token::Plus).ok()).is_some() {
								// '+' after closing bracket
								AddressingMode::IndirectXAutoIncrement
							} else {
								AddressingMode::IndirectX
							}
						},
						RegisterName::Y => AddressingMode::IndirectY,
						_ => return Err(format!("Invalid register {:?} for indirect addressing", name)),
					})
				},
				Token::Number(number) => {
					let number_literal = Number::Literal(*number);
					match tokens.next().ok_or("Expected '+' or ')'")? {
						Token::Plus => {
							tokens
								.next()
								.and_then(|token| match token {
									Token::Register(RegisterName::X) => Some(()),
									_ => None,
								})
								.ok_or("Expected register X")?;
							Ok(AddressingMode::DirectPageXIndexedIndirect(number_literal))
						},
						Token::CloseParenthesis => {
							tokens.next().ok_or("Expected '+'")?.expect(Token::Plus)?;
							match tokens.next().ok_or("Expected 'Y'")? {
								Token::Register(RegisterName::Y) => {
									Ok(AddressingMode::DirectPageIndirectYIndexed(number_literal))
								},
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
}

impl Opcode {
	pub fn make_mov(destination: AddressingMode, source: AddressingMode) -> Self {
		Self { mnemonic: "mov".to_owned(), first_operand: Some(destination), second_operand: Some(source) }
	}
}
