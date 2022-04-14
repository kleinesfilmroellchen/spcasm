use std::num::NonZeroU8;

use crate::lexer::RegisterName;
use crate::lexer::Token;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction<'a> {
	pub label: Option<Label>,
	pub opcode: Opcode,
	// Only used for instructions that need a target label
	pub target: Option<&'a Label>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
	pub name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Opcode {
	pub mnemonic: String,
	pub first_operand: Option<AddressingMode>,
	pub second_operand: Option<AddressingMode>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AddressingMode {
	// #immediate
	Immediate(i64),
	// (X)
	IndirectX,
	// (X) with automatic X++
	IndirectXAutoIncrement,
	// (dp)
	IndirectDirectPage(i64),
	// (dp+X)
	IndirectDirectPageXIndexed(i64),
	// (dp+Y)
	IndirectDirectPageYIndexed(i64),
	// (abs)
	AbsoluteIndirect(i64),
	// (abs+X)
	AbsoluteIndirectXIndexed(i64),
	// (abs+Y)
	AbsoluteIndirectYIndexed(i64),
	// ...
	Register(RegisterName),
}

impl AddressingMode {
	pub fn opcode_size(&self) -> NonZeroU8 {
		unsafe {
			NonZeroU8::new_unchecked(match self {
				Self::IndirectX | Self::IndirectXAutoIncrement => 1,
				Self::Immediate(_)
				| Self::IndirectDirectPage(_)
				| Self::IndirectDirectPageXIndexed(_)
				| Self::IndirectDirectPageYIndexed(_) => 2,
				Self::AbsoluteIndirect(_) | Self::AbsoluteIndirectXIndexed(_) | Self::AbsoluteIndirectYIndexed(_) => 3,
				Self::Register(_) => unimplemented!(),
			})
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Flags {
	Negative,
	Zero,
	Carry,
	//...
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Instruction>, String> {
	let mut tokens = tokens.iter().peekable();

	let mut instructions = Vec::new();

	while let Some(token) = tokens.next().cloned() {
		match token {
			Token::Identifier(identifier) => {
				if is_valid_mnemonic(&identifier) {
					let mut tokens_for_instruction: Vec<Token> = Vec::new();
					while tokens.peek().and_then(|token| token.expect(Token::Newline).err()).is_some() {
						tokens_for_instruction.push(tokens.next().cloned().ok_or("Expected instruction")?);
					}
					instructions.push(create_instruction(&identifier.to_lowercase(), &tokens_for_instruction)?);
					tokens
						.next()
						.and_then(|token| token.expect(Token::Newline).ok())
						.ok_or("Expected newline or end of file")?;
				} else {
					unimplemented!("Handle labels");
				}
			},
			_ => return Err(format!("Unexpected token {:?}", token)),
		}
	}

	Ok(instructions)
}

fn is_valid_mnemonic(identifier: &str) -> bool {
	[
		"mov", "adc", "sbc", "cmp", "and", "or", "eor", "inc", "dec", "asl", "lsr", "rol", "ror", "xcn", "movw", "incw",
		"decw", "addw", "subw", "cmpw", "mul", "div", "daa", "das", "bra", "beq", "bne", "bcs", "bcc", "bvs", "bvc",
		"bmi", "bpl", "bbs", "bbc", "cbne", "dbnz", "jmp", "call", "pcall", "tcall", "brk", "ret", "ret1", "push", "pop",
		"set1", "clr1", "tset1", "tclr1", "and1", "or1", "eor1", "not1", "mov1", "clrc", "setc", "notc", "clrv", "clrp",
		"setp", "ei", "di", "nop", "sleep", "stop",
	]
	.contains(&identifier.to_lowercase().as_str())
}

fn create_instruction<'tok, 'ins>(mnemonic: &'_ str, tokens: &'tok [Token]) -> Result<Instruction<'ins>, String> {
	println!("{} {:?}", mnemonic, tokens);
	match mnemonic {
		"mov" => create_mov(tokens),
		_ => unimplemented!("Handle other instructions"),
	}
}

fn create_mov<'tok, 'ins>(tokens: &'tok [Token]) -> Result<Instruction<'ins>, String> {
	let mut tokens = tokens.iter();
	// TODO: allow other addressing modes
	let register = tokens.next().ok_or("Expected register name")?.expect(Token::Register(RegisterName::A))?.register();
	let _comma = tokens.next().ok_or("Expected ','")?.expect(Token::Comma)?.clone();
	let after_comma = tokens.next().ok_or("Expected addressing mode")?.clone();
	let mode = if let Ok(Token::Number(number)) = after_comma.expect(Token::Number(0)) {
		if *number <= 0xFF {
			AddressingMode::IndirectDirectPage(*number)
		} else {
			AddressingMode::AbsoluteIndirect(*number)
		}
	} else {
		// TODO: Parse more complex addressing modes
		AddressingMode::Immediate(0)
	};
	Ok(Instruction { opcode: Opcode::make_load(register, mode), target: None, label: None })
}

impl Opcode {
	pub fn make_load(register: RegisterName, mode: AddressingMode) -> Self {
		Self {
			mnemonic: "mov".to_owned(),
			first_operand: Some(AddressingMode::Register(register)),
			second_operand: Some(mode),
		}
	}
}
