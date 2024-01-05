//! Implementations of machine instructions in the emulated CPU.
//!
//! ### Note on cycle-accuracy
//! Exact microarchitectural timings are not documented on the SPC700 / S-SMP. Some software may (implicitly) rely on
//! them regardless, since the DSP has specific register and memory access patterns that will interact with CPU memory
//! accesses in particular ways. In many places, microarchitectural timings are guessed based on several known facts:
//! - The CPU can only access memory once per cycle.
//! - The first cycle of any instruction is spent in opcode fetch (and possibly decode, but that doesn't need to be
//!   modeled precisely).
//! - The memory bus is 8 bits, so e.g. 16-bit accesses take 2 (assumed consecutive) cycles.
//! - Memory access order is known for 16-bit addresses: usually it's lsb first, except for the stack (call and return)
//!   where only the top of the stack is accessed at any point.

#![allow(unused)]

use log::trace;

use super::Smp;
use crate::memory::Memory;
use crate::smp::ProgramStatusWord;

/// Action taken after an instruction cycle was executed.
#[derive(Clone, Copy)]
pub enum MicroArchAction {
	/// Continue instruction on next cycle. The instruction can return state which will be given to it on the next
	/// cycle.
	Continue(InstructionInternalState),
	/// Execute the next instruction as specified in the program counter.
	Next,
}

/// Internal state of an instruction.
#[derive(Clone, Copy, Debug, Default)]
pub struct InstructionInternalState {
	relative: i8,
	address:  u16,
	operand:  u8,
	operand2: u8,
}

impl InstructionInternalState {
	fn with_operand2(mut self, operand2: u8) -> Self {
		self.operand2 = operand2;
		self
	}

	fn with_operand(mut self, operand: u8) -> Self {
		self.operand = operand;
		self
	}

	fn with_relative(mut self, relative: i8) -> Self {
		self.relative = relative;
		self
	}

	fn with_address(mut self, address: u16) -> Self {
		self.address = address;
		self
	}
}

/// Function that implements an instruction.
///
/// Arguments are:
/// - the CPU to operate on
/// - the memory to access (only one memory read or write is possible in hardware, so except for debugging purposes,
///   only one memory access should be performed)
/// - the current cycle within the instruction, necessary for intra-instruction cycle timing
pub type InstructionImpl =
	fn(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction;

pub const OPCODE_TABLE: [InstructionImpl; 256] = [
	// 0x00
	nop,
	tcall_0,
	set1_0,
	bbs_0,
	or_a_dp,
	or_a_addr,
	or_a_x_indirect,
	or_a_dp_x_indirect,
	or_a_imm,
	or_dp_dp,
	or1,
	asl_dp,
	asl_addr,
	push_psw,
	tset1_addr,
	brk,
	// 0x10
	bpl,
	tcall_1,
	clr1_0,
	bbc_0,
	or_a_dp_x,
	or_a_addr_x,
	or_a_addr_y,
	or_a_dp_y_indirect,
	or_dp_imm,
	or_x_y_indirect,
	decw,
	asl_dp_x,
	asl_a,
	dec_x,
	cmp_x_addr,
	jmp_indexed,
	// 0x20
	clrp,
	tcall_2,
	set1_1,
	bbs_1,
	and_a_dp,
	and_a_addr,
	and_a_x_indirect,
	and_a_dp_x_indirect,
	and_a_imm,
	and_dp_dp,
	or1_inverted,
	rol_dp,
	rol_addr,
	push_a,
	cbne,
	bra,
	// 0x30
	bmi,
	tcall_3,
	clr1_1,
	bbc_1,
	and_a_dp_x,
	and_a_addr_x,
	and_a_addr_y,
	and_a_dp_y_indirect,
	and_dp_imm,
	and_x_y_indirect,
	incw,
	rol_dp_x,
	rol_a,
	inc_x,
	cmp_x_dp,
	call,
	// 0x40
	setp,
	tcall_4,
	set1_2,
	bbs_2,
	eor_a_dp,
	eor_a_addr,
	eor_a_x_indirect,
	eor_a_dp_x_indirect,
	eor_a_imm,
	eor_dp_dp,
	and1,
	lsr_dp,
	lsr_addr,
	push_x,
	tclr1,
	pcall,
	// 0x50
	bvc,
	tcall_5,
	clr1_2,
	bbc_2,
	eor_a_dp_x,
	eor_a_addr_x,
	eor_a_addr_y,
	eor_a_dp_y_indirect,
	eor_dp_imm,
	eor_x_y_indirect,
	cmpw_ya_dp,
	lsr_dp_x,
	lsr_a,
	mov_x_y,
	cmp_y_addr,
	jmp_addr,
	// 0x60
	clrc,
	tcall_6,
	set1_3,
	bbs_3,
	cmp_a_dp,
	cmp_a_addr,
	cmp_a_x_indirect,
	cmp_a_dp_x_indirect,
	cmp_a_imm,
	cmp_dp_dp,
	and1_inverted,
	ror_dp,
	ror_addr,
	push_y,
	dbnz_dp,
	ret,
	// 0x70
	bvs,
	tcall_7,
	clr1_3,
	bbc_3,
	cmp_a_dp_x,
	cmp_a_addr_x,
	cmp_a_addr_y,
	cmp_a_dp_y_indirect,
	cmp_dp_imm,
	cmp_x_y_indirect,
	addw_ya_dp,
	ror_dp_x,
	ror_a,
	mov_a_x,
	cmp_y_dp,
	reti,
	// 0x80
	setc,
	tcall_8,
	set1_4,
	bbs_4,
	adc_a_dp,
	adc_a_addr,
	adc_a_x_indirect,
	adc_a_dp_x_indirect,
	adc_a_imm,
	adc_dp_dp,
	eor1,
	dec_dp,
	dec_addr,
	mov_y_imm,
	pop_psw,
	mov_dp_imm,
	// 0x90
	bbc,
	tcall_9,
	clr1_4,
	bbc_4,
	adc_a_dp_x,
	adc_a_addr_x,
	adc_a_addr_y,
	adc_a_dp_y_indirect,
	adc_dp_imm,
	adc_x_y_indirect,
	subw_ya_dp,
	dec_dp_x,
	dec_a,
	mov_x_sp,
	div,
	xcn,
	// 0xA0
	ei,
	tcall_10,
	set1_5,
	bbs_5,
	sbc_a_dp,
	sbc_a_addr,
	sbc_a_x_indirect,
	sbc_a_dp_x_indirect,
	sbc_a_imm,
	sbc_dp_dp,
	mov1_c_addr,
	inc_dp,
	inc_addr,
	cmp_y_imm,
	pop_a,
	mov_x_inc_a,
	// 0xB0
	bcs,
	tcall_11,
	clr1_5,
	bbc_5,
	sbc_a_dp_x,
	sbc_a_addr_x,
	sbc_a_addr_y,
	sbc_a_dp_y_indirect,
	sbc_dp_imm,
	sbc_x_y_indirect,
	movw_ya_dp,
	inc_dp_x,
	inc_a,
	mov_sp_x,
	das,
	mov_a_inc_x,
	// 0xC0
	di,
	tcall_12,
	set1_6,
	bbs_6,
	mov_dp_a,
	mov_addr_a,
	mov_x_indirect_a,
	mov_dp_x_indirect_a,
	cmp_x_imm,
	mov_addr_x,
	mov1_addr_c,
	mov_dp_y,
	mov_addr_y,
	mov_x_imm,
	pop_x,
	mul,
	// 0xD0
	bne,
	tcall_13,
	clr1_6,
	bbc_6,
	mov_dp_x_a,
	mov_addr_x_a,
	mov_addr_y_a,
	mov_dp_indirect_y_a,
	mov_dp_x,
	mov_dp_y_x,
	movw_dp_ya,
	mov_dp_x_y,
	dec_y,
	mov_a_y,
	cbne_dp_x,
	daa,
	// 0xE0
	clrv,
	tcall_14,
	set1_7,
	bbs_7,
	mov_a_dp,
	mov_a_addr,
	mov_a_x_indirect,
	mov_a_dp_x_indirect,
	mov_a_imm,
	mov_x_addr,
	not1,
	mov_y_dp,
	mov_y_addr,
	notc,
	pop_y,
	sleep,
	// 0xF0
	beq,
	tcall_15,
	clr1_7,
	bbc_7,
	mov_a_dp_x,
	mov_a_addr_x,
	mov_a_addr_y,
	mov_a_dp_indirect_y,
	mov_x_dp,
	mov_x_dp_y,
	mov_dp_dp,
	mov_y_dp_x,
	inc_y,
	mov_y_a,
	dbnz_y,
	stop,
];

macro_rules! debug_instruction {
	($assembly:expr, $cycle:expr) => {
		if $cycle == 0 {
			log::debug!($assembly);
		}
	};
}

fn nop(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn or_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn or_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn asl_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn asl_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn push_psw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tset1_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn brk(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn bpl(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn or_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn decw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn asl_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn asl_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dec_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec x", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.x -= 1;
			cpu.set_negative(cpu.x);
			cpu.set_zero(cpu.x);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn cmp_x_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn jmp_indexed(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn clrp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn and_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn and_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn or1_inverted(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn rol_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn rol_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn push_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cbne(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bra(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn bmi(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn and_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn incw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn rol_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn rol_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn inc_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_x_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn call(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn setp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_4(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn eor_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn eor_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn lsr_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn lsr_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn push_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tclr1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn pcall(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn bvc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn eor_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn cmpw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn lsr_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn lsr_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_x_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_y_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn jmp_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn clrc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn cmp_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn cmp_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and1_inverted(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn ror_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn ror_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn push_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dbnz_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn ret(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn bvs(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn cmp_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp dp, #imm", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		2 => {
			let immediate = cpu.read_next_pc(memory);
			MicroArchAction::Continue(InstructionInternalState::default().with_operand(immediate))
		},
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			// Dummy read from destination address according to fullsnes.
			let operand2 = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand2(operand2))
		},
		4 => {
			let result = (state.operand2 as i8).wrapping_sub(state.operand as i8);
			cpu.set_negative(result as u8);
			cpu.set_zero(result as u8);
			cpu.set_subtract_carry(state.operand2 as i8, state.operand as i8);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn cmp_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn addw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn ror_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn ror_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_y_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn reti(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn setc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_8(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_4(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_4(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn adc_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn adc_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dec_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dec_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_y_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn pop_psw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp, #imm", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		2 => {
			let immediate = cpu.read_next_pc(memory);
			MicroArchAction::Continue(InstructionInternalState { operand: immediate, ..Default::default() })
		},
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(InstructionInternalState {
				address,
				operand: state.operand,
				..Default::default()
			})
		},
		3 => {
			// Dummy read from destination address according to fullsnes.
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		4 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn bbc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_9(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_4(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_4(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn adc_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn adc_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn subw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dec_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dec_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_x_sp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn div(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn xcn(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn ei(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_10(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn sbc_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn sbc_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov1_c_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn inc_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn inc_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cmp_y_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn pop_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_x_inc_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn bcs(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_11(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn sbc_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sbc_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn movw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn inc_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn inc_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_sp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov sp, x", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.sp = cpu.x;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn das(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_inc_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn di(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_12(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_addr_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_x_indirect_a(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("mov (x), a", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			// Does indeed perform a dummy read according to fullsnes.
			let _ = cpu.read(cpu.x as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(InstructionInternalState::default())
		},
		2 => {
			// TODO: Not sure if this cycle actually does nothing?
			MicroArchAction::Continue(InstructionInternalState::default())
		},
		3 => {
			cpu.write(cpu.x as u16 + cpu.direct_page_offset(), cpu.a, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_dp_x_indirect_a(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn cmp_x_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov1_addr_c(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_x_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, #imm", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let immediate = cpu.read_next_pc(memory);
			cpu.x = immediate;
			cpu.set_negative(cpu.x);
			cpu.set_zero(cpu.x);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn pop_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mul(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn bne(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("bne", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let offset = cpu.read_next_pc(memory) as i8;
			// branch if Z == 0
			if !cpu.psw.contains(ProgramStatusWord::Zero) {
				trace!("taking branch to {:+}", offset);
				MicroArchAction::Continue(InstructionInternalState { relative: offset, ..Default::default() })
			} else {
				MicroArchAction::Next
			}
		},
		2 => {
			// Hardware calculates branch target in this step, we do everything at the very end since memory accesses
			// don't happen.
			MicroArchAction::Continue(state)
		},
		3 => {
			cpu.pc = (cpu.pc as isize + state.relative as isize) as u16;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn tcall_13(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_x_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_addr_x_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_addr_y_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_indirect_y_a(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn mov_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_y_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn movw_dp_ya(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_x_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dec_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn cbne_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn daa(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn clrv(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_14(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn set1_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbs_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn mov_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn mov_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, #imm", cycle);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let immediate = cpu.read_next_pc(memory);
			cpu.a = immediate;
			cpu.set_negative(cpu.a);
			cpu.set_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_x_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn not1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_y_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_y_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn notc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn pop_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn sleep(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn beq(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_15(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn clr1_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn bbc_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_dp_indirect_y(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	todo!()
}
fn mov_x_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_x_dp_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_y_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn inc_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_y_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dbnz_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn stop(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
