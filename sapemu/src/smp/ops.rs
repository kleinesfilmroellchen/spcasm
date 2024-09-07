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
// On purpose in this file, to allow easier reading of cycle-by-cycle behavior of instructions.
#![allow(clippy::match_same_arms, clippy::cast_lossless)]

use log::trace;
use spcasm::sema::Register;

use super::Smp;
use crate::memory::Memory;
use crate::smp::{ProgramStatusWord, RunState};

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
	/// An 8-bit relative value, usually used for branch targets.
	relative: i8,
	/// A 16-bit address immediate.
	address:  u16,
	/// An 8-bit operand, usually the value stored to memory or loaded from memory.
	operand:  u8,
	operand2: u8,
}

impl InstructionInternalState {
	const fn with_operand2(mut self, operand2: u8) -> Self {
		self.operand2 = operand2;
		self
	}

	const fn with_operand(mut self, operand: u8) -> Self {
		self.operand = operand;
		self
	}

	const fn with_relative(mut self, relative: i8) -> Self {
		self.relative = relative;
		self
	}

	const fn with_address(mut self, address: u16) -> Self {
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
	mov_x_a,
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
	($assembly:expr, $cycle:expr, $cpu:expr) => {
		if $cycle == 0 {
			log::debug!(concat!("[{:04x}] ", $assembly), $cpu.pc - 1);
		}
	};
}

fn nop(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("nop", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Next,
		_ => unreachable!(),
	}
}
fn tcall_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall0", cycle, cpu);
	tcall::<0>(cpu, memory, cycle, state)
}
fn set1_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 0", cycle, cpu);
	set_clear_dp::<0, true>(cpu, memory, cycle, state)
}
fn bbs_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("bbs 0", cycle, cpu);
	branch_on_bit::<0, true>(cpu, memory, cycle, state)
}
fn or_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or a, (dp)", cycle, cpu);
	logic_op_a_dp(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or a, (addr)", cycle, cpu);
	logic_op_a_addr(cpu, memory, cycle, state, |a, b| a | b)
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
	debug_instruction!("bpl", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Sign }, false>(cpu, memory, cycle, state)
}
fn tcall_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall1", cycle, cpu);
	tcall::<1>(cpu, memory, cycle, state)
}
fn clr1_0(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 0", cycle, cpu);
	set_clear_dp::<0, false>(cpu, memory, cycle, state)
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
	debug_instruction!("dec x", cycle, cpu);

	dec_register::<{ Register::X }>(cpu, cycle)
}
fn cmp_x_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn jmp_indexed(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("jmp (addr+x)", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address_low = u16::from(cpu.read_next_pc(memory));
			MicroArchAction::Continue(state.with_address(address_low))
		},
		2 => {
			let address = u16::from(cpu.read_next_pc(memory)) << 8 | state.address;
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			// We don’t know whether the CPU can already read the first address byte in this cycle,
			// but doing the offset calculation in a separate cycle seems more plausible.
			let address_of_jump = state.address + u16::from(cpu.x);
			MicroArchAction::Continue(state.with_address(address_of_jump))
		},
		4 => {
			let pc_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(pc_low))
		},
		5 => {
			let pc_high = cpu.read(state.address.wrapping_add(1), memory);
			let new_pc = u16::from(pc_high) << 8 | u16::from(state.operand);
			trace!("jump to {:04x} (had X offset {:02x})", new_pc, cpu.x);
			cpu.pc = new_pc;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn clrp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall2", cycle, cpu);
	tcall::<2>(cpu, memory, cycle, state)
}
fn set1_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 1", cycle, cpu);
	set_clear_dp::<1, true>(cpu, memory, cycle, state)
}
fn bbs_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn and_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and a, (dp)", cycle, cpu);
	logic_op_a_dp(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and a, (addr)", cycle, cpu);
	logic_op_a_addr(cpu, memory, cycle, state, |a, b| a & b)
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
	debug_instruction!("bra", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let offset = cpu.read_next_pc(memory) as i8;
			trace!("taking branch to {:+}", offset);
			MicroArchAction::Continue(state.with_relative(offset))
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

fn bmi(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("bmi", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Sign }, true>(cpu, memory, cycle, state)
}
fn tcall_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall3", cycle, cpu);
	tcall::<3>(cpu, memory, cycle, state)
}
fn clr1_1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 1", cycle, cpu);
	set_clear_dp::<1, false>(cpu, memory, cycle, state)
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
	inc_register::<{ Register::X }>(cpu, cycle)
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
	debug_instruction!("tcall4", cycle, cpu);
	tcall::<4>(cpu, memory, cycle, state)
}
fn set1_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 2", cycle, cpu);
	set_clear_dp::<2, true>(cpu, memory, cycle, state)
}
fn bbs_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn eor_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor a, (dp)", cycle, cpu);
	logic_op_a_dp(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor a, (addr)", cycle, cpu);
	logic_op_a_addr(cpu, memory, cycle, state, |a, b| a ^ b)
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
	debug_instruction!("bvc", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Overflow }, false>(cpu, memory, cycle, state)
}
fn tcall_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall5", cycle, cpu);
	tcall::<5>(cpu, memory, cycle, state)
}
fn clr1_2(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 2", cycle, cpu);
	set_clear_dp::<2, false>(cpu, memory, cycle, state)
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
fn mov_x_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, a", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.x = cpu.a;
			cpu.set_negative_zero(cpu.x);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("tcall6", cycle, cpu);
	tcall::<6>(cpu, memory, cycle, state)
}
fn set1_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 3", cycle, cpu);
	set_clear_dp::<3, true>(cpu, memory, cycle, state)
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
	debug_instruction!("bvs", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Overflow }, true>(cpu, memory, cycle, state)
}
fn tcall_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall7", cycle, cpu);
	tcall::<7>(cpu, memory, cycle, state)
}
fn clr1_3(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 3", cycle, cpu);
	set_clear_dp::<3, false>(cpu, memory, cycle, state)
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
	debug_instruction!("cmp dp, #imm", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let immediate = cpu.read_next_pc(memory);
			MicroArchAction::Continue(state.with_operand(immediate))
		},
		2 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let operand2 = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand2(operand2))
		},
		4 => {
			let result = (state.operand2 as i8).wrapping_sub(state.operand as i8);
			trace!("cmp {:02x} - {:02x} = {:+}", state.operand2, state.operand, result);
			cpu.set_negative_zero(result as u8);
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
	debug_instruction!("cmp y, dp", cycle, cpu);

	cmp_register_dp::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn reti(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn setc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn tcall_8(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall8", cycle, cpu);
	tcall::<8>(cpu, memory, cycle, state)
}
fn set1_4(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 4", cycle, cpu);
	set_clear_dp::<4, true>(cpu, memory, cycle, state)
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
	debug_instruction!("mov y, #imm", cycle, cpu);
	move_from_imm::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn pop_psw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp, #imm", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let immediate = cpu.read_next_pc(memory);
			MicroArchAction::Continue(state.with_operand(immediate))
		},
		2 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
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
	debug_instruction!("tcall9", cycle, cpu);
	tcall::<9>(cpu, memory, cycle, state)
}
fn clr1_4(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 4", cycle, cpu);
	set_clear_dp::<4, false>(cpu, memory, cycle, state)
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
	debug_instruction!("dec a", cycle, cpu);

	dec_register::<{ Register::A }>(cpu, cycle)
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
	debug_instruction!("tcall10", cycle, cpu);
	tcall::<10>(cpu, memory, cycle, state)
}
fn set1_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 5", cycle, cpu);
	set_clear_dp::<5, true>(cpu, memory, cycle, state)
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
	debug_instruction!("inc dp", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory).wrapping_add(1);
			cpu.set_negative_zero(value);
			MicroArchAction::Continue(state.with_operand(value))
		},
		3 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn inc_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("inc addr", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address_low = u16::from(cpu.read_next_pc(memory));
			MicroArchAction::Continue(state.with_address(address_low))
		},
		2 => {
			let address = u16::from(cpu.read_next_pc(memory)) << 8 | state.address;
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value = cpu.read(state.address, memory).wrapping_add(1);
			cpu.set_negative_zero(value);
			MicroArchAction::Continue(state.with_operand(value))
		},
		4 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bcs", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Carry }, true>(cpu, memory, cycle, state)
}
fn tcall_11(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall11", cycle, cpu);
	tcall::<11>(cpu, memory, cycle, state)
}
fn clr1_5(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 5", cycle, cpu);
	set_clear_dp::<5, false>(cpu, memory, cycle, state)
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
#[allow(clippy::cast_lossless)]
fn movw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("movw ya, dp", cycle, cpu);

	// TODO: Memory access pattern of this instruction is not known.
	// Given that there's probably just one temporary address register, we assume:
	// 1: TAR <- address + DP offset
	// 2: A <- (TAR)
	// 3: TAR <- address + 1 + DP offset
	// 4: Y <- (TAR)
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let a_address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(a_address))
		},
		2 => {
			cpu.a = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		3 => {
			// wraparound at direct page boundary
			let y_address = (state.address + 1) % 0xff;
			MicroArchAction::Continue(state.with_address(y_address))
		},
		4 => {
			cpu.y = cpu.read(state.address, memory);

			// TODO: assume that negative and zero flags are set for the entire word, which is how I would design it :)
			let value_16bit = ((cpu.y as u16) << 8) | cpu.a as u16;
			cpu.psw.set(ProgramStatusWord::Sign, (value_16bit as i16) < 0);
			cpu.psw.set(ProgramStatusWord::Zero, value_16bit == 0);

			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn inc_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn inc_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	inc_register::<{ Register::A }>(cpu, cycle)
}
fn mov_sp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov sp, x", cycle, cpu);

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
	debug_instruction!("tcall12", cycle, cpu);
	tcall::<12>(cpu, memory, cycle, state)
}
fn set1_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 6", cycle, cpu);
	set_clear_dp::<6, true>(cpu, memory, cycle, state)
}
fn bbs_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp, a", cycle, cpu);

	move_to_dp::<{ Register::A }>(cpu, memory, cycle, state)
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
	debug_instruction!("mov (x), a", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			// Does indeed perform a dummy read according to fullsnes.
			let _ = cpu.read(u16::from(cpu.x) + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(InstructionInternalState::default())
		},
		2 => {
			// TODO: Not sure if this cycle actually does nothing?
			MicroArchAction::Continue(InstructionInternalState::default())
		},
		3 => {
			cpu.write(u16::from(cpu.x) + cpu.direct_page_offset(), cpu.a, memory);
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
	debug_instruction!("mov dp, y", cycle, cpu);

	move_to_dp::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn mov_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_x_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, #imm", cycle, cpu);
	move_from_imm::<{ Register::X }>(cpu, memory, cycle, state)
}
fn pop_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mul(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}

fn bne(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("bne", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Zero }, false>(cpu, memory, cycle, state)
}
fn tcall_13(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall13", cycle, cpu);
	tcall::<13>(cpu, memory, cycle, state)
}
fn clr1_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 6", cycle, cpu);
	set_clear_dp::<6, false>(cpu, memory, cycle, state)
}
fn bbc_6(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_dp_x_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp+x, a", cycle, cpu);
	todo!()
}
fn mov_addr_x_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov addr+x, a", cycle, cpu);
	todo!()
}
fn mov_addr_y_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov addr+y, a", cycle, cpu);
	todo!()
}
#[allow(clippy::cast_lossless)]
fn mov_dp_indirect_y_a(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("mov (dp)+y, a", cycle, cpu);

	// This one is very involved, since it needs to store A at byte[word[dp]+Y].
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			// read direct page address of 16-bit pointer
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			// read lower byte of 16-bit pointer
			let pointer_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(pointer_low))
		},
		3 => {
			// read upper byte of 16-bit pointer
			let pointer_high = cpu.read(state.address.wrapping_add(1), memory);
			MicroArchAction::Continue(state.with_operand2(pointer_high))
		},
		4 => {
			// calculate target address
			// TODO: Does this actually happen here? The cycle count implies it...
			let base_address = (state.operand as u16) | ((state.operand2 as u16) << 8);
			let indexed_address = base_address.wrapping_add(cpu.y as u16);
			trace!("write to [{:04x}+{:02x}] = [{:04x}]", base_address, cpu.y, indexed_address);
			MicroArchAction::Continue(state.with_address(indexed_address))
		},
		5 => {
			// issue dummy read, as with almost all stores
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		6 => {
			// write A to memory
			cpu.write(state.address, cpu.a, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp, x", cycle, cpu);

	move_to_dp::<{ Register::X }>(cpu, memory, cycle, state)
}
fn mov_dp_y_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn movw_dp_ya(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("movw dp, ya", cycle, cpu);

	// TODO: Memory access pattern of this instruction is not known.
	// Given that there's probably just one temporary address register, we assume:
	// 1: TAR <- address + DP offset
	// 2: A -> (TAR)
	// 3: TAR <- address + 1 + DP offset
	// 4: Y -> (TAR)
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let a_address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(a_address))
		},
		2 => {
			cpu.write(state.address, cpu.a, memory);
			MicroArchAction::Continue(state)
		},
		3 => {
			// wraparound at direct page boundary
			let y_address = (state.address + 1) % 0xff;
			MicroArchAction::Continue(state.with_address(y_address))
		},
		4 => {
			cpu.write(state.address, cpu.y, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_dp_x_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dec_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec y", cycle, cpu);

	dec_register::<{ Register::Y }>(cpu, cycle)
}
fn mov_a_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, y", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.a = cpu.y;
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("tcall14", cycle, cpu);
	tcall::<14>(cpu, memory, cycle, state)
}
fn set1_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("set1 7", cycle, cpu);
	set_clear_dp::<7, true>(cpu, memory, cycle, state)
}
fn bbs_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, dp", cycle, cpu);

	move_from_dp::<{ Register::A }>(cpu, memory, cycle, state)
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
	debug_instruction!("mov a, #imm", cycle, cpu);
	move_from_imm::<{ Register::A }>(cpu, memory, cycle, state)
}
fn mov_x_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn not1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn mov_y_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov y, dp", cycle, cpu);

	move_from_dp::<{ Register::Y }>(cpu, memory, cycle, state)
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
	debug_instruction!("beq", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Zero }, true>(cpu, memory, cycle, state)
}
fn tcall_15(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tcall15", cycle, cpu);
	tcall::<15>(cpu, memory, cycle, state)
}
fn clr1_7(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clr1 7", cycle, cpu);
	set_clear_dp::<7, false>(cpu, memory, cycle, state)
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
	debug_instruction!("mov x, dp", cycle, cpu);

	move_from_dp::<{ Register::X }>(cpu, memory, cycle, state)
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
	debug_instruction!("inc y", cycle, cpu);
	inc_register::<{ Register::Y }>(cpu, cycle)
}
fn mov_y_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn dbnz_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	todo!()
}
fn stop(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("stop", cycle, cpu);

	match cycle {
		0 => {
			cpu.run_state = RunState::Halted;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

/// Common implementation for all conditional branches. The flag to branch on and its state when to branch are constant
/// parameters since they are determined at compile time, hopefully improving optimizations around flag checks.
#[inline]
#[allow(clippy::needless_pass_by_ref_mut)]
fn branch_on<const FLAG: ProgramStatusWord, const IS_SET: bool>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let offset = cpu.read_next_pc(memory) as i8;
			// branch if flag set or cleared as the caller requested
			if cpu.psw.contains(FLAG) == IS_SET {
				trace!("flag {} in {}: {}, taking branch to {:+}", FLAG, cpu.psw, cpu.psw.contains(FLAG), offset);
				MicroArchAction::Continue(state.with_relative(offset))
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

#[inline]
fn move_to_dp<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		3 => {
			cpu.write(state.address, cpu.register_read::<REGISTER>(), memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
#[allow(clippy::needless_pass_by_ref_mut)]
fn move_from_dp<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let a_address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(a_address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			cpu.register_write::<REGISTER>(value);
			cpu.set_negative_zero(value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
#[allow(clippy::needless_pass_by_ref_mut)]
fn move_from_imm<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let value = cpu.read_next_pc(memory);
			cpu.register_write::<REGISTER>(value);
			cpu.set_negative_zero(value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
#[allow(clippy::needless_pass_by_ref_mut)]
fn cmp_register_dp<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let operand = cpu.read(state.address, memory);
			let register = cpu.register_read::<REGISTER>();
			let result = (register as i8).wrapping_sub(operand as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(register as i8, operand as i8);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", register, operand, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn dec_register<const REGISTER: Register>(cpu: &mut Smp, cycle: usize) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.register_write::<REGISTER>(cpu.register_read::<REGISTER>().wrapping_sub(1));
			cpu.set_negative_zero(cpu.register_read::<REGISTER>());
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn inc_register<const REGISTER: Register>(cpu: &mut Smp, cycle: usize) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.register_write::<REGISTER>(cpu.register_read::<REGISTER>().wrapping_add(1));
			cpu.set_negative_zero(cpu.register_read::<REGISTER>());
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn tcall<const INDEX: u8>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction
where
	[(); (INDEX + (0xff - 15)) as usize]:, // Only indices between 0 and 15 inclusive are allowed
{
	let call_address = 0xFFDE - u16::from(INDEX) * 2;
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			// return address ends up in little endian order, but since stack grows downwards, we first push the upper 8
			// bits.
			cpu.push(((cpu.pc >> 8) & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		// wait for the stack pointer decrement that hardware has to do
		2 => MicroArchAction::Continue(state),
		3 => {
			cpu.push((cpu.pc & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		4 => MicroArchAction::Continue(state),
		5 => {
			let lower_target_address = cpu.read(call_address, memory);
			MicroArchAction::Continue(state.with_address(u16::from(lower_target_address)))
		},
		6 => {
			let upper_target_address = u16::from(cpu.read(call_address + 1, memory));
			let target_address = state.address | upper_target_address << 8;
			trace!("tcall {INDEX} got call target {target_address:04x} (from {call_address:04x})");
			MicroArchAction::Continue(state.with_address(target_address))
		},
		7 => {
			cpu.pc = state.address;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn set_clear_dp<const BIT_INDEX: u8, const SHOULD_SET: bool>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction
where
	[(); (BIT_INDEX + (0xff - 7)) as usize]:, // Only indices between 0 and 7 inclusive are allowed
{
	let bit_value: u8 = u8::from(SHOULD_SET);
	let bitmask = !(1 << BIT_INDEX);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			let modified = value & bitmask | (bit_value << BIT_INDEX);
			MicroArchAction::Continue(state.with_operand(modified))
		},
		3 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn branch_on_bit<const BIT_INDEX: u8, const BRANCH_IF_SET: bool>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction
where
	[(); (BIT_INDEX + (0xff - 7)) as usize]:, // Only indices between 0 and 7 inclusive are allowed
{
	let bit_value: u8 = u8::from(BRANCH_IF_SET);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(value))
		},
		3 => {
			let relative_offset = cpu.read_next_pc(memory) as i8;
			MicroArchAction::Continue(state.with_relative(relative_offset))
		},
		4 =>
			if (state.operand >> BIT_INDEX) == bit_value {
				trace!(
					"bit {BIT_INDEX} {} in {} ({:08b}), taking branch",
					if BRANCH_IF_SET { "set" } else { "clear" },
					state.operand,
					state.operand
				);
				MicroArchAction::Next
			} else {
				MicroArchAction::Continue(state)
			},
		// CPU calculates branch target in this step.
		5 => MicroArchAction::Continue(state),
		6 => {
			cpu.pc = (cpu.pc as isize + state.relative as isize) as u16;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_a_dp(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			let result = op(cpu.a, value);
			cpu.a = result;
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_a_addr(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address_low = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address_low))
		},
		2 => {
			let address_high = cpu.read_next_pc(memory) as u16;
			let address = address_high << 8 | state.address;
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			let result = op(cpu.a, value);
			cpu.a = result;
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
