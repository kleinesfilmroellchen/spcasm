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

use spcasm::sema::Register;

use super::Smp;
use crate::memory::Memory;
use crate::smp::{ProgramStatusWord, RunState, BREAK_VECTOR};
use crate::trace;

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
	address2: u16,
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

	const fn with_address2(mut self, address2: u16) -> Self {
		self.address2 = address2;
		self
	}
}

/// Function that implements an instruction.
///
/// Arguments are:
/// - the CPU to operate on
/// - the memory to access (only one memory read or write is possible per cycle in hardware, so except for debugging
///   purposes, only one memory access should be performed)
/// - the current cycle within the instruction, necessary for intra-instruction cycle timing
/// - the state carried over from the previous cycle, necessary for keeping track of intermediate values
///
/// Output is a micro-architectural action for the processor to take, which influences the control flow in the CPU
/// calling code.
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
	bcc,
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
		#[cfg(debug_assertions)]
		if $cycle == 0 {
			log::debug!(concat!("[{:04x}] ", $assembly), $cpu.pc.wrapping_sub(1));
		}
	};
}

fn nop(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("nop", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(state),
		1 => MicroArchAction::Next,
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
	debug_instruction!("or a, (x)", cycle, cpu);
	logic_op_a_x_indirect(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("or a, (dp+x)", cycle, cpu);
	logic_op_a_dp_x_indirect(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or a, #imm", cycle, cpu);
	logic_op_a_imm(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or (dp), (dp)", cycle, cpu);
	logic_op_dp_dp(cpu, memory, cycle, state, |a, b| a | b)
}
fn or1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or1 c, addr.bit", cycle, cpu);
	bit_logic_addr::<false>(cpu, memory, cycle, state, |carry, bit| carry || bit)
}
fn asl_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("asl (dp)", cycle, cpu);
	shift_dp(cpu, memory, cycle, state, |value, _| ((value << 1), value & 0b1000_0000 > 0))
}
fn asl_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("asl (addr)", cycle, cpu);
	shift_addr(cpu, memory, cycle, state, |value, _| ((value << 1), value & 0b1000_0000 > 0))
}
fn push_psw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("push psw", cycle, cpu);
	push::<{ Register::PSW }>(cpu, memory, cycle, state)
}
fn tset1_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tset1 addr", cycle, cpu);
	test1(cpu, memory, cycle, state, |value, a| value | a)
}
fn brk(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("brk", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.push(((cpu.pc >> 8) & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		2 => {
			cpu.push((cpu.pc & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		3 => {
			cpu.push(cpu.psw.bits(), memory);
			cpu.set_interrupt(false);
			cpu.set_break(true);
			MicroArchAction::Continue(state)
		},
		4 => MicroArchAction::Continue(state),
		5 => {
			let low_address = cpu.read(BREAK_VECTOR, memory);
			MicroArchAction::Continue(state.with_address(low_address as u16))
		},
		6 => {
			let address = (cpu.read(BREAK_VECTOR + 1, memory) as u16) << 8 | state.address;
			MicroArchAction::Continue(state.with_address(address))
		},
		7 => {
			cpu.pc = state.address;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbc 0", cycle, cpu);
	branch_on_bit::<0, false>(cpu, memory, cycle, state)
}
fn or_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or a, dp+x", cycle, cpu);
	logic_op_a_dp_x(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or a, addr+x", cycle, cpu);
	logic_op_a_addr_register::<{ Register::X }>(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or a, addr+y", cycle, cpu);
	logic_op_a_addr_register::<{ Register::Y }>(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("or a, (dp)+y", cycle, cpu);
	logic_op_a_dp_y_indirect(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or dp, #imm", cycle, cpu);
	logic_op_dp_imm(cpu, memory, cycle, state, |a, b| a | b)
}
fn or_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("or (x), (y)", cycle, cpu);
	logic_op_x_y_indirect(cpu, memory, cycle, state, |a, b| a | b)
}
fn decw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("decw (dp)", cycle, cpu);
	inc_dec_word(cpu, memory, cycle, state, |value| value.wrapping_sub(1))
}
fn asl_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("asl (dp+x)", cycle, cpu);
	shift_dp_x(cpu, memory, cycle, state, |value, _| ((value << 1), value & 0b1000_0000 > 0))
}
fn asl_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("asl a", cycle, cpu);
	shift_register::<{ Register::A }>(cpu, memory, cycle, state, |value, _| ((value << 1), value & 0b1000_0000 > 0))
}
fn dec_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec x", cycle, cpu);
	inc_dec_register::<{ Register::X }>(cpu, cycle, |value| value.wrapping_sub(1))
}
fn cmp_x_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp x, addr", cycle, cpu);
	cmp_register_addr::<{ Register::X }>(cpu, memory, cycle, state)
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
			let address_of_jump = state.address.wrapping_add(u16::from(cpu.x));
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
	debug_instruction!("clrp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(state),
		1 => {
			cpu.psw.remove(ProgramStatusWord::DirectPage);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbs 1", cycle, cpu);
	branch_on_bit::<1, true>(cpu, memory, cycle, state)
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
	debug_instruction!("and a, (x)", cycle, cpu);
	logic_op_a_x_indirect(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("and a, (dp+x)", cycle, cpu);
	logic_op_a_dp_x_indirect(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and a, #imm", cycle, cpu);
	logic_op_a_imm(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and (dp), (dp)", cycle, cpu);
	logic_op_dp_dp(cpu, memory, cycle, state, |a, b| a & b)
}
fn or1_inverted(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or1 c, !addr.bit", cycle, cpu);
	bit_logic_addr::<false>(cpu, memory, cycle, state, |carry, bit| carry || !bit)
}
fn rol_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("rol (dp)", cycle, cpu);
	shift_dp(cpu, memory, cycle, state, |value, carry| ((value << 1) | (carry as u8), value & 0b1000_0000 > 0))
}
fn rol_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("rol (addr)", cycle, cpu);
	shift_addr(cpu, memory, cycle, state, |value, carry| ((value << 1) | (carry as u8), value & 0b1000_0000 > 0))
}
fn push_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("push a", cycle, cpu);
	push::<{ Register::A }>(cpu, memory, cycle, state)
}
fn cbne(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cbne (dp)", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset = cpu.read_next_pc(memory) as i8;
			MicroArchAction::Continue(state.with_relative(offset))
		},
		3 => {
			let operand = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(operand))
		},
		4 =>
			if state.operand == cpu.a {
				MicroArchAction::Next
			} else {
				trace!("taking branch to {:+}", state.relative);
				MicroArchAction::Continue(state)
			},
		5 => {
			// Hardware calculates branch target in this step, we do everything at the very end since memory accesses
			// don't happen.
			MicroArchAction::Continue(state)
		},
		6 => {
			cpu.pc = (cpu.pc as isize + state.relative as isize) as u16;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbc 1", cycle, cpu);
	branch_on_bit::<1, false>(cpu, memory, cycle, state)
}
fn and_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and a, dp+x", cycle, cpu);
	logic_op_a_dp_x(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and a, addr+x", cycle, cpu);
	logic_op_a_addr_register::<{ Register::X }>(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and a, addr+y", cycle, cpu);
	logic_op_a_addr_register::<{ Register::Y }>(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("and a, (dp)+y", cycle, cpu);
	logic_op_a_dp_y_indirect(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and dp, #imm", cycle, cpu);
	logic_op_dp_imm(cpu, memory, cycle, state, |a, b| a & b)
}
fn and_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("and (x), (y)", cycle, cpu);
	logic_op_x_y_indirect(cpu, memory, cycle, state, |a, b| a & b)
}
fn incw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("incw (dp)", cycle, cpu);
	inc_dec_word(cpu, memory, cycle, state, |value| value.wrapping_add(1))
}
fn rol_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("rol (dp+x)", cycle, cpu);
	shift_dp_x(cpu, memory, cycle, state, |value, carry| ((value << 1) | (carry as u8), value & 0b1000_0000 > 0))
}
fn rol_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("rol a", cycle, cpu);
	shift_register::<{ Register::A }>(cpu, memory, cycle, state, |value, carry| {
		((value << 1) | (carry as u8), value & 0b1000_0000 > 0)
	})
}
fn inc_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("inc x", cycle, cpu);
	inc_dec_register::<{ Register::X }>(cpu, cycle, |value| value.wrapping_add(1))
}
fn cmp_x_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp x, dp", cycle, cpu);
	cmp_register_dp::<{ Register::X }>(cpu, memory, cycle, state)
}
fn call(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("call", cycle, cpu);
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
			// return address ends up in little endian order, but since stack grows downwards, we first push the upper 8
			// bits.
			cpu.push(((cpu.pc >> 8) & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		4 => MicroArchAction::Continue(state),
		5 => {
			cpu.push((cpu.pc & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		6 => MicroArchAction::Continue(state),
		7 => {
			trace!("[{:04x}] call to {:04x}, stack size {}", cpu.pc, state.address, 0xff - cpu.sp);
			cpu.pc = state.address;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn setp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("setp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(state),
		1 => {
			cpu.psw.insert(ProgramStatusWord::DirectPage);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbs 2", cycle, cpu);
	branch_on_bit::<2, true>(cpu, memory, cycle, state)
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
	debug_instruction!("eor a, (x)", cycle, cpu);
	logic_op_a_x_indirect(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("eor a, (dp+x)", cycle, cpu);
	logic_op_a_dp_x_indirect(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor a, #imm", cycle, cpu);
	logic_op_a_imm(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("or (dp), (dp)", cycle, cpu);
	logic_op_dp_dp(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn and1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and1 c, addr.bit", cycle, cpu);
	bit_logic_addr::<true>(cpu, memory, cycle, state, |carry, bit| carry && bit)
}
fn lsr_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("lsr (dp)", cycle, cpu);
	shift_dp(cpu, memory, cycle, state, |value, _| (value >> 1, value & 1 > 0))
}
fn lsr_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("lsr (addr)", cycle, cpu);
	shift_addr(cpu, memory, cycle, state, |value, _| (value >> 1, value & 1 > 0))
}
fn push_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("push x", cycle, cpu);
	push::<{ Register::X }>(cpu, memory, cycle, state)
}
fn tclr1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("tclr1 addr", cycle, cpu);
	test1(cpu, memory, cycle, state, |value, a| value & !a)
}
fn pcall(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("pcall", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address_low = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address_low))
		},
		2 => {
			// return address ends up in little endian order, but since stack grows downwards, we first push the upper 8
			// bits.
			cpu.push(((cpu.pc >> 8) & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		3 => MicroArchAction::Continue(state),
		4 => {
			cpu.push((cpu.pc & 0xff) as u8, memory);
			MicroArchAction::Continue(state)
		},
		5 => {
			trace!("[{:04x}] pcall to ff{:02x}, stack size {}", cpu.pc, state.address, 0xff - cpu.sp);
			cpu.pc = state.address | 0xff00;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbc 2", cycle, cpu);
	branch_on_bit::<2, false>(cpu, memory, cycle, state)
}
fn eor_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor a, dp+x", cycle, cpu);
	logic_op_a_dp_x(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor a, addr+x", cycle, cpu);
	logic_op_a_addr_register::<{ Register::X }>(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor a, addr+y", cycle, cpu);
	logic_op_a_addr_register::<{ Register::Y }>(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("eor a, (dp)+y", cycle, cpu);
	logic_op_a_dp_y_indirect(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor dp, #imm", cycle, cpu);
	logic_op_dp_imm(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn eor_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("eor (x), (y)", cycle, cpu);
	logic_op_x_y_indirect(cpu, memory, cycle, state, |a, b| a ^ b)
}
fn cmpw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmpw ya, dp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let low_byte = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(low_byte as u16))
		},
		3 => {
			let high_byte = cpu.read(increment_wrap_within_page(state.address), memory);
			let memory_value = (high_byte as u16) << 8 | state.address2;

			let ya = cpu.ya();
			let expanded_result = (ya as u32).wrapping_add((!memory_value) as u32) + 1;
			let result = (expanded_result & 0xffff) as u16;
			cpu.psw.set(ProgramStatusWord::Sign, (result as i16) < 0);
			cpu.psw.set(ProgramStatusWord::Zero, result == 0);
			cpu.psw.set(ProgramStatusWord::Carry, expanded_result >= 0x1_0000);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn lsr_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("lsr (dp+x)", cycle, cpu);
	shift_dp_x(cpu, memory, cycle, state, |value, _| (value >> 1, value & 1 > 0))
}
fn lsr_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("lsr a", cycle, cpu);
	shift_register::<{ Register::A }>(cpu, memory, cycle, state, |value, _| (value >> 1, value & 1 > 0))
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
	debug_instruction!("cmp y, addr", cycle, cpu);
	cmp_register_addr::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn jmp_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("jmp addr", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address_low = u16::from(cpu.read_next_pc(memory));
			MicroArchAction::Continue(state.with_address(address_low))
		},
		2 => {
			let address = u16::from(cpu.read_next_pc(memory)) << 8 | state.address;
			trace!("jump to {:04x}", address);
			cpu.pc = address;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn clrc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clrc", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(state),
		1 => {
			cpu.psw.remove(ProgramStatusWord::Carry);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbs 3", cycle, cpu);
	branch_on_bit::<3, true>(cpu, memory, cycle, state)
}
fn cmp_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp a, dp", cycle, cpu);
	cmp_register_dp::<{ Register::A }>(cpu, memory, cycle, state)
}
fn cmp_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp a, addr", cycle, cpu);
	cmp_register_addr::<{ Register::A }>(cpu, memory, cycle, state)
}
fn cmp_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("cmp a, (x)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		// TODO: not sure what the CPU does in this cycle.
		1 => {
			let address = cpu.x as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let operand = cpu.read(state.address, memory);
			let result = (cpu.a as i8).wrapping_sub(operand as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(cpu.a, operand);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", cpu.a, operand, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn cmp_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("cmp a, (dp+x)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let operand = cpu.read(state.address2, memory);
			let result = (cpu.a as i8).wrapping_sub(operand as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(cpu.a, operand);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", cpu.a, operand, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn cmp_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp a, #imm", cycle, cpu);
	cmp_register_imm::<{ Register::A }>(cpu, memory, cycle, state)
}
fn cmp_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp (dp), (dp)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address2 = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address2(address2))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value2 = cpu.read(state.address2, memory);
			MicroArchAction::Continue(state.with_operand2(value2))
		},
		4 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(value))
		},
		5 => {
			let result = (state.operand as i8).wrapping_sub(state.operand2 as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(state.operand, state.operand2);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", state.operand, state.operand2, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn and1_inverted(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("and1 c, !addr.bit", cycle, cpu);
	bit_logic_addr::<true>(cpu, memory, cycle, state, |carry, bit| carry && !bit)
}
fn ror_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("ror (dp)", cycle, cpu);
	shift_dp(cpu, memory, cycle, state, |value, carry| (value >> 1 | (carry as u8) << 7, value & 1 > 0))
}
fn ror_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("ror (addr)", cycle, cpu);
	shift_addr(cpu, memory, cycle, state, |value, carry| (value >> 1 | (carry as u8) << 7, value & 1 > 0))
}
fn push_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("push y", cycle, cpu);
	push::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn dbnz_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dbnz (dp)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			let decremented = value.wrapping_sub(1);
			MicroArchAction::Continue(state.with_operand(decremented))
		},
		3 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Continue(state)
		},
		4 => {
			let offset = cpu.read_next_pc(memory) as i8;
			if state.operand == 0 {
				MicroArchAction::Next
			} else {
				trace!(
					"decremented {:04x} to non-zero value {}, taking branch to {:+x}",
					state.address,
					state.operand,
					offset
				);
				MicroArchAction::Continue(state.with_relative(offset))
			}
		},
		5 => {
			// Hardware calculates branch target in this step, we do everything at the very end since memory accesses
			// don't happen.
			MicroArchAction::Continue(state)
		},
		6 => {
			cpu.pc = (cpu.pc as isize + state.relative as isize) as u16;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn ret(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("ret", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let pc_low = cpu.pop(memory);
			MicroArchAction::Continue(state.with_address(pc_low as u16))
		},
		2 => MicroArchAction::Continue(state),
		3 => {
			let pc_high = cpu.pop(memory);
			MicroArchAction::Continue(state.with_address(u16::from(pc_high) << 8 | state.address))
		},
		4 => {
			trace!("[{:04x}] return to {:04x}, stack size {}", cpu.pc, state.address, 0xff - cpu.sp);
			cpu.pc = state.address;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbc 3", cycle, cpu);
	branch_on_bit::<3, false>(cpu, memory, cycle, state)
}
fn cmp_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp a, dp+x", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let operand = cpu.read(state.address, memory);
			let result = (cpu.a as i8).wrapping_sub(operand as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(cpu.a, operand);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", cpu.a, operand, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn cmp_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp a, addr+x", cycle, cpu);
	cmp_a_addr_register::<{ Register::X }>(cpu, memory, cycle, state)
}
fn cmp_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp a, addr+y", cycle, cpu);
	cmp_a_addr_register::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn cmp_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("cmp a, (dp+y)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = state.address + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2.wrapping_add(cpu.y as u16), memory);
			let result = (cpu.a as i8).wrapping_sub(value as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(cpu.a, value);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", cpu.a, value, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
			cpu.set_subtract_carry(state.operand2, state.operand);
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
	debug_instruction!("cmp (x), (y)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let operand = cpu.read(cpu.x as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand(operand))
		},
		2 => {
			let operand2 = cpu.read(cpu.y as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand2(operand2))
		},
		3 => {
			let result = (state.operand as i8).wrapping_sub(state.operand2 as i8);
			trace!("cmp {:02x} - {:02x} = {:+}", state.operand, state.operand2, result);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(state.operand, state.operand2);
			MicroArchAction::Continue(state)
		},
		4 => MicroArchAction::Next,
		_ => unreachable!(),
	}
}
fn addw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("addw ya, dp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let low_byte = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(low_byte as u16))
		},
		3 => {
			let high_byte = cpu.read(increment_wrap_within_page(state.address), memory);
			let memory_value = (high_byte as u16) << 8 | state.address2;

			let ya = cpu.ya();
			let (result, has_carry) = ya.overflowing_add(memory_value);

			cpu.psw.set(ProgramStatusWord::Sign, (result as i16) < 0);
			cpu.psw.set(ProgramStatusWord::Zero, result == 0);
			cpu.psw.set(ProgramStatusWord::Carry, has_carry);
			cpu.psw.set(ProgramStatusWord::Overflow, (ya as i16).overflowing_add(memory_value as i16).1);

			let half_carry_result = (ya & 0x0fff) + (memory_value & 0x0fff) >= 0x1000;
			cpu.psw.set(ProgramStatusWord::HalfCarry, half_carry_result);

			cpu.y = ((result >> 8) & 0xff) as u8;
			cpu.a = (result & 0xff) as u8;
			MicroArchAction::Continue(state)
		},
		4 => MicroArchAction::Next,
		_ => unreachable!(),
	}
}
fn ror_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("ror (dp+x)", cycle, cpu);
	shift_dp_x(cpu, memory, cycle, state, |value, carry| (value >> 1 | (carry as u8) << 7, value & 1 > 0))
}
fn ror_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("ror a", cycle, cpu);
	shift_register::<{ Register::A }>(cpu, memory, cycle, state, |value, carry| {
		(value >> 1 | (carry as u8) << 7, value & 1 > 0)
	})
}
fn mov_a_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, x", cycle, cpu);
	mov_register_register::<{ Register::A }, { Register::X }>(cpu, memory, cycle, state)
}
fn cmp_y_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp y, dp", cycle, cpu);
	cmp_register_dp::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn reti(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("reti", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.psw = ProgramStatusWord(cpu.pop(memory));
			MicroArchAction::Continue(state)
		},
		2 => {
			let pc_low = cpu.pop(memory);
			MicroArchAction::Continue(state.with_address(pc_low as u16))
		},
		3 => MicroArchAction::Continue(state),
		4 => {
			let pc_high = cpu.pop(memory);
			MicroArchAction::Continue(state.with_address(u16::from(pc_high) << 8 | state.address))
		},
		5 => {
			trace!("[{:04x}] return to {:04x}, stack size {}", cpu.pc, state.address, 0xff - cpu.sp);
			cpu.pc = state.address;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn setc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("setc", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(state),
		1 => {
			cpu.psw.insert(ProgramStatusWord::Carry);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbs 4", cycle, cpu);
	branch_on_bit::<4, true>(cpu, memory, cycle, state)
}
fn adc_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc a, dp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc a, addr", cycle, cpu);
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
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("adc a, (x)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.x as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("adc a, (dp+x)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2, memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc a, #imm", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let value = cpu.read_next_pc(memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc (dp), (dp)", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address2 = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address2(address2))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value2 = cpu.read(state.address2, memory);
			MicroArchAction::Continue(state.with_operand2(value2))
		},
		4 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(value))
		},
		5 => {
			trace!("{} + {} + {}", state.operand, state.operand2, cpu.carry());
			let result = state.operand.wrapping_add(state.operand2).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(state.operand, state.operand2);
			cpu.write(state.address, result, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn eor1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("eor1 c, addr.bit", cycle, cpu);
	bit_logic_addr::<false>(cpu, memory, cycle, state, |carry, bit| carry ^ bit)
}
fn dec_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec dp", cycle, cpu);
	inc_dec_dp(cpu, memory, cycle, state, |value| value.wrapping_sub(1))
}
fn dec_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec addr", cycle, cpu);
	inc_dec_addr(cpu, memory, cycle, state, |value| value.wrapping_sub(1))
}
fn mov_y_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov y, #imm", cycle, cpu);
	move_from_imm::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn pop_psw(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("pop psw", cycle, cpu);
	pop::<{ Register::PSW }>(cpu, memory, cycle, state)
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

fn bcc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("bcc", cycle, cpu);
	branch_on::<{ ProgramStatusWord::Carry }, false>(cpu, memory, cycle, state)
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
	debug_instruction!("bbc 4", cycle, cpu);
	branch_on_bit::<4, false>(cpu, memory, cycle, state)
}
fn adc_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc a, dp+x", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc a, addr+x", cycle, cpu);

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
			let offset_address = state.address.wrapping_add(cpu.x as u16);
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		4 => {
			let value = cpu.read(state.address, memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc a, addr+y", cycle, cpu);

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
			let offset_address = state.address.wrapping_add(cpu.y as u16);
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		4 => {
			let value = cpu.read(state.address, memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("adc a, (dp)+y", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = state.address + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2.wrapping_add(cpu.y as u16), memory);
			trace!("{} + {} + {}", cpu.a, value, cpu.carry());
			let result = cpu.a.wrapping_add(value).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(cpu.a, value);
			cpu.a = result;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("adc (dp), #imm", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let immediate = cpu.read_next_pc(memory);
			MicroArchAction::Continue(state.with_operand(immediate))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand2(value))
		},
		4 => {
			trace!("{} + {} + {}", state.operand2, state.operand, cpu.carry());
			let result = state.operand2.wrapping_add(state.operand).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(state.operand2, state.operand);
			cpu.write(state.address, result, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn adc_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let operand = cpu.read(cpu.x as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand(operand))
		},
		2 => {
			let operand2 = cpu.read(cpu.y as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand2(operand2))
		},
		3 => {
			trace!("{} + {} + {}", state.operand, state.operand2, cpu.carry());
			let result = state.operand.wrapping_add(state.operand2).wrapping_add(cpu.carry() as u8);
			cpu.set_add_carry_flags(state.operand, state.operand2);
			MicroArchAction::Continue(state.with_operand(result))
		},
		4 => {
			cpu.write(cpu.x as u16 + cpu.direct_page_offset(), state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn subw_ya_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("subw ya, dp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let low_byte = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(low_byte as u16))
		},
		3 => {
			let high_byte = cpu.read(increment_wrap_within_page(state.address), memory);
			let memory_value = (high_byte as u16) << 8 | state.address2;

			let ya = cpu.ya();
			let expanded_result = (ya as u32).wrapping_add((!memory_value) as u32) + 1;
			let result = (expanded_result & 0xffff) as u16;
			cpu.psw.set(ProgramStatusWord::Sign, (result as i16) < 0);
			cpu.psw.set(ProgramStatusWord::Zero, result == 0);
			cpu.psw.set(ProgramStatusWord::Carry, expanded_result >= 0x1_0000);
			cpu.psw.set(ProgramStatusWord::Overflow, (ya as i16).overflowing_sub(memory_value as i16).1);

			let half_carry_result = (ya & 0x0fff) + (((!memory_value).wrapping_add(1)) & 0x0fff) >= 0x1000;
			cpu.psw.set(ProgramStatusWord::HalfCarry, half_carry_result);

			cpu.y = ((result >> 8) & 0xff) as u8;
			cpu.a = (result & 0xff) as u8;
			MicroArchAction::Continue(state)
		},
		4 => MicroArchAction::Next,
		_ => unreachable!(),
	}
}
fn dec_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec dp+x", cycle, cpu);
	inc_dec_dp_x(cpu, memory, cycle, state, |value| value.wrapping_sub(1))
}
fn dec_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec a", cycle, cpu);
	inc_dec_register::<{ Register::A }>(cpu, cycle, |value| value.wrapping_sub(1))
}
fn mov_x_sp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, sp", cycle, cpu);
	mov_register_register::<{ Register::X }, { Register::SP }>(cpu, memory, cycle, state)
}
fn div(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("div", cycle, cpu);
	match cycle {
		// hardware is slow.
		0 ..= 10 => MicroArchAction::Continue(state),
		11 => {
			// what the fuck is this code.
			let mut ya = cpu.ya() as u32;
			let subtractor = (cpu.x as u32) << 9;
			for _ in 0 .. 9 {
				ya = if (ya & 0x10000) > 0 { ((ya << 1) | 1) & 0x1FFFF } else { (ya << 1) & 0x1FFFF };

				if ya >= subtractor {
					ya ^= 1;
				}

				if (ya & 1) == 1 {
					ya = ya.wrapping_sub(subtractor) & 0x1FFFF;
				}
			}
			let div = (ya & 0xff) as u8;
			let modulus = ((ya >> 9) & 0xff) as u8;

			trace!("{} / {} = {div} mod {modulus}", cpu.ya(), cpu.x);

			cpu.psw.set(ProgramStatusWord::Overflow, ya & 0x100 > 0);
			cpu.psw.set(ProgramStatusWord::Zero, div == 0);
			cpu.psw.set(ProgramStatusWord::Sign, (div as i8) < 0);
			cpu.psw.set(ProgramStatusWord::HalfCarry, cpu.y & 0xf >= cpu.x & 0xf);

			cpu.a = div;
			cpu.y = modulus;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn xcn(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("xcn", cycle, cpu);
	match cycle {
		0 ..= 3 => MicroArchAction::Continue(state),
		4 => {
			cpu.a = (cpu.a & 0xf) << 4 | (cpu.a & 0xf0) >> 4;
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn ei(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("ei", cycle, cpu);

	match cycle {
		0 ..= 1 => MicroArchAction::Continue(state),
		2 => {
			cpu.psw.insert(ProgramStatusWord::Interrupt);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbs 5", cycle, cpu);
	branch_on_bit::<5, true>(cpu, memory, cycle, state)
}
fn sbc_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc a, dp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			cpu.a = cpu.perform_sub_carry(cpu.a, value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc a, addr", cycle, cpu);

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
			cpu.a = cpu.perform_sub_carry(cpu.a, value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("sbc a, (x)", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.x as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			cpu.a = cpu.perform_sub_carry(cpu.a, value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("sbc a, (dp+x)", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2, memory);
			cpu.a = cpu.perform_sub_carry(cpu.a, value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc a, #imm", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let value = cpu.read_next_pc(memory);
			cpu.a = cpu.perform_sub_carry(cpu.a, value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc dp, dp", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address2 = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address2(address2))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value2 = cpu.read(state.address2, memory);
			MicroArchAction::Continue(state.with_operand2(value2))
		},
		4 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(value))
		},
		5 => {
			let result = cpu.perform_sub_carry(state.operand, state.operand2);
			cpu.write(state.address, result, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov1_c_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov1 c, addr.bit", cycle, cpu);
	bit_logic_addr::<true>(cpu, memory, cycle, state, |carry, bit| bit)
}
fn inc_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("inc dp", cycle, cpu);
	inc_dec_dp(cpu, memory, cycle, state, |value| value.wrapping_add(1))
}
fn inc_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("inc addr", cycle, cpu);
	inc_dec_addr(cpu, memory, cycle, state, |value| value.wrapping_add(1))
}
fn cmp_y_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp y, #imm", cycle, cpu);
	cmp_register_imm::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn pop_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("pop a", cycle, cpu);
	pop::<{ Register::A }>(cpu, memory, cycle, state)
}
fn mov_x_inc_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov (x+), a", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			// Does indeed perform a dummy read according to fullsnes.
			let _ = cpu.read(u16::from(cpu.x) + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(InstructionInternalState::default())
		},
		2 => MicroArchAction::Continue(InstructionInternalState::default()),
		3 => {
			cpu.write(u16::from(cpu.x) + cpu.direct_page_offset(), cpu.a, memory);
			cpu.x = cpu.x.wrapping_add(1);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbc 5", cycle, cpu);
	branch_on_bit::<5, false>(cpu, memory, cycle, state)
}
fn sbc_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc a, dp+x", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			cpu.a = cpu.perform_sub_carry(cpu.a, value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc a, addr+x", cycle, cpu);
	sbc_a_addr_register::<{ Register::X }>(cpu, memory, cycle, state)
}
fn sbc_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc a, addr+y", cycle, cpu);
	sbc_a_addr_register::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn sbc_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("sbc a, (dp)+y", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = state.address + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2.wrapping_add(cpu.y as u16), memory);
			cpu.a = cpu.perform_sub_carry(cpu.a, value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_dp_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sbc dp, #imm", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let immediate = cpu.read_next_pc(memory);
			MicroArchAction::Continue(state.with_operand(immediate))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand2(value))
		},
		4 => {
			let result = cpu.perform_sub_carry(state.operand2, state.operand);
			cpu.write(state.address, result, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn sbc_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("sbc (x), (y)", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.x as u16 + cpu.direct_page_offset();
			let operand = cpu.read(address, memory);
			MicroArchAction::Continue(state.with_operand(operand).with_address(address))
		},
		2 => {
			let operand2 = cpu.read(cpu.y as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand2(operand2))
		},
		3 => {
			let result = cpu.perform_sub_carry(state.operand, state.operand2);
			MicroArchAction::Continue(state.with_operand(result))
		},
		4 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
			let y_address = increment_wrap_within_page(state.address);
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
	debug_instruction!("inc dp+x", cycle, cpu);
	inc_dec_dp_x(cpu, memory, cycle, state, |value| value.wrapping_add(1))
}
fn inc_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("inc a", cycle, cpu);
	inc_dec_register::<{ Register::A }>(cpu, cycle, |value| value.wrapping_add(1))
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
	debug_instruction!("das", cycle, cpu);

	match cycle {
		0 | 1 => MicroArchAction::Continue(InstructionInternalState::default()),
		2 => {
			if !cpu.psw.contains(ProgramStatusWord::Carry) || cpu.a > 0x99 {
				cpu.a = cpu.a.wrapping_sub(0x60);
				cpu.set_carry(false);
			}
			if !cpu.psw.contains(ProgramStatusWord::HalfCarry) || cpu.a & 0xf > 0x9 {
				cpu.a = cpu.a.wrapping_sub(6);
			}
			trace!("decimal adjust => {}", cpu.a);
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_a_inc_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, (x)+", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.x) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			cpu.a = value;
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Continue(state)
		},
		3 => {
			cpu.x = cpu.x.wrapping_add(1);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn di(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("di", cycle, cpu);

	match cycle {
		0 | 1 => MicroArchAction::Continue(state),
		2 => {
			cpu.psw.remove(ProgramStatusWord::Interrupt);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbs 6", cycle, cpu);
	branch_on_bit::<6, true>(cpu, memory, cycle, state)
}
fn mov_dp_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp, a", cycle, cpu);
	move_to_dp::<{ Register::A }>(cpu, memory, cycle, state)
}
fn mov_addr_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov addr, a", cycle, cpu);
	move_to_addr::<{ Register::A }>(cpu, memory, cycle, state)
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
	debug_instruction!("mov (dp+x), a", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let _ = cpu.read(state.address2, memory);
			MicroArchAction::Continue(state)
		},
		6 => {
			cpu.write(state.address2, cpu.a, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn cmp_x_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cmp x, #imm", cycle, cpu);
	cmp_register_imm::<{ Register::X }>(cpu, memory, cycle, state)
}
fn mov_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov addr, x", cycle, cpu);
	move_to_addr::<{ Register::X }>(cpu, memory, cycle, state)
}
fn mov1_addr_c(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov1 addr.bit, c", cycle, cpu);

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
			let address = state.address & 0x1fff;
			let bit_index = (state.address >> 13) as u8;
			let value = cpu.read(address, memory);
			// operand2 = bit index, operand = memory value
			let carry_bit = (cpu.carry() as u8) << bit_index;
			let masked_value = !(1 << bit_index) & value;
			let result = carry_bit | masked_value;
			trace!(
				"write bit {} to {} of address {:04x} ({:02x}) = {:02x}",
				bit_index,
				cpu.carry(),
				address,
				value,
				result
			);
			MicroArchAction::Continue(state.with_operand(result).with_address(address))
		},
		4 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Continue(state)
		},
		5 => MicroArchAction::Next,
		_ => unreachable!(),
	}
}
fn mov_dp_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp, y", cycle, cpu);

	move_to_dp::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn mov_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov addr, y", cycle, cpu);
	move_to_addr::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn mov_x_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, #imm", cycle, cpu);
	move_from_imm::<{ Register::X }>(cpu, memory, cycle, state)
}
fn pop_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("pop x", cycle, cpu);
	pop::<{ Register::X }>(cpu, memory, cycle, state)
}
fn mul(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mul ya", cycle, cpu);
	match cycle {
		0 ..= 7 => MicroArchAction::Continue(state),
		8 => {
			let p1 = cpu.a as u16;
			let p2 = cpu.y as u16;
			let result = p1.wrapping_mul(p2);
			cpu.a = (result & 0xff) as u8;
			cpu.y = (result >> 8) as u8;
			cpu.set_negative_zero(cpu.y);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbc 6", cycle, cpu);
	branch_on_bit::<6, false>(cpu, memory, cycle, state)
}
fn mov_dp_x_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp+x, a", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		4 => {
			cpu.write(state.address, cpu.a, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_addr_x_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov addr+x, a", cycle, cpu);

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
			let offset_address = state.address.wrapping_add(cpu.x as u16);
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		4 => {
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		5 => {
			cpu.write(state.address, cpu.a, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_addr_y_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov addr+y, a", cycle, cpu);

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
			let offset_address = state.address.wrapping_add(cpu.y as u16);
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		4 => {
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		5 => {
			cpu.write(state.address, cpu.a, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
			let pointer_high = cpu.read(increment_wrap_within_page(state.address), memory);
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
	debug_instruction!("mov dp+y, x", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.y as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		4 => {
			cpu.write(state.address, cpu.x, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
			let y_address = increment_wrap_within_page(state.address);
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
	debug_instruction!("mov dp+x, y", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		4 => {
			cpu.write(state.address, cpu.y, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn dec_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dec y", cycle, cpu);
	inc_dec_register::<{ Register::Y }>(cpu, cycle, |value| value.wrapping_sub(1))
}
fn mov_a_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, y", cycle, cpu);
	mov_register_register::<{ Register::A }, { Register::Y }>(cpu, memory, cycle, state)
}
fn cbne_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("cbne dp+x", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset = cpu.read_next_pc(memory) as i8;
			MicroArchAction::Continue(state.with_relative(offset))
		},
		3 => {
			let operand = cpu.read(((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand(operand))
		},
		4 => MicroArchAction::Continue(state),
		5 =>
			if state.operand == cpu.a {
				MicroArchAction::Next
			} else {
				trace!("taking branch to {:+}", state.relative);
				MicroArchAction::Continue(state)
			},
		6 => {
			// Hardware calculates branch target in this step, we do everything at the very end since memory accesses
			// don't happen.
			MicroArchAction::Continue(state)
		},
		7 => {
			cpu.pc = (cpu.pc as isize + state.relative as isize) as u16;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn daa(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("daa", cycle, cpu);

	match cycle {
		0 | 1 => MicroArchAction::Continue(InstructionInternalState::default()),
		2 => {
			if cpu.psw.contains(ProgramStatusWord::Carry) || cpu.a > 0x99 {
				cpu.a = cpu.a.wrapping_add(0x60);
				cpu.set_carry(true);
			}
			if cpu.psw.contains(ProgramStatusWord::HalfCarry) || cpu.a & 0xf > 0x9 {
				cpu.a = cpu.a.wrapping_add(6);
			}
			trace!("decimal adjust => {}", cpu.a);
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

fn clrv(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("clrv", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(state),
		1 => {
			cpu.psw.remove(ProgramStatusWord::Overflow | ProgramStatusWord::HalfCarry);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbs 7", cycle, cpu);
	branch_on_bit::<7, true>(cpu, memory, cycle, state)
}
fn mov_a_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, dp", cycle, cpu);
	move_from_dp::<{ Register::A }>(cpu, memory, cycle, state)
}
fn mov_a_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, addr", cycle, cpu);
	move_from_addr::<{ Register::A }>(cpu, memory, cycle, state)
}
fn mov_a_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("mov a, (x)", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			// TODO: Not sure if this cycle actually does nothing?
			MicroArchAction::Continue(state)
		},
		2 => {
			cpu.a = cpu.read(u16::from(cpu.x) + cpu.direct_page_offset(), memory);
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("mov a, (dp+x)", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			cpu.a = cpu.read(state.address2, memory);
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_a_imm(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, #imm", cycle, cpu);
	move_from_imm::<{ Register::A }>(cpu, memory, cycle, state)
}
fn mov_x_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, addr", cycle, cpu);
	move_from_addr::<{ Register::X }>(cpu, memory, cycle, state)
}
fn not1(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("not1 addr.bit", cycle, cpu);

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
			let address = state.address & 0x1fff;
			let bit_index = (state.address >> 13) as u8;
			let value = cpu.read(address, memory);
			let result = value ^ (1 << bit_index);
			trace!("negate bit {} of address {:04x} ({:02x}) = {:02x}", bit_index, address, value, result);
			MicroArchAction::Continue(state.with_operand(result).with_address(address))
		},
		4 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_y_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov y, dp", cycle, cpu);
	move_from_dp::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn mov_y_addr(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov y, addr", cycle, cpu);
	move_from_addr::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn notc(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("notc", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(state),
		1 => MicroArchAction::Continue(state),
		2 => {
			cpu.psw.toggle(ProgramStatusWord::Carry);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn pop_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("pop y", cycle, cpu);
	pop::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn sleep(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("sleep", cycle, cpu);

	match cycle {
		0 => {
			cpu.run_state = RunState::WaitingForInterrupt;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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
	debug_instruction!("bbc 7", cycle, cpu);
	branch_on_bit::<7, false>(cpu, memory, cycle, state)
}
fn mov_a_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, dp+x", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			cpu.a = value;
			cpu.set_negative_zero(cpu.a);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_a_addr_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, addr+x", cycle, cpu);
	move_from_addr_offset::<{ Register::X }>(cpu, memory, cycle, state)
}
fn mov_a_addr_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov a, addr+y", cycle, cpu);
	move_from_addr_offset::<{ Register::Y }>(cpu, memory, cycle, state)
}
fn mov_a_dp_indirect_y(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	debug_instruction!("mov a, (dp)+y", cycle, cpu);

	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = state.address + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2.wrapping_add(cpu.y as u16), memory);
			cpu.a = value;
			cpu.set_negative_zero(value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_x_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, dp", cycle, cpu);

	move_from_dp::<{ Register::X }>(cpu, memory, cycle, state)
}
fn mov_x_dp_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov x, dp+y", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.y as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			cpu.x = value;
			cpu.set_negative_zero(cpu.x);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_dp_dp(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov dp, dp", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address2 = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address2(address2))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value2 = cpu.read(state.address2, memory);
			MicroArchAction::Continue(state.with_operand2(value2))
		},
		4 => {
			cpu.write(state.address, state.operand2, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn mov_y_dp_x(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov y, dp+x", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			cpu.y = value;
			cpu.set_negative_zero(cpu.y);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
fn inc_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("inc y", cycle, cpu);
	inc_dec_register::<{ Register::Y }>(cpu, cycle, |value| value.wrapping_add(1))
}
fn mov_y_a(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("mov y, a", cycle, cpu);
	mov_register_register::<{ Register::Y }, { Register::A }>(cpu, memory, cycle, state)
}
fn dbnz_y(cpu: &mut Smp, memory: &mut Memory, cycle: usize, state: InstructionInternalState) -> MicroArchAction {
	debug_instruction!("dbnz y", cycle, cpu);
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let decremented = cpu.y.wrapping_sub(1);
			MicroArchAction::Continue(state.with_operand(decremented))
		},
		2 => {
			cpu.y = state.operand;
			MicroArchAction::Continue(state)
		},
		3 => {
			let offset = cpu.read_next_pc(memory) as i8;
			if state.operand == 0 {
				MicroArchAction::Next
			} else {
				trace!("decremented y to non-zero value {}, taking branch to {:+x}", state.operand, offset);
				MicroArchAction::Continue(state.with_relative(offset))
			}
		},
		4 => {
			// Hardware calculates branch target in this step, we do everything at the very end since memory accesses
			// don't happen.
			MicroArchAction::Continue(state)
		},
		5 => {
			cpu.pc = (cpu.pc as isize + state.relative as isize) as u16;
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
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

/// Wrap an address increment within a 256-byte page.
#[inline]
const fn increment_wrap_within_page(address: u16) -> u16 {
	add_wrap_within_page(address, 1)
}

/// Wrap an address addition within a 256-byte page.
#[inline]
const fn add_wrap_within_page(address: u16, addition: u8) -> u16 {
	let low_byte_increment = (address & 0xff).wrapping_add(addition as u16) & 0xff;
	address & 0xff00 | low_byte_increment
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
fn move_to_addr<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
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
			let _ = cpu.read(state.address, memory);
			MicroArchAction::Continue(state)
		},
		4 => {
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
fn move_from_addr<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
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
			cpu.register_write::<REGISTER>(value);
			cpu.set_negative_zero(value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
#[allow(clippy::needless_pass_by_ref_mut)]
fn move_from_addr_offset<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
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
		3 => MicroArchAction::Continue(state),
		4 => {
			let offset_address = state.address.wrapping_add(cpu.register_read::<REGISTER>() as u16);
			let value = cpu.read(offset_address, memory);
			cpu.a = value;
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
fn cmp_register_dp<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &Memory,
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
			cpu.set_subtract_carry(register, operand);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", register, operand, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn cmp_register_imm<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let operand = cpu.read_next_pc(memory);
			let register = cpu.register_read::<REGISTER>();
			let result = (register as i8).wrapping_sub(operand as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(register, operand);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", register, operand, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn cmp_register_addr<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
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
			let operand = cpu.read(state.address, memory);
			let register = cpu.register_read::<REGISTER>();
			let result = (register as i8).wrapping_sub(operand as i8);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(register, operand);
			trace!("cmp {:02x} - {:02x} = {:+} (flags {})", register, operand, result, cpu.psw);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn inc_dec_register<const REGISTER: Register>(cpu: &mut Smp, cycle: usize, op: impl Fn(u8) -> u8) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			cpu.register_write::<REGISTER>(op(cpu.register_read::<REGISTER>()));
			cpu.set_negative_zero(cpu.register_read::<REGISTER>());
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn inc_dec_dp_x(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(value))
		},
		4 => {
			let value = op(state.operand);
			cpu.set_negative_zero(value);
			cpu.write(state.address, value, memory);
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
			let upper_target_address = u16::from(cpu.read(increment_wrap_within_page(call_address), memory));
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
		4 => {
			let value = (state.operand >> BIT_INDEX) & 1 == bit_value;
			trace!(
				"bit {BIT_INDEX} {} in {} ({:08b}), {}taking branch",
				if (state.operand >> BIT_INDEX) > 0 { "set" } else { "clear" },
				state.operand,
				state.operand,
				if value { "" } else { "not " }
			);
			if value {
				MicroArchAction::Continue(state)
			} else {
				MicroArchAction::Next
			}
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
fn inc_dec_dp(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = u16::from(cpu.read_next_pc(memory)) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = op(cpu.read(state.address, memory));
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

#[inline]
fn inc_dec_addr(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8) -> u8,
) -> MicroArchAction {
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
			let value = op(cpu.read(state.address, memory));
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

#[inline]
fn logic_op_a_x_indirect(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		// TODO: not sure what the CPU does in this cycle.
		1 => {
			let address = cpu.x as u16 + cpu.direct_page_offset();
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
fn logic_op_a_dp_x_indirect(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2, memory);
			let result = op(cpu.a, value);
			cpu.a = result;
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_a_dp_y_indirect(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let pointer_address = state.address + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(pointer_address))
		},
		3 => {
			let address_low = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_address2(address_low as u16))
		},
		4 => {
			let address_high = cpu.read(increment_wrap_within_page(state.address), memory) as u16;
			let address = address_high << 8 | state.address2;
			MicroArchAction::Continue(state.with_address2(address))
		},
		5 => {
			let value = cpu.read(state.address2.wrapping_add(cpu.y as u16), memory);
			let result = op(cpu.a, value);
			cpu.a = result;
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_a_dp_x(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16;
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let offset_address = ((state.address + cpu.x as u16) & 0xff) + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(offset_address))
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

#[inline]
fn logic_op_a_addr_register<const REGISTER: Register>(
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
			let offset_address = state.address.wrapping_add(cpu.register_read::<REGISTER>() as u16);
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		4 => {
			let value = cpu.read(state.address, memory);
			let result = op(cpu.a, value);
			cpu.a = result;
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

/// TODO: Make this generic to reuse for subtraction
#[inline]
fn cmp_a_addr_register<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
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
			let offset_address = state.address.wrapping_add(cpu.register_read::<REGISTER>() as u16);
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		4 => {
			let operand = cpu.read(state.address, memory);
			let result = (cpu.a as i8).wrapping_sub(operand as i8);
			trace!("cmp {:02x} - {:02x} = {:+}", cpu.a, operand, result);
			cpu.set_negative_zero(result as u8);
			cpu.set_subtract_carry(cpu.a, operand);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn sbc_a_addr_register<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
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
			let offset_address = state.address.wrapping_add(cpu.register_read::<REGISTER>() as u16);
			MicroArchAction::Continue(state.with_address(offset_address))
		},
		4 => {
			let operand = cpu.read(state.address, memory);
			cpu.a = cpu.perform_sub_carry(cpu.a, operand);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_a_imm(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let value = cpu.read_next_pc(memory);
			let result = op(cpu.a, value);
			cpu.a = result;
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_dp_dp(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address2 = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address2(address2))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value2 = cpu.read(state.address2, memory);
			MicroArchAction::Continue(state.with_operand2(value2))
		},
		4 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(value))
		},
		5 => {
			let result = op(state.operand, state.operand2);
			cpu.write(state.address, result, memory);
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_x_y_indirect(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let operand = cpu.read(cpu.x as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand(operand))
		},
		2 => {
			let operand2 = cpu.read(cpu.y as u16 + cpu.direct_page_offset(), memory);
			MicroArchAction::Continue(state.with_operand2(operand2))
		},
		3 => {
			let result = op(state.operand, state.operand2);
			cpu.set_negative_zero(result);
			MicroArchAction::Continue(state.with_operand(result))
		},
		4 => {
			cpu.write(cpu.x as u16 + cpu.direct_page_offset(), state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn logic_op_dp_imm(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, u8) -> u8,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let immediate = cpu.read_next_pc(memory);
			MicroArchAction::Continue(state.with_operand(immediate))
		},
		2 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		3 => {
			let value = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand2(value))
		},
		4 => {
			let result = op(state.operand, state.operand2);
			cpu.write(state.address, result, memory);
			cpu.set_negative_zero(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn bit_logic_addr<const TAKES_FOUR_CYCLES: bool>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(bool, bool) -> bool,
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
			let address = state.address & 0x1fff;
			let bit_index = (state.address >> 13) as u8;
			let value = cpu.read(address, memory);
			// operand2 = bit index, operand = memory value
			let memory_bit = (value >> bit_index) & 1 > 0;
			trace!("read bit {} of address {:04x} ({:02x}) = {}", bit_index, address, value, memory_bit);
			let result = op(cpu.carry(), memory_bit);
			cpu.set_carry(result);
			if TAKES_FOUR_CYCLES {
				MicroArchAction::Next
			} else {
				MicroArchAction::Continue(state)
			}
		},
		4 => MicroArchAction::Next,
		_ => unreachable!(),
	}
}

/// op is a function receiving and returning the 8-bit value as well as the carry value.
#[inline]
fn shift_register<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, bool) -> (u8, bool),
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let (result, carry) = op(cpu.register_read::<REGISTER>(), cpu.carry());
			cpu.set_negative_zero(result);
			cpu.set_carry(carry);
			cpu.register_write::<REGISTER>(result);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

/// op is a function receiving and returning the 8-bit value as well as the carry value.
#[inline]
fn shift_dp(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, bool) -> (u8, bool),
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let value = cpu.read(state.address, memory);
			let (result, carry) = op(value, cpu.carry());
			cpu.set_negative_zero(result);
			cpu.set_carry(carry);
			MicroArchAction::Continue(state.with_operand(result))
		},
		3 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

/// op is a function receiving and returning the 8-bit value as well as the carry value.
#[inline]
fn shift_dp_x(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, bool) -> (u8, bool),
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory).wrapping_add(cpu.x) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => MicroArchAction::Continue(state),
		3 => {
			let value = cpu.read(state.address, memory);
			let (result, carry) = op(value, cpu.carry());
			cpu.set_negative_zero(result);
			cpu.set_carry(carry);
			MicroArchAction::Continue(state.with_operand(result))
		},
		4 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn shift_addr(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u8, bool) -> (u8, bool),
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
			let (result, carry) = op(value, cpu.carry());
			cpu.set_negative_zero(result);
			cpu.set_carry(carry);
			MicroArchAction::Continue(state.with_operand(result))
		},
		4 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn push<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => MicroArchAction::Continue(state),
		2 => {
			cpu.push(cpu.register_read::<REGISTER>(), memory);
			MicroArchAction::Continue(state)
		},
		3 => MicroArchAction::Next,
		_ => unreachable!(),
	}
}

#[inline]
fn pop<const REGISTER: Register>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => MicroArchAction::Continue(state.with_operand(cpu.pop(memory))),
		2 => MicroArchAction::Continue(state),
		3 => {
			cpu.register_write::<REGISTER>(state.operand);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn test1(
	cpu: &mut Smp,
	memory: &mut Memory,
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
			cpu.set_negative_zero(cpu.a.wrapping_sub(value));
			MicroArchAction::Continue(state.with_operand(value))
		},
		4 => {
			let result = op(state.operand, cpu.a);
			MicroArchAction::Continue(state.with_operand(result))
		},
		5 => {
			cpu.write(state.address, state.operand, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn inc_dec_word(
	cpu: &mut Smp,
	memory: &mut Memory,
	cycle: usize,
	state: InstructionInternalState,
	op: impl Fn(u16) -> u16,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let address = cpu.read_next_pc(memory) as u16 + cpu.direct_page_offset();
			MicroArchAction::Continue(state.with_address(address))
		},
		2 => {
			let low_operand = cpu.read(state.address, memory);
			MicroArchAction::Continue(state.with_operand(low_operand))
		},
		3 => {
			let high_operand = cpu.read(increment_wrap_within_page(state.address), memory);
			MicroArchAction::Continue(state.with_operand2(high_operand))
		},
		4 => {
			let value = state.operand as u16 | (state.operand2 as u16) << 8;
			let result = op(value);
			cpu.set_negative_zero_word(result);
			cpu.write(state.address, (result & 0xff) as u8, memory);
			MicroArchAction::Continue(state.with_address2(result))
		},
		5 => {
			cpu.write(increment_wrap_within_page(state.address), (state.address2 >> 8) as u8, memory);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}

#[inline]
fn mov_register_register<const TARGET: Register, const SOURCE: Register>(
	cpu: &mut Smp,
	memory: &Memory,
	cycle: usize,
	state: InstructionInternalState,
) -> MicroArchAction {
	match cycle {
		0 => MicroArchAction::Continue(InstructionInternalState::default()),
		1 => {
			let value = cpu.register_read::<SOURCE>();
			cpu.register_write::<TARGET>(value);
			cpu.set_negative_zero(value);
			MicroArchAction::Next
		},
		_ => unreachable!(),
	}
}
