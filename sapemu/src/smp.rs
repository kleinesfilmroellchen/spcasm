//! S-SMP (SPC700 CPU) emulator.

#![allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap, clippy::cast_sign_loss)]

mod ops;
pub mod peripherals;
pub mod upload;

use log::{debug, error};
use spcasm::sema::Register;

use self::ops::{InstructionImpl, InstructionInternalState, OPCODE_TABLE};
use self::peripherals::{ControlRegister, CpuIOPorts, ProgramStatusWord, RunState, TestRegister, Timers};
use crate::dsp::registers::DspRegisters;
use crate::memory::Memory;
use crate::trace;

/// State of the microprocessor.
#[derive(Clone, Debug)]
pub struct Smp {
	/// TEST register.
	pub test:                 TestRegister,
	/// CONTROL register.
	pub control:              ControlRegister,
	/// Accumulator.
	pub a:                    u8,
	/// X index register.
	pub x:                    u8,
	/// Y index register.
	pub y:                    u8,
	/// Program Counter.
	pub pc:                   u16,
	/// Stack Pointer.
	pub sp:                   u8,
	/// Program Status Word (flags register).
	pub psw:                  ProgramStatusWord,
	/// Main CPU I/O ports.
	pub ports:                CpuIOPorts,
	/// CPU-internal timers.
	pub timers:               Timers,
	/// Cycle counter for debugging purposes.
	pub(crate) cycle_counter: u128,

	/// Cycle within an instruction.
	pub(crate) instruction_cycle:      usize,
	/// Opcode of the instruction being executed.
	pub(crate) current_opcode:         u8,
	/// Last instruction state returned by the instruction.
	pub(crate) last_instruction_state: InstructionInternalState,

	/// CPU execution state.
	pub(crate) run_state: RunState,

	/// Cached function that executes the current instruction.
	pub(crate) instruction_function: InstructionImpl,
}

impl Default for Smp {
	fn default() -> Self {
		Self {
			test:                   TestRegister::default(),
			control:                ControlRegister::default(),
			a:                      Default::default(),
			x:                      Default::default(),
			y:                      Default::default(),
			pc:                     Default::default(),
			sp:                     Default::default(),
			psw:                    ProgramStatusWord::default(),
			ports:                  CpuIOPorts::default(),
			timers:                 Timers::default(),
			cycle_counter:          Default::default(),
			instruction_cycle:      Default::default(),
			current_opcode:         Default::default(),
			last_instruction_state: InstructionInternalState::default(),
			run_state:              RunState::default(),
			instruction_function:   ops::nop,
		}
	}
}

/// CPU clock rate (Hz)
pub const CPU_RATE: usize = 2_048_000;

/// TEST register.
pub const TEST: u16 = 0x00F0;
/// CONTROL register.
pub const CONTROL: u16 = 0x00F1;
/// CPUIO0 register.
pub const CPUIO0: u16 = 0x00F4;
/// CPUIO1 register.
pub const CPUIO1: u16 = 0x00F5;
/// CPUIO2 register.
pub const CPUIO2: u16 = 0x00F6;
/// CPUIO3 register.
pub const CPUIO3: u16 = 0x00F7;
/// DSP register read/write port.
pub const DSPADDR: u16 = 0x00F2;
/// DSP register address port.
pub const DSPDATA: u16 = 0x00F3;
/// Timer 0 divider.
pub const T0DIV: u16 = 0x00FA;
/// Timer 1 divider.
pub const T1DIV: u16 = 0x00FB;
/// Timer 2 divider.
pub const T2DIV: u16 = 0x00FC;
/// Timer 0 output.
pub const T0OUT: u16 = 0x00FD;
/// Timer 1 output.
pub const T1OUT: u16 = 0x00FE;
/// Timer 2 output.
pub const T2OUT: u16 = 0x00FF;

/// Vector for software interrupts.
pub const BREAK_VECTOR: u16 = 0xFFDE;
/// Vector for resets.
pub const RESET_VECTOR: u16 = 0xFFFE;

impl Smp {
	/// Create a new reset CPU.
	pub fn new(memory: &mut Memory) -> Self {
		let control = ControlRegister::default();
		Self {
			control,
			// Reset values of A, X, Y, SP seem to be unknown.
			a: 0,
			x: 0,
			y: 0,
			sp: 0,
			pc: memory.read_word(RESET_VECTOR, control.contains(ControlRegister::BootRomEnable)),
			timers: Timers::new(),
			..Default::default()
		}
	}

	/// Run a single CPU cycle.
	pub fn tick(&mut self, memory: &mut Memory, dsp: &mut DspRegisters) {
		self.cycle_counter += 1;
		if self.run_state != RunState::Running {
			return;
		}
		trace!("SMP tick {}", self.cycle_counter);

		self.timers.tick(self.control);

		// Fetch next instruction
		if self.instruction_cycle == 0 {
			self.current_opcode = self.read_next_pc(memory, dsp);
			self.instruction_function = OPCODE_TABLE[self.current_opcode as usize];
			trace!(
				"(@{}) fetch instruction [{:04x}] = {:02x}",
				self.cycle_counter,
				self.pc.wrapping_sub(1),
				self.current_opcode
			);
		}

		// Execute tick
		let instruction_result =
			(self.instruction_function)(self, memory, dsp, self.instruction_cycle, self.last_instruction_state);
		// Decide whether to advance to next instruction or not
		match instruction_result {
			ops::MicroArchAction::Continue(new_state) => {
				self.last_instruction_state = new_state;
				self.instruction_cycle += 1;
			},
			ops::MicroArchAction::Next => {
				self.instruction_cycle = 0;
				trace!("----- next instruction");
			},
		}
	}

	#[inline]
	fn direct_page_offset(&self) -> u16 {
		u16::from((self.psw & ProgramStatusWord::DirectPage).0) << 3
	}

	#[inline]
	fn set_zero(&mut self, value: u8) {
		self.psw.set(ProgramStatusWord::Zero, value == 0);
	}

	#[inline]
	fn set_negative(&mut self, value: u8) {
		self.psw.set(ProgramStatusWord::Sign, (value as i8) < 0);
	}

	/// Set the negative and zero flags depending on the input value.
	#[inline]
	fn set_negative_zero(&mut self, value: u8) {
		self.set_zero(value);
		self.set_negative(value);
	}

	/// Set the negative and zero flags depending on the input value.
	#[inline]
	fn set_negative_zero_word(&mut self, value: u16) {
		self.psw.set(ProgramStatusWord::Zero, value == 0);
		self.psw.set(ProgramStatusWord::Sign, (value as i16) < 0);
	}

	/// Set the carry flag based on some other calculation; the carry flag effectively operates as a single-bit register
	/// in many instructions.
	#[inline]
	fn set_carry(&mut self, carry: bool) {
		self.psw.set(ProgramStatusWord::Carry, carry);
	}

	/// Returns the state of the carry flag.
	#[inline]
	const fn carry(&self) -> bool {
		self.psw.contains(ProgramStatusWord::Carry)
	}

	/// Set the carry flag if the subtraction produces a carry bit.
	#[inline]
	fn set_subtract_carry(&mut self, op1: u8, op2: u8) {
		// FIXME: is there a better way to find the carry bit other than to emulate the hardware full-adder?
		let expanded_result = u16::from(op1).wrapping_add(u16::from(!op2)) + 1;
		self.psw.set(ProgramStatusWord::Carry, expanded_result & 0x100 > 0);
	}

	/// Sets all relevant flags of an 8-bit addition with carry in.
	#[inline]
	fn set_add_carry_flags(&mut self, op1: u8, op2: u8) {
		let (result, has_carry) = op1.carrying_add(op2, self.carry());
		let half_carry_result = (op1 & 0x0f) + (op2 & 0x0f) + u8::from(self.carry()) >= 0x10;

		self.psw.set(ProgramStatusWord::Overflow, (op1 as i8).carrying_add(op2 as i8, self.carry()).1);
		self.psw.set(ProgramStatusWord::Sign, (result as i8) < 0);
		self.psw.set(ProgramStatusWord::Zero, result == 0);
		self.psw.set(ProgramStatusWord::Carry, has_carry);
		self.psw.set(ProgramStatusWord::HalfCarry, half_carry_result);
	}

	/// Sets all relevant flags of an 8-bit subtraction with carry (or borrow) in, and returns the result.
	#[inline]
	fn perform_sub_carry(&mut self, op1: u8, op2: u8) -> u8 {
		let expanded_result = u16::from(op1).wrapping_add(u16::from(!op2)) + u16::from(self.carry());
		trace!("{} + {} + {} = {}", op1, !op2, self.carry(), expanded_result);
		let result = (expanded_result & 0xff) as u8;
		self.psw.set(ProgramStatusWord::Sign, (result as i8) < 0);
		self.psw.set(ProgramStatusWord::Zero, result == 0);
		self.psw.set(ProgramStatusWord::Overflow, (op1 as i8).borrowing_sub(op2 as i8, !self.carry()).1);
		let half_carry_result = (op1 & 0x0f) + ((!op2) & 0x0f) + u8::from(self.carry()) >= 0x10;
		self.psw.set(ProgramStatusWord::HalfCarry, half_carry_result);

		self.psw.set(ProgramStatusWord::Carry, expanded_result >= 0x100);
		result
	}

	/// Set the interrupt flag.
	#[inline]
	fn set_interrupt(&mut self, interrupt: bool) {
		self.psw.set(ProgramStatusWord::Interrupt, interrupt);
	}

	/// Set the break flag.
	#[inline]
	fn set_break(&mut self, break_: bool) {
		self.psw.set(ProgramStatusWord::Break, break_);
	}

	#[track_caller]
	fn write(&mut self, address: u16, value: u8, memory: &mut Memory, dsp: &mut DspRegisters) {
		match address {
			TEST => self.test_write(value),
			CONTROL => self.control_write(value),
			CPUIO0 | CPUIO1 | CPUIO2 | CPUIO3 => self.ports.write(address - CPUIO0, value),
			T0DIV => self.timers.timer_divisor[0] = value,
			T1DIV => self.timers.timer_divisor[1] = value,
			T2DIV => self.timers.timer_divisor[2] = value,
			// Writes to the timer output registers pass through to memory.
			DSPDATA => {
				let dsp_address = memory.read(DSPADDR, self.control.contains(ControlRegister::BootRomEnable));
				dsp.write(dsp_address, value);
			},
			_ => self.memory_write(address, value, memory),
		}
	}

	#[track_caller]
	fn read(&mut self, address: u16, memory: &Memory, dsp: &DspRegisters) -> u8 {
		match address {
			TEST => self.test.0,
			CONTROL => self.control.0,
			CPUIO0 | CPUIO1 | CPUIO2 | CPUIO3 => self.ports.read(address - CPUIO0),
			DSPDATA => {
				let dsp_address = memory.read(DSPADDR, self.control.contains(ControlRegister::BootRomEnable));
				dsp.read(dsp_address)
			},
			T0DIV => self.timers.timer_divisor[0],
			T1DIV => self.timers.timer_divisor[1],
			T2DIV => self.timers.timer_divisor[2],
			T0OUT => self.timers.timer_out[0],
			T1OUT => self.timers.timer_out[1],
			T2OUT => self.timers.timer_out[2],
			_ => memory.read(address, self.control.contains(ControlRegister::BootRomEnable)),
		}
	}

	/// Reads memory at the current program counter and advances it afterwards.
	fn read_next_pc(&mut self, memory: &Memory, dsp: &DspRegisters) -> u8 {
		let data = self.read(self.pc, memory, dsp);
		self.pc = self.pc.wrapping_add(1);
		data
	}

	/// Pushes a value onto the stack. Note that this actually takes two cycles in hardware, which users must account
	/// for.
	fn push(&mut self, value: u8, memory: &mut Memory) {
		self.memory_write(self.stack_top(), value, memory);
		self.sp = self.sp.wrapping_sub(1);
	}

	/// Pops a value from the stack. Note that this actually takes two cycles in hardware, which users must account
	/// for.
	fn pop(&mut self, memory: &Memory) -> u8 {
		self.sp = self.sp.wrapping_add(1);
		memory.read(self.stack_top(), self.control.contains(ControlRegister::BootRomEnable))
	}

	/// Returns the address of the hardware stack top, i.e. the lowest stack address that is free.
	#[inline]
	#[must_use]
	pub const fn stack_top(&self) -> u16 {
		self.sp as u16 + 0x100
	}

	#[allow(clippy::needless_pass_by_ref_mut)]
	#[track_caller]
	fn memory_write(&mut self, address: u16, value: u8, memory: &mut Memory) {
		if self.test.contains(TestRegister::RamWriteDisable) {
			debug!("RAM write to {} is disabled via TEST register", address);
			return;
		}
		memory.write(address, value);
	}

	#[track_caller]
	fn test_write(&mut self, value: u8) {
		trace!("TEST = {:08b}", value);
		self.test = TestRegister(value);

		if self.test.contains(TestRegister::Crash) {
			error!("CPU was crashed via TEST register");
			self.run_state = RunState::Crashed;
		}
	}

	#[track_caller]
	fn control_write(&mut self, value: u8) {
		trace!("CONTROL = {:08b}", value);
		self.control = ControlRegister(value);

		if self.control.contains(ControlRegister::ResetPorts01) {
			self.ports.reset_port(0);
			self.ports.reset_port(1);
		}

		if self.control.contains(ControlRegister::ResetPorts23) {
			self.ports.reset_port(2);
			self.ports.reset_port(3);
		}
	}

	/// Writes to a register determined at compile-time.
	#[track_caller]
	fn register_write<const REGISTER: Register>(&mut self, value: u8) {
		match REGISTER {
			Register::A => self.a = value,
			Register::X => self.x = value,
			Register::Y => self.y = value,
			Register::SP => self.sp = value,
			Register::PSW | Register::P => self.psw = ProgramStatusWord(value),
			Register::YA => unreachable!("16-bit YA not allowed for 8-bit register write"),
			Register::C => self.psw.set(ProgramStatusWord::Carry, value != 0),
		}
	}

	/// Reads from a register determined at compile-time.
	#[track_caller]
	#[inline]
	fn register_read<const REGISTER: Register>(&self) -> u8 {
		match REGISTER {
			Register::A => self.a,
			Register::X => self.x,
			Register::Y => self.y,
			Register::SP => self.sp,
			Register::PSW | Register::P => self.psw.0,
			Register::YA => unreachable!("16-bit YA not allowed for 8-bit register read"),
			Register::C => u8::from(self.psw.contains(ProgramStatusWord::Carry)),
		}
	}

	fn ya(&self) -> u16 {
		(u16::from(self.y) << 8) | u16::from(self.a)
	}

	/// Returns whether the CPU is halted or not. A halted CPU must be reset to continue execution.
	#[must_use]
	pub fn is_halted(&self) -> bool {
		!self.run_state.is_running() || self.test.contains(TestRegister::Crash)
	}

	/// Copies the state of the various memory mapped registers from the hidden memory behind it.
	pub(crate) fn copy_mapped_registers_from_memory(&mut self, memory: &Memory) {
		self.test = TestRegister(memory.read(TEST, true));
		self.control = ControlRegister(memory.read(CONTROL, true));

		self.ports.write(0, memory.read(CPUIO0, true));
		self.ports.write_to_smp::<0>(memory.read(CPUIO0, true));
		self.ports.write(1, memory.read(CPUIO1, true));
		self.ports.write_to_smp::<1>(memory.read(CPUIO1, true));
		self.ports.write(2, memory.read(CPUIO2, true));
		self.ports.write_to_smp::<2>(memory.read(CPUIO2, true));
		self.ports.write(3, memory.read(CPUIO3, true));
		self.ports.write_to_smp::<3>(memory.read(CPUIO3, true));
	}
}
