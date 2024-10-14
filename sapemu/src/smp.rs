//! S-SMP (SPC700 CPU) emulator.

#![allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap, clippy::cast_sign_loss)]

mod ops;
pub mod upload;

use std::marker::ConstParamTy;

use bitflags::bitflags;
use log::{debug, error};
use spcasm::sema::Register;

use self::ops::InstructionInternalState;
use crate::memory::Memory;
use crate::smp::ops::OPCODE_TABLE;
use crate::trace;

/// State of the microprocessor.
#[derive(Clone, Debug, Default)]
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
}

/// CPU execution state.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(crate) enum RunState {
	#[default]
	Running,
	Crashed,
	#[allow(unused)]
	Halted,
	#[allow(unused)]
	WaitingForInterrupt,
}

impl RunState {
	/// Whether this is a running state (where the CPU may continue executing at some point without a reset.)
	#[inline]
	#[must_use]
	pub fn is_running(self) -> bool {
		![Self::Halted, Self::Crashed].contains(&self)
	}
}

/// Main CPU I/O ports.
#[derive(Clone, Default, Debug)]
pub struct CpuIOPorts {
	/// S-SMP write ports (to main CPU)
	pub write_ports: [u8; 4],
	/// S-SMP read ports (from main CPU)
	pub read_ports:  [u8; 4],
}

impl CpuIOPorts {
	#[inline]
	fn check_port_number(port_number: u16) {
		assert!(port_number <= 3, "Illegal port number {port_number}");
	}

	/// Perform a write to the main CPU.
	#[inline]
	#[track_caller]
	pub fn write(&mut self, port_number: u16, value: u8) {
		Self::check_port_number(port_number);

		trace!("Write CPUIO {0} = {1:02x} ({1})", port_number, value);
		self.write_ports[port_number as usize] = value;
	}

	/// Perform a read from the main CPU.
	#[inline]
	#[track_caller]
	#[allow(clippy::needless_pass_by_ref_mut)]
	pub fn read(&mut self, port_number: u16) -> u8 {
		Self::check_port_number(port_number);

		trace!("Read CPUIO {0} = {1:02x} ({1})", port_number, self.read_ports[port_number as usize]);
		self.read_ports[port_number as usize]
	}

	/// Reset the main CPU input port to 0.
	#[inline]
	#[track_caller]
	pub fn reset_port(&mut self, port_number: u16) {
		Self::check_port_number(port_number);

		trace!("Reset CPUIO {0}", port_number);
		self.read_ports[port_number as usize] = 0;
	}

	/// Perform a read from the SMP.
	///
	/// # Panics
	/// Panics if the port number is invalid.
	#[inline]
	#[track_caller]
	#[must_use]
	pub fn read_from_smp<const PORT_NUMBER: u8>(&self) -> u8
	where
		// FIXME: currently accepted hack to create arbitrary expression bounds.
		//        This expression is designed so it will overflow for values >= 4.
		[(); (PORT_NUMBER + (0xff - 3)) as usize]:, // Only port numbers between 0 and 3 inclusive are allowed
	{
		trace!("[SNES-CPU] read CPUIO {PORT_NUMBER} = {0:02x} ({0})", self.write_ports[PORT_NUMBER as usize]);
		self.write_ports[PORT_NUMBER as usize]
	}

	/// Perform a write to the SMP.
	#[inline]
	#[track_caller]
	pub fn write_to_smp<const PORT_NUMBER: u8>(&mut self, value: u8)
	where
		// FIXME: currently accepted hack to create arbitrary expression bounds.
		//        This expression is designed so it will overflow for values >= 4.
		[(); (PORT_NUMBER + (0xff - 3)) as usize]:, // Only port numbers between 0 and 3 inclusive are allowed
	{
		trace!("[SNES-CPU] write CPUIO {PORT_NUMBER} = {0:02x} ({0})", value);
		self.read_ports[PORT_NUMBER as usize] = value;
	}
}

/// CPU clock rate (Hz)
pub const CPU_RATE: usize = 2_048_000;

/// Internal CPU timers.
#[derive(Clone, Copy, Debug)]
pub struct Timers {
	/// Current timer output value (`TnOUT`).
	pub timer_out:        [u8; 3],
	/// Timer divisor values (`TnDIV`).
	pub timer_divisor:    [u8; 3],
	/// How many CPU clock cycles until timer is incremented the next time.
	timer_tick_remaining: [usize; 3],
}

impl Default for Timers {
	fn default() -> Self {
		Self::new()
	}
}

impl Timers {
	const T01_CLOCKS_PER_STEP: usize = CPU_RATE / Self::T01_RATE;
	const T01_RATE: usize = 8000;
	const T2_CLOCKS_PER_STEP: usize = CPU_RATE / Self::T2_RATE;
	const T2_RATE: usize = 64000;
	const TIMER_CLOCKS_PER_STEP: [usize; 3] =
		[Self::T01_CLOCKS_PER_STEP, Self::T01_CLOCKS_PER_STEP, Self::T2_CLOCKS_PER_STEP];
	#[allow(unused)]
	const TIMER_RATES: [usize; 3] = [Self::T01_RATE, Self::T01_RATE, Self::T2_RATE];

	/// Create new timers.
	#[must_use]
	pub fn new() -> Self {
		let mut new = Self { timer_out: [0; 3], timer_divisor: [1; 3], timer_tick_remaining: [0; 3] };
		new.reset_timers_if_necessary();
		new
	}

	#[inline]
	fn reset_timers_if_necessary(&mut self) {
		for finalized_timer in
			self.timer_tick_remaining
				.into_iter()
				.enumerate()
				.filter_map(|(i, timer)| if timer == 0 { Some(i) } else { None })
		{
			// FIXME: Not sure if this is the only place where we should check the divisor.
			let mut divisor = self.timer_divisor[finalized_timer] as usize;
			if divisor == 0 {
				divisor = 256;
			}
			self.timer_tick_remaining[finalized_timer] = Self::TIMER_CLOCKS_PER_STEP[finalized_timer] * divisor;
			self.timer_out[finalized_timer] = (self.timer_out[finalized_timer] + 1) % 0xf;
			#[cfg(debug_assertions)]
			trace!(
				"Timer {} step to {} ({}Hz / {})",
				finalized_timer,
				self.timer_out[finalized_timer],
				Self::TIMER_RATES[finalized_timer],
				divisor
			);
		}
	}

	/// Emulate a CPU tick for the timers.
	#[inline]
	pub fn tick(&mut self, control: ControlRegister) {
		self.reset_timers_if_necessary();

		for (_, timer) in &mut self
			.timer_tick_remaining
			.iter_mut()
			.enumerate()
			.filter(|(i, _)| control.contains(ControlRegister(1 << i)))
		{
			*timer -= 1;
		}
	}
}

/// Internal TEST register.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct TestRegister(u8);

impl Default for TestRegister {
	fn default() -> Self {
		Self(0xA0)
	}
}

/// Internal CONTROL register.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct ControlRegister(u8);

impl Default for ControlRegister {
	fn default() -> Self {
		Self(0xB0)
	}
}

/// Program Status Word (flags register).
#[derive(Clone, Copy, Debug, PartialEq, Eq, ConstParamTy)]
#[repr(transparent)]
#[derive(Default)]
pub struct ProgramStatusWord(pub(crate) u8);

bitflags! {
	impl TestRegister: u8 {
		/// Disable timers with flag = 1
		const TimerEnable = 0b0000_0001;
		/// Disable RAM writes with flag = 1
		const RamWriteDisable = 0b0000_0010;
		/// Crash the CPU with flag = 1
		const Crash = 0b0000_0100;
		/// Disable timers with flag = 0
		const TimerDisable = 0b0000_1000;
		/// Add RAM wait states
		const RamWaitstates = 0b0011_0000;
		/// Add ROM/IO wait states
		const IoWaitstates = 0b1100_0000;
	}

	impl ControlRegister: u8 {
		/// Enable Timer 0
		const Timer0Enable = 0b0000_0001;
		/// Enable Timer 1
		const Timer1Enable = 0b0000_0010;
		/// Enable Timer 2
		const Timer2Enable = 0b0000_0100;
		/// All timer enable flags
		const AllTimersEnable = Self::Timer0Enable.bits() | Self::Timer1Enable.bits() | Self::Timer2Enable.bits();
		/// Reset CPUIO 0 & 1 latches
		const ResetPorts01 = 0b0001_0000;
		/// Reset CPUIO 2 & 3 latches
		const ResetPorts23 = 0b0010_0000;
		/// Enable Boot ROM with flag = 1
		const BootRomEnable = 0b1000_0000;
	}

	impl ProgramStatusWord: u8 {
		/// N
		const Sign = 0b1000_0000;
		/// V
		const Overflow = 0b0100_0000;
		/// P
		const DirectPage = 0b0010_0000;
		/// B
		const Break = 0b0001_0000;
		/// H
		const HalfCarry = 0b0000_1000;
		/// I
		const Interrupt = 0b0000_0100;
		/// Z
		const Zero = 0b0000_0010;
		/// C
		const Carry = 0b0000_0001;
	}
}

impl std::fmt::Display for ProgramStatusWord {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}{}{}{}{}{}{}{}",
			if self.contains(Self::Sign) { "N" } else { "-" },
			if self.contains(Self::Overflow) { "V" } else { "-" },
			if self.contains(Self::DirectPage) { "P" } else { "-" },
			if self.contains(Self::Break) { "B" } else { "-" },
			if self.contains(Self::HalfCarry) { "H" } else { "-" },
			if self.contains(Self::Interrupt) { "I" } else { "-" },
			if self.contains(Self::Zero) { "Z" } else { "-" },
			if self.contains(Self::Carry) { "C" } else { "-" },
		)
	}
}

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
	pub fn tick(&mut self, memory: &mut Memory) {
		self.cycle_counter += 1;
		if self.run_state != RunState::Running {
			return;
		}
		#[cfg(debug_assertions)]
		trace!("SMP tick {}", self.cycle_counter);

		self.timers.tick(self.control);

		// Fetch next instruction
		if self.instruction_cycle == 0 {
			self.current_opcode = self.read_next_pc(memory);
			trace!(
				"(@{}) fetch instruction [{:04x}] = {:02x}",
				self.cycle_counter,
				self.pc.wrapping_sub(1),
				self.current_opcode
			);
		}

		// Execute tick
		let instruction_result = OPCODE_TABLE[self.current_opcode as usize](
			self,
			memory,
			self.instruction_cycle,
			self.last_instruction_state,
		);
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

	#[allow(unused)]
	#[track_caller]
	fn write(&mut self, address: u16, value: u8, memory: &mut Memory) {
		match address {
			TEST => self.test_write(value),
			CONTROL => self.control_write(value),
			CPUIO0 | CPUIO1 | CPUIO2 | CPUIO3 => self.ports.write(address - CPUIO0, value),
			_ => self.memory_write(address, value, memory),
		}
	}

	#[track_caller]
	fn read(&mut self, address: u16, memory: &Memory) -> u8 {
		match address {
			TEST => self.test.0,
			CONTROL => self.control.0,
			CPUIO0 | CPUIO1 | CPUIO2 | CPUIO3 => self.ports.read(address - CPUIO0),
			_ => memory.read(address, self.control.contains(ControlRegister::BootRomEnable)),
		}
	}

	/// Reads memory at the current program counter and advances it afterwards.
	fn read_next_pc(&mut self, memory: &Memory) -> u8 {
		let data = self.read(self.pc, memory);
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
}
