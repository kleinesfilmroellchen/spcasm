//! S-SMP (SPC700 CPU) emulator.

mod ops;

use bitflags::bitflags;
use log::{debug, error, trace};

use self::ops::InstructionInternalState;
use crate::memory::Memory;
use crate::smp::ops::OPCODE_TABLE;

/// State of the microprocessor.
pub struct Smp {
	/// TEST register.
	pub test:      TestRegister,
	/// CONTROL register.
	pub control:   ControlRegister,
	/// Accumulator.
	pub a:         u8,
	/// X index register.
	pub x:         u8,
	/// Y index register.
	pub y:         u8,
	/// Program Counter.
	pub pc:        u16,
	/// Stack Pointer.
	pub sp:        u8,
	/// Program Status Word (flags register).
	pub psw:       ProgramStatusWord,
	/// Main CPU I/O ports.
	pub ports:     CpuIOPorts,
	/// CPU-internal timers.
	pub timers:    Timers,
	/// Cycle counter for debugging purposes.
	cycle_counter: u128,

	/// Cycle within an instruction.
	instruction_cycle:      usize,
	/// Opcode of the instruction being executed.
	current_opcode:         u8,
	/// Last instruction state returned by the instruction.
	last_instruction_state: InstructionInternalState,

	/// CPU execution state.
	run_state: RunState,
}

/// CPU execution state.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum RunState {
	#[default]
	Running,
	Crashed,
	#[allow(unused)]
	Halted,
	#[allow(unused)]
	WaitingForInterrupt,
}

/// Main CPU I/O ports.
#[derive(Default)]
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
	pub fn write(&mut self, port_number: u16, value: u8) {
		Self::check_port_number(port_number);

		trace!("CPUIO {0} = {1:02x} ({1})", port_number, value);
		self.write_ports[port_number as usize] = value;
	}

	/// Perform a read from the main CPU.
	#[inline]
	pub fn read(&mut self, port_number: u16) -> u8 {
		Self::check_port_number(port_number);

		trace!("Read CPUIO {0} = {1:02x} ({1})", port_number, self.read_ports[port_number as usize]);
		self.read_ports[port_number as usize]
	}

	/// Reset the main CPU input port to 0.
	#[inline]
	pub fn reset_port(&mut self, port_number: u16) {
		Self::check_port_number(port_number);

		trace!("Reset CPUIO {0}", port_number);
		self.read_ports[port_number as usize] = 0;
	}
}

/// CPU clock rate (Hz)
pub const CPU_RATE: usize = 2_048_000;

/// Internal CPU timers.
pub struct Timers {
	/// Current timer output value (TnOUT).
	pub timer_out:        [u8; 3],
	/// Timer divisor values (TnDIV).
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
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct TestRegister(u8);
/// Internal CONTROL register.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ControlRegister(u8);
/// Program Status Word (flags register).
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ProgramStatusWord(u8);

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

const TEST: u16 = 0x00F0;
const CONTROL: u16 = 0x00F1;
const CPUIO0: u16 = 0x00F4;
const CPUIO1: u16 = 0x00F5;
const CPUIO2: u16 = 0x00F6;
const CPUIO3: u16 = 0x00F7;

impl Smp {
	/// Create a new reset CPU.
	pub fn new(memory: &mut Memory) -> Self {
		let control = ControlRegister(0xB0);
		Self {
			test: TestRegister(0xA0),
			control,
			// Reset values of A, X, Y, SP seem to be unknown.
			a: 0,
			x: 0,
			y: 0,
			sp: 0,
			pc: memory.read_word(0xFFFE, control.contains(ControlRegister::BootRomEnable)),
			psw: ProgramStatusWord(0),
			ports: CpuIOPorts::default(),
			timers: Timers::new(),
			cycle_counter: 0,
			instruction_cycle: 0,
			current_opcode: 0,
			last_instruction_state: InstructionInternalState::default(),
			run_state: RunState::default(),
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
			debug!("fetch {:04x} = {:02x}", self.pc - 1, self.current_opcode);
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
		((self.psw & ProgramStatusWord::DirectPage).0 as u16) << 3
	}

	#[inline]
	fn set_zero(&mut self, value: u8) {
		self.psw.set(ProgramStatusWord::Zero, value == 0);
	}

	#[inline]
	fn set_negative(&mut self, value: u8) {
		self.psw.set(ProgramStatusWord::Sign, (value as i8) < 0);
	}

	#[inline]
	fn set_subtract_carry(&mut self, op1: i8, op2: i8) {
		self.psw.set(ProgramStatusWord::Carry, op1.checked_sub(op2).is_none());
	}

	#[inline]
	#[allow(unused)]
	fn set_add_carry(&mut self, op1: i8, op2: i8) {
		self.psw.set(ProgramStatusWord::Carry, op1.checked_add(op2).is_none());
	}

	#[allow(unused)]
	fn write(&mut self, address: u16, value: u8, memory: &mut Memory) {
		match address {
			TEST => self.test_write(value),
			CONTROL => self.control_write(value),
			CPUIO0 | CPUIO1 | CPUIO2 | CPUIO3 => self.ports.write(address - CPUIO0, value),
			_ => self.memory_write(address, value, memory),
		}
	}

	fn read(&mut self, address: u16, memory: &mut Memory) -> u8 {
		match address {
			TEST => self.test.0,
			CONTROL => self.control.0,
			CPUIO0 | CPUIO1 | CPUIO2 | CPUIO3 => self.ports.read(address - CPUIO0),
			_ => memory.read(address, self.control.contains(ControlRegister::BootRomEnable)),
		}
	}

	/// Reads memory at the current program counter and advances it afterwards.
	fn read_next_pc(&mut self, memory: &mut Memory) -> u8 {
		let data = self.read(self.pc, memory);
		self.pc += 1;
		data
	}

	fn memory_write(&mut self, address: u16, value: u8, memory: &mut Memory) {
		if self.test.contains(TestRegister::RamWriteDisable) {
			debug!("RAM write to {} is disabled via TEST register", address);
			return;
		}
		memory.write(address, value);
	}

	fn test_write(&mut self, value: u8) {
		trace!("TEST = {:08b}", value);
		self.test = TestRegister(value);

		if self.test.contains(TestRegister::Crash) {
			error!("CPU was crashed via TEST register");
			self.run_state = RunState::Crashed;
		}
	}

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
}
