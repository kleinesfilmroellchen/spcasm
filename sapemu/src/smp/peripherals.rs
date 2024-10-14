//! In-CPU peripherals: I/O ports, timers, CONTROL and TEST registers.

use std::marker::ConstParamTy;

use bitflags::bitflags;

use super::CPU_RATE;
use crate::trace;

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
pub struct TestRegister(pub(super) u8);

impl Default for TestRegister {
	fn default() -> Self {
		Self(0xA0)
	}
}

/// Internal CONTROL register.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct ControlRegister(pub(super) u8);

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
