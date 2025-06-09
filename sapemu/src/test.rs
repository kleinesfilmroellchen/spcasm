//! Tests based on single step tests' single-instruction tests that include bus activity (important to obtain
//! cycle-accuracy).
//!
//! - Test files are here: <https://github.com/SingleStepTests/spc700/tree/main/v1>
//! - File format is described here: <https://github.com/SingleStepTests/65816/blob/main/README.md>
//!
//! Example test:
//!
//! ```json
//! {"name":"00 0000","initial":{"pc":30256,"a":56,"x":78,"y":127,"sp":236,"psw":145,"ram":[[30256,0]]},
//!   "final":{"a":56,"x":78,"y":127,"sp":236,"pc":30257,"psw":145,"ram":[[30256,0]]},
//!   "cycles":[[30256,0,"read"],[30257,null,"read"]]}
//! ```

use std::fmt::Write;

use log::info;
use rstest::rstest;
use serde::Deserialize;
use time::macros::format_description;

use crate::dsp::registers::DspRegisters;
use crate::memory::Memory;
use crate::smp::peripherals::{ControlRegister, CpuIOPorts, ProgramStatusWord, TestRegister};
use crate::smp::{
	CONTROL, CPUIO0, CPUIO1, CPUIO2, CPUIO3, DSPADDR, DSPDATA, Smp, T0DIV, T0OUT, T1DIV, T1OUT, T2DIV, T2OUT, TEST,
};

#[derive(Deserialize, Debug, Clone)]
struct Test {
	name:          String,
	#[serde(rename = "initial")]
	initial_state: ProcessorState,
	#[serde(rename = "final")]
	final_state:   ProcessorState,
	cycles:        Vec<Cycle>,
}

#[derive(Deserialize, Debug, Clone)]
struct ProcessorState {
	pc:  u16,
	a:   u8,
	x:   u8,
	y:   u8,
	sp:  u8,
	psw: u8,
	ram: RamState,
}

impl From<&ProcessorState> for Smp {
	fn from(val: &ProcessorState) -> Self {
		Self {
			a: val.a,
			x: val.x,
			y: val.y,
			pc: val.pc,
			sp: val.sp,
			test: val.ram.test_register().unwrap_or_default(),
			// Tests assume that boot ROM is disabled if the control register is not explicitly set via RAM inputs.
			control: val.ram.control_register().unwrap_or(ControlRegister::empty()),
			ports: val.ram.cpuio_ports(),
			psw: ProgramStatusWord(val.psw),
			..Default::default()
		}
	}
}

impl PartialEq<Smp> for ProcessorState {
	fn eq(&self, other: &Smp) -> bool {
		self.pc == other.pc
			&& self.a == other.a
			&& self.x == other.x
			&& self.y == other.y
			&& self.sp == other.sp
			&& self.psw == other.psw.bits()
	}
}

impl ProcessorState {
	pub fn mismatch_info(&self, smp: &Smp) -> String {
		let mut info = String::new();
		if smp.a != self.a {
			writeln!(info, "expected a = {:02x} but got {:02x}", self.a, smp.a).unwrap();
		}
		if smp.x != self.x {
			writeln!(info, "expected x = {:02x} but got {:02x}", self.x, smp.x).unwrap();
		}
		if smp.y != self.y {
			writeln!(info, "expected y = {:02x} but got {:02x}", self.y, smp.y).unwrap();
		}
		if smp.psw.bits() != self.psw {
			writeln!(info, "expected psw = {} but got {}", ProgramStatusWord(self.psw), smp.psw).unwrap();
		}
		if smp.sp != self.sp {
			writeln!(info, "expected sp = 01{:02x} but got 01{:02x}", self.sp, smp.sp).unwrap();
		}
		if smp.pc != self.pc {
			writeln!(info, "expected pc = {:04x} but got {:04x}", self.pc, smp.pc).unwrap();
		}
		info
	}
}

#[derive(Deserialize, Debug, Clone)]
#[repr(transparent)]
struct RamState(Vec<MemoryCellState>);

impl From<&RamState> for Memory {
	fn from(val: &RamState) -> Self {
		let mut memory = Self::new();
		for MemoryCellState { address, value } in &val.0 {
			memory.write(*address, *value);
		}
		memory
	}
}

/// Any test accessing these addresses will be wrong since it doesn't properly account for hardware behavior.
/// See <https://github.com/SingleStepTests/spc700/issues/1>.
const IGNORED_ADDRESSES: &[u16] = &[TEST, CONTROL, DSPADDR, DSPDATA, T0DIV, T0OUT, T1DIV, T1OUT, T2DIV, T2OUT];

impl PartialEq<Memory> for RamState {
	fn eq(&self, other: &Memory) -> bool {
		self.0.iter().all(|MemoryCellState { address, value }| {
			IGNORED_ADDRESSES.contains(address) || other.read(*address, false) == *value
		})
	}
}

impl RamState {
	pub fn test_register(&self) -> Option<TestRegister> {
		self.0.iter().find_map(
			|MemoryCellState { address, value }| {
				if *address == TEST { Some(TestRegister::from_bits_retain(*value)) } else { None }
			},
		)
	}

	pub fn control_register(&self) -> Option<ControlRegister> {
		self.0.iter().find_map(
			|MemoryCellState { address, value }| {
				if *address == CONTROL { Some(ControlRegister::from_bits_retain(*value)) } else { None }
			},
		)
	}

	pub fn cpuio_ports(&self) -> CpuIOPorts {
		let mut ports = CpuIOPorts::default();
		for MemoryCellState { address, value } in &self.0 {
			match *address {
				CPUIO0 => {
					ports.write(0, *value);
					ports.write_to_smp::<0>(*value);
				},
				CPUIO1 => {
					ports.write(1, *value);
					ports.write_to_smp::<1>(*value);
				},
				CPUIO2 => {
					ports.write(2, *value);
					ports.write_to_smp::<2>(*value);
				},
				CPUIO3 => {
					ports.write(3, *value);
					ports.write_to_smp::<3>(*value);
				},
				_ => {},
			}
		}
		ports
	}

	pub fn mismatch_info(&self, memory: &Memory) -> String {
		self.0
			.iter()
			.filter_map(|MemoryCellState { address, value: expected }| {
				let actual = memory.read(*address, false);
				if *expected == actual {
					None
				} else {
					Some(format!("at address {address:04X}: expected {expected:02x} but got {actual:02x}\n"))
				}
			})
			.collect::<String>()
	}
}

#[derive(Deserialize, Debug, Clone)]
#[serde(from = "(u16, u8)")]
struct MemoryCellState {
	address: u16,
	value:   u8,
}

impl From<(u16, u8)> for MemoryCellState {
	fn from((address, value): (u16, u8)) -> Self {
		Self { address, value }
	}
}

#[allow(unused)]
#[derive(Deserialize, Debug, Clone)]
#[serde(untagged, try_from = "(Option<u16>, Option<u8>, String)")]
enum Cycle {
	Read { address: u16, value: Option<u8> },
	Write { address: u16, value: u8 },
	Wait,
}

impl TryFrom<(Option<u16>, Option<u8>, String)> for Cycle {
	type Error = String;

	fn try_from((address, value, activity): (Option<u16>, Option<u8>, String)) -> Result<Self, Self::Error> {
		match activity.as_str() {
			"read" => Ok(Self::Read { address: address.ok_or_else(|| "missing read address".to_string())?, value }),
			"write" => Ok(Self::Write {
				address: address.ok_or_else(|| "missing write address".to_string())?,
				value:   value.ok_or_else(|| "missing write value".to_string())?,
			}),
			"wait" => Ok(Self::Wait),
			_ => Err("unknown type".into()),
		}
	}
}

/// rstest limitation: we have to generate all values in advance, unfortunately.
/// python: `for i in range(0xff+1): print(hex(i), end=', ')`
#[rstest]
fn single_instruction(
	#[values(
		0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf, 0x10, 0x11, 0x12, 0x13, 0x14,
		0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26,
		0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
		0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a,
		0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c,
		0x5d, 0x5e, 0x5f, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e,
		0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x80,
		0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91, 0x92,
		0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4,
		0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6,
		0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8,
		0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
		0xdb, 0xdc, 0xdd, 0xde, 0xdf, 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec,
		0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe,
		0xff
	)]
	instruction: u8,
) {
	use crate::smp::DSPDATA;

	let _ = simple_logger::SimpleLogger::new()
		.with_level(log::LevelFilter::Trace)
		.with_utc_timestamps()
		.with_colors(true)
		.with_timestamp_format(format_description!(version = 2, "[second].[subsecond digits:6]"))
		.init();

	let file_name = format!("https://raw.githubusercontent.com/SingleStepTests/spc700/main/v1/{instruction:02x}.json");
	let maybe_test_file: Result<_, reqwest::Error> = try { reqwest::blocking::get(file_name)?.bytes()? };
	match maybe_test_file {
		Ok(file_bytes) => {
			let mut copy = file_bytes.to_vec();
			let test_file: Vec<Test> = simd_json::serde::from_slice(&mut copy).expect("couldn't parse test set");
			for test in test_file {
				if test
					.initial_state
					.ram
					.0
					.iter()
					.chain(test.final_state.ram.0.iter())
					.any(|x| IGNORED_ADDRESSES.contains(&x.address))
				{
					info!("skipping test {} since it uses ignored addresses", test.name);
					continue;
				}
				info!("#######################################\nperforming test {}...", test.name);
				let mut smp: Smp = (&test.initial_state).into();
				let mut memory: Memory = (&test.initial_state.ram).into();
				let mut dsp = DspRegisters::default();
				dsp.write(memory.ram[DSPADDR as usize], memory.ram[DSPDATA as usize]);
				// TODO: Check cycle behavior.
				let cycles_taken = test.cycles.len();
				for _ in 0 .. cycles_taken {
					smp.tick(&mut memory, &mut dsp);
				}
				memory.copy_mapped_registers_from_smp(&smp);
				memory.ram[DSPDATA as usize] = dsp.read(memory.ram[DSPADDR as usize]);
				assert!(
					test.final_state.ram == memory,
					"memory mismatch at test {}: {}",
					test.name,
					test.final_state.ram.mismatch_info(&memory)
				);
				assert!(
					test.final_state == smp,
					"cpu mismatch at test {}: {}",
					test.name,
					test.final_state.mismatch_info(&smp)
				);
			}
		},
		Err(why) => {
			eprintln!(" - couldn't download the testset: {why}");
		},
	}
}
