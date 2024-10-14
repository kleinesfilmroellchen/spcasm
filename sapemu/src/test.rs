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

#![allow(unused)]

use bitflags::Flags;
use log::info;
use rstest::rstest;
use serde::Deserialize;
use time::macros::format_description;

use crate::dsp::registers::DspRegisters;
use crate::dsp::Dsp;
use crate::memory::Memory;
use crate::smp::peripherals::{ControlRegister, CpuIOPorts, ProgramStatusWord, TestRegister};
use crate::smp::{Smp, CONTROL, CPUIO0, CPUIO1, CPUIO2, CPUIO3, DSPADDR, TEST};

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
			info.push_str(&format!("expected a = {:02x} but got {:02x}\n", self.a, smp.a));
		}
		if smp.x != self.x {
			info.push_str(&format!("expected x = {:02x} but got {:02x}\n", self.x, smp.x));
		}
		if smp.y != self.y {
			info.push_str(&format!("expected y = {:02x} but got {:02x}\n", self.y, smp.y));
		}
		if smp.psw.bits() != self.psw {
			info.push_str(&format!("expected psw = {} but got {}\n", ProgramStatusWord(self.psw), smp.psw));
		}
		if smp.sp != self.sp {
			info.push_str(&format!("expected sp = 01{:02x} but got 01{:02x}\n", self.sp, smp.sp));
		}
		if smp.pc != self.pc {
			info.push_str(&format!("expected pc = {:04x} but got {:04x}\n", self.pc, smp.pc));
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

impl PartialEq<Memory> for RamState {
	fn eq(&self, other: &Memory) -> bool {
		self.0.iter().all(|MemoryCellState { address, value }| other.read(*address, false) == *value)
	}
}

impl RamState {
	pub fn test_register(&self) -> Option<TestRegister> {
		self.0.iter().find_map(
			|MemoryCellState { address, value }| {
				if *address == TEST {
					Some(TestRegister::from_bits_retain(*value))
				} else {
					None
				}
			},
		)
	}

	pub fn control_register(&self) -> Option<ControlRegister> {
		self.0.iter().find_map(
			|MemoryCellState { address, value }| {
				if *address == CONTROL {
					Some(ControlRegister::from_bits_retain(*value))
				} else {
					None
				}
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

/// Tests that are not run due to issues with `SingleStepTests`' disregard of hardware properties.
/// See <https://github.com/SingleStepTests/spc700/issues/1>.
const IGNORED_TESTS: &[&str] = &[
	"09 01A8", "39 0295", "59 0146", "3A 0355", "47 02DD", "6E 0344", "99 02BD", "A9 01BF", "C7 000C", "D7 00B9",
	"D7 0110", "D7 0111", "DA 0082", // DSP-related
	"03 001B", "02 0097", "07 0123", "06 01D9", "04 02A7", "09 026A", "0E 022E", "0B 03E4", "18 0078", "12 03A4",
	"13 02F3", "1A 00D4", "1B 0034", "23 00FF", "22 018B", "24 0023", "27 0002", "29 00B4", "2B 00EA", "26 01ED",
	"2E 0014", "32 0018", "33 0115", "34 0179", "3B 002F", "3A 0046", "3E 0082", "37 0328", "38 03D5", "42 00C4",
	"43 002E", "44 007E", "46 008B", "49 00B0", "47 00A2", "4B 0379", "52 013E", "57 01C8", "54 0200", "53 0272",
	"58 005F", "5B 01C0", "5A 02CE", "62 0089", "63 01A0", "64 0135", "67 0200", "66 0296", "69 0179", "6B 009B",
	"6E 031A", "74 006C", "79 004C", "78 01B0", "77 0302", "7E 0042", "7B 01BE", "7A 021D", "82 0053", "83 0013",
	"84 0176", "89 0212", "8B 0123", "93 001F", "92 023B", "94 01AE", "8F 03C6", "99 00F0", "9B 00CC", "98 031D",
	"9A 0332", "A2 00AC", "A3 00AF", "A7 009A", "A6 028E", "A9 01A8", "AB 02F8", "B0 0028", "B4 002D", "AF 02B8",
	"B3 022A", "B2 021C", "BA 0087", "B8 0115", "BB 01BF", "B7 0258", "B9 022B", "BF 00A4", "C4 016A", "C2 02F7",
	"C6 00A3", "C7 0173", "CB 001F", "CD 013B", "CA 0227", "D2 0024", "CF 022E", "D3 0065", "D4 000A", "D8 00D3",
	"D9 00B9", "DB 001D", "D7 025A", "DA 0115", "DE 01E6", "E2 013E", "E3 00FC", "E6 0036", "E4 013F", "E7 015C",
	"EB 0158", "F2 0003", "F3 015C", "F9 005A", "F7 025E", "F4 03AA", "FB 0313", "03 01CC", "02 0290", "06 03CF",
	"09 0317", "13 02F4", "18 01CF", "1A 0072", "1B 032C", "22 0221", "24 0149", "27 00A6", "23 0301", "26 02D0",
	"29 014A", "2B 016B", "2E 018E", "32 01C0", "37 0049", "34 035B", "3B 0160", "3E 0138", "3A 03E1", "43 00BA",
	"44 010C", "46 00F4", "47 0116", "49 0148", "4B 0384", "52 020A", "58 00B4", "57 01D0", "5A 0194", "5B 023F",
	"62 017F", "64 0282", "63 03CA", "66 039A", "69 0228", "6B 018B", "6E 03AB", "79 0135", "74 03CC", "7A 02C6",
	"7B 0339", "82 008B", "7E 02B7", "84 02CE", "8B 0159", "93 018D", "97 0119", "92 02D1", "94 02B7", "9B 032E",
	"A2 0209", "A7 0166", "B4 010D", "BA 00DE", "B7 0360", "BB 02E7", "BF 02B9", "C4 0184", "C7 018D", "CB 0162",
	"D4 003A", "D2 0261", "DA 01A1", "D7 0267", "D9 0314", "DE 01EA", "E2 021F", "E3 0173", "E7 0056", "E4 0264",
	"E6 013A", "EB 03C6", "F7 007F", "F3 02EA", "F4 03C1", "F9 00A3", "FB 0375", "18 01F9", "1A 0179", "1B 03BF",
	"27 0176", "24 0309", "23 039E", "29 01DC", "2B 02C4", "2E 0260", "37 0123", "3E 0185", "3B 0351", "43 0225",
	"44 0260", "46 0284", "58 02C7", "5B 02E6", "5A 0302", "62 0203", "6B 01FC", "69 03A2", "79 017D", "7A 02D6",
	"7E 0384", "82 039B", "8B 03B4", "93 0263", "92 034F", "94 03DC", "97 02BB", "9B 0334", "A2 025C", "A7 0173",
	"B4 0137", "BA 00E3", "B7 03CF", "C7 021F", "C4 0393", "CB 01A7", "D4 0081", "D7 036D", "DA 031D", "DE 0347",
	"E2 032B", "E3 025C", "E6 01F4", "E7 01A7", "E4 0322", "F3 0376", "F7 03E2", "1A 02CA", "1B 03CE", "27 01D7",
	"29 0280", "37 0357", "3E 023A", "44 02DE", "5A 0373", "62 02A5", "79 0213", "8B 03C4", "93 035C", "A7 01C6",
	"B4 0279", "BA 02B8", "C7 023A", "CB 026B", "D4 01C3", "D7 03BD", "E6 02E6", "E7 0331", "F3 03D5", "27 020F",
	"44 033F", "A7 0295", "B4 02C7", "C7 0251", "D4 033B", "E6 0331", "27 024E", "A7 031E", "B4 034F", "C7 03A6",
	"D4 0348", "27 0276", "A7 0339", "19 00CA", "29 02C2", "39 0094", "49 01DE", "59 00C5", "99 0162", "A9 0267",
	"B9 02B2", "FA 008F", "19 0278", "29 02CA", "39 015F", "49 036C", "59 012E", "99 0290", "A9 0276", "FA 00D8",
	"19 02DF", "29 038A", "59 01EB", "99 02E0", "A9 02D6", "FA 01CB", "19 02E8", "99 032E", "A9 032B", "FA 02A1",
	"19 03CE", "A9 0334",
];

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
				if IGNORED_TESTS.contains(&test.name.as_ref()) {
					info!("skipping ignored test {}", test.name);
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
