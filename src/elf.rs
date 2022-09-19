//! ELF writing support
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]

use std::fs::File;
use std::io::{BufWriter, Write};

use object::write::Object;

/// Write the assembled data to a bare-bones ELF file.
/// # Errors
/// I/O errors.
pub fn write_to_elf(output_stream: &mut impl Write, data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
	let mut elf = Object::new(object::BinaryFormat::Elf, object::Architecture::Avr, object::Endianness::Little);
	let section = elf.add_section(".data".into(), ".data".into(), object::SectionKind::Other);
	elf.set_section_data(section, data, 1);
	elf.write_stream(output_stream)?;
	Ok(())
}
