//! ELF writing support
#![allow(clippy::module_name_repetitions)]

use std::cell::Cell;
use std::io::Write;
use std::pin::Pin;

use object::elf::{ELFOSABI_STANDALONE, EM_PDSP, ET_EXEC, PT_LOAD, SHF_ALLOC, SHF_EXECINSTR, SHF_WRITE, SHT_PROGBITS};
use object::write::elf::{FileHeader, ProgramHeader, SectionHeader, SectionIndex};
use object::write::StringId;
#[allow(unused)]
use smartstring::alias::String;

use crate::Segments;

/// Save all the metadata since object is too dumb to do this itself.
struct SegmentMetadata {
	name:            std::string::String,
	// The following values are set later, once the data has been constructed into immutable memory.
	name_id:         Cell<Option<StringId>>,
	section_index:   Cell<Option<SectionIndex>>,
	elf_file_offset: Cell<Option<u64>>,
	data:            Vec<u8>,
	start_address:   u64,
}

/// Writes assembled data to an ELF file, separating it by segments.
///
/// # Errors
/// I/O errors.
#[allow(clippy::missing_panics_doc, clippy::cast_sign_loss, clippy::cast_possible_truncation, clippy::cast_lossless)]
pub fn write_to_elf(output_stream: &mut impl Write, data: Segments<u8>) -> Result<(), std::io::Error> {
	// FIXME: This appears to be a bug with object; it can't write past a Vec's capacity though it has a mutable
	// reference and can totally do it. Therefore, preallocate a gigantic buffer, which is inefficient but works.
	let mut buffer = Vec::with_capacity(65536);
	let mut elf = object::write::elf::Writer::new(object::Endianness::Little, false, &mut buffer);

	// Sequence stolen from https://github.com/gimli-rs/object/blob/master/crates/examples/src/bin/elfcopy.rs
	// Since object is a shitty library, ELF contents have to be written with basically no library support; you're 100%
	// responsible for doing everything in the right order. Someone should write a better library for this.

	// It is of UTMOST IMPORTANCE that we write the ELF data in the exact order that we reserve it.
	// Of course, we also have to write all data which we reserve.
	// The current order is:
	// - File header
	// - Program headers
	// - Sections:
	// 	- Null section (we reserve this explicitly since object cannot be trusted)
	// 	- Actual data sections, in proper address order
	// 	- shstrtab section (section header string table)
	// - Section headers (yes, weird position, but I don't want to mess with something that readelf accepts)

	// TODO: Check if Pin is necessary here.
	let mut segments: Vec<Pin<Box<SegmentMetadata>>> = Vec::new();

	// Step 1: Create metadata.
	// Since segments is a BTreeMap, it will always be sorted by address, simplifying later write steps.
	for (segment_start, segment_contents) in data.segments {
		let metadata = Box::pin(SegmentMetadata {
			name:            format!(".text_{:04X}", segment_start),
			name_id:         Cell::new(None),
			section_index:   Cell::new(None),
			elf_file_offset: Cell::new(None),
			data:            segment_contents,
			start_address:   segment_start as u64,
		});
		segments.push(metadata);
	}
	// Borrow checker hint: Segments are now immutable.
	let segments = segments;

	// Step 2: Reserve sections.
	elf.reserve_null_section_index();
	for segment in &segments {
		let name_id = elf.add_section_name(segment.name.as_bytes());
		let section_index = elf.reserve_section_index();
		segment.name_id.set(Some(name_id));
		segment.section_index.set(Some(section_index));
	}
	elf.reserve_shstrtab_section_index();

	// Step 3: Reserve file ranges and program headers.
	elf.reserve_file_header();
	elf.reserve_program_headers(segments.len() as u32);
	for segment in &segments {
		let section_file_offset = elf.reserve(segment.data.len(), 1);
		segment.elf_file_offset.set(Some(section_file_offset as u64));
	}
	elf.reserve_shstrtab();
	elf.reserve_section_headers();

	// Step 4: Write file header.
	elf.write_file_header(&FileHeader {
		os_abi:      ELFOSABI_STANDALONE,
		abi_version: 0,
		e_type:      ET_EXEC,
		// "Sony DSP processor", It might be us? :^)
		e_machine:   EM_PDSP,
		// Doesn't really matter for the ROM, but in practice this is the reset address.
		e_entry:     0xFFC0,
		e_flags:     0,
	})
	.map_err(|_| std::io::Error::from(std::io::ErrorKind::Other))?;

	// Step 5: Write program headers.
	elf.write_align_program_headers();
	for segment in &segments {
		elf.write_program_header(&ProgramHeader {
			p_type:   PT_LOAD,
			p_flags:  0,
			p_offset: segment.elf_file_offset.get().unwrap(),
			p_vaddr:  segment.start_address,
			p_paddr:  segment.start_address,
			p_filesz: segment.data.len() as u64,
			p_memsz:  segment.data.len() as u64,
			p_align:  1,
		});
	}

	// Step 6: Write sections.
	for segment in &segments {
		elf.pad_until(segment.elf_file_offset.get().unwrap() as usize);
		elf.write(&segment.data);
	}

	// Step 7: Write symbols and strings.
	elf.write_shstrtab();

	// Step 8: Write section headers.
	elf.write_null_section_header();
	elf.write_symtab_section_header(0);
	for segment in &segments {
		elf.write_section_header(&SectionHeader {
			sh_addr:      segment.start_address,
			name:         segment.name_id.get(),
			sh_type:      SHT_PROGBITS,
			// Read, write, execute.
			sh_flags:     (SHF_ALLOC | SHF_WRITE | SHF_EXECINSTR) as u64,
			sh_offset:    segment.elf_file_offset.get().unwrap(),
			sh_size:      segment.data.len() as u64,
			sh_link:      0,
			sh_info:      0,
			sh_addralign: 1,
			sh_entsize:   0,
		});
	}
	elf.write_shstrtab_section_header();

	assert_eq!(elf.reserved_len(), elf.len());

	output_stream.write_all(&buffer)?;
	output_stream.flush()?;

	Ok(())
}
