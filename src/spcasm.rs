//! spcasm binary.

use std::fs::File;
use std::io::Write;

use crate::{cli, elf, dump_reference_tree, run_assembler_on_file};

pub fn main() -> miette::Result<()> {
	use clap::Parser;

	use crate::cli::BackendOptions;

	miette::set_hook(Box::new(|_| {
		Box::new(
			miette::MietteHandlerOpts::new().unicode(true).context_lines(3).tab_width(4).with_cause_chain().build(),
		)
	}))?;

	let mut args = cli::SpcasmCli::parse();
	args.warning_flags.expand_all();
	let file_name = args.input;

	let (environment, assembled) =
		run_assembler_on_file(&file_name.to_string_lossy(), std::sync::Arc::new(args.warning_flags))?;

	if (args.dump_references) {
		dump_reference_tree(&environment.borrow().globals);
	}

	if let Some(outfile) = args.output {
		let mut outfile: Box<dyn Write> = if outfile.to_string_lossy() == "-" {
			Box::new(std::io::stdout())
		} else {
			Box::new(
				File::options()
					.create(true)
					.truncate(true)
					.write(true)
					.open(outfile)
					.expect("Couldn't open output file"),
			)
		};
		match args.output_format {
			cli::OutputFormat::Elf => elf::write_to_elf(&mut outfile, &assembled).unwrap(),
			cli::OutputFormat::Plain => outfile.write_all(&assembled).unwrap(),
			cli::OutputFormat::HexDump =>
				outfile.write_fmt(format_args!("{}", crate::pretty_hex(&assembled, None))).unwrap(),
		};
	}
	Ok(())
}
