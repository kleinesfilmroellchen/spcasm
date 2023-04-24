//! spcasm binary.

use std::fs::File;
use std::io::Write;

#[allow(unused)]
use smartstring::alias::String;

use crate::{cli, dump_ast, dump_reference_tree, elf, run_assembler, AssemblyCode, AssemblyError};

pub fn main() -> miette::Result<()> {
	use clap::Parser;

	#[cfg(feature = "dep:human-panic")]
	human_panic::setup_panic!(human_panic::metadata!());
	miette::set_hook(Box::new(|_| {
		Box::new(
			miette::MietteHandlerOpts::new().unicode(true).context_lines(3).tab_width(4).with_cause_chain().build(),
		)
	}))?;

	let mut args = cli::SpcasmCli::parse();
	args.warning_flags.expand_all();
	let file_name = args.input;

	let options = std::sync::Arc::new(args.warning_flags);
	let code = AssemblyCode::from_file_or_assembly_error(&file_name.to_string_lossy()).map_err(AssemblyError::from)?;
	// Errors are already reported, so we only need to handle the success case.
	if let Ok((environment, assembled)) = run_assembler(&code, options.clone()) {
		if args.dump_references {
			dump_reference_tree(&environment.read_recursive().globals);
		}

		if args.dump_ast {
			dump_ast(&environment.read_recursive().files.get(&code.name).unwrap().read_recursive().content);
		}

		if *options.had_error.read() {
			std::process::exit(1);
		}

		if let Some(outfile) = args.output {
			let mut outfile: Box<dyn Write> = if outfile.to_string_lossy() == "-" {
				Box::new(std::io::stdout())
			} else {
				Box::new(std::io::BufWriter::new(
					File::options()
						.create(true)
						.truncate(true)
						.write(true)
						.open(outfile)
						.expect("Couldn't open output file"),
				))
			};
			match args.output_format {
				cli::OutputFormat::Elf => elf::write_to_elf(&mut outfile, &assembled).unwrap(),
				cli::OutputFormat::Plain => outfile.write_all(&assembled).unwrap(),
				cli::OutputFormat::HexDump =>
					outfile.write_fmt(format_args!("{}", crate::pretty_hex(&assembled, None))).unwrap(),
			};
		}
		Ok(())
	} else {
		std::process::exit(1);
	}
}
