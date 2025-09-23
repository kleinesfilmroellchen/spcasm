//! spcasm binary.

use std::fs::File;
use std::io::Write;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use spcasm::{
	AssemblyCode, AssemblyError, cli, dump_ast, dump_reference_tree, elf, run_assembler, run_assembler_into_segments,
};

fn main() -> miette::Result<()> {
	use clap::Parser;

	#[cfg(feature = "human-panic")]
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
			let mut references = environment.read_recursive().globals.values().cloned().collect::<Vec<_>>();
			references.sort_by_cached_key(|reference| {
				reference
					.read()
					.location
					.as_ref()
					.and_then(|location| {
						location
							.try_value(
								reference.read_recursive().source_span(),
								&std::sync::Arc::new(AssemblyCode::new("", &String::new())),
							)
							.ok()
					})
					.map_or_else(|| "(unknown)".to_string(), |location| format!("{location:04X}"))
			});
			dump_reference_tree(&references);
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
						.map_err(AssemblyError::from)?,
				))
			};
			match args.output_format {
				// TODO: Don't do double work assembling here.
				cli::OutputFormat::Elf => {
					let (_, output, maybe_entry_point) = run_assembler_into_segments(&code, options).unwrap();
					if let Some(entry_point) = maybe_entry_point {
						elf::write_to_elf(&mut outfile, output, entry_point).map_err(AssemblyError::from)?;
					} else {
						Err(AssemblyError::MissingStartpos { src: code.clone() })?;
					}
				},
				cli::OutputFormat::Plain => outfile.write_all(&assembled).map_err(AssemblyError::from)?,
				cli::OutputFormat::HexDump => outfile
					.write_fmt(format_args!("{}", spcasm::pretty_hex(&assembled, None)))
					.map_err(AssemblyError::from)?,
			}
		}

		Ok(())
	} else {
		std::process::exit(1);
	}
}
