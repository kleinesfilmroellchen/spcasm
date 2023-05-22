# spcasm

spcasm is a modern assembler for the SPC700 processor architecture, famously used in the S-SMP as the sound co-processor of the SNES. spcasm is designed to be user-friendly, fast and feature-rich. Different from other assemblers for the SNES architecture family, spcasm is not intended for ROM hacking and patching in general, but regular from-scratch software development.

[This project is just for fun](https://justforfunnoreally.dev/). Still, there's many reasons to use spcasm:

- **spcasm is fast. Best-in-class fast.** spcasm assembles about 365.714 KB/s on a modern computer, which is five times the entire address space. The assembler is between 1.3 (Windows) and 3.2 (Linux) times as fast as Asar. The BRR encoder runs at 1.6 MB/s on best compression, which is two times as fast as BRRTools.
- **spcasm runs in your browser.** (No, really, no server.) This is achieved with the power of WebAssembly, allowing spcasm to run so fast that a compile button would be overkill. See <https://kleinesfilmroellchen.github.io/spcasm> for a live demo based on the master branch.
- **BRR support and integration:** spcasm includes the `brr` binary which is intended to be a replacement for the functionality that BRRTools provides. However, including S-SMP samples ("BRR" samples) in your binary (useful if all your samples fit in ARAM at once) is as simple as

```assembly
brr "my-sample.wav"
```

- Nice errors, helpful warnings and information about possible pitfalls. spcasm reports errors in a nice format, if your terminal supports it:
  ![](https://raw.githubusercontent.com/kleinesfilmroellchen/spcasm/main/doc/error-examples.gif)

## Installation

This is a normal Rust project without special dependencies. Use `cargo` or [`just`](https://just.systems/) for building, running, testing etc.

## Usage

```trycmd
$ spcasm --help
A modern, user-friendly SPC700 assembler.

Usage: spcasm[EXE] [OPTIONS] <INPUT> [OUTPUT]

Arguments:
  <INPUT>
          Assembly file to assemble

  [OUTPUT]
          Binary output file

Options:
  -w, --ignore <IGNORE>
          Warnings to silence

  -W, --error <ERROR>
          Warnings to turn into a hard error

  -l, --reference-pass-limit <REFERENCE_PASS_LIMIT>
          Limit for the number of reference resolution passes spcasm will perform.
          
          Usually 2-3 passes are enough and very high pass numbers often indicate infinite loops. If
          this number of passes is exceeded during reference resolution, spcasm will report
          unresolved references as normal.
          
          [default: 10]

  -r, --macro-recursion-limit <MACRO_RECURSION_LIMIT>
          Limit for the number of recursive macro calls allowed by spcasm.
          
          Increase this limit carefully; very high recursion amounts are usually caused by
          infinitely recursive macros. Any recursion exceeding this value will cause a specific
          error.
          
          [default: 1000]

  -f, --output-format <OUTPUT_FORMAT>
          Format to output to
          
          [default: elf]

          Possible values:
          - elf:      Output the binary data within a .data section of an ELF file
          - plain:    Output just the binary data
          - hex-dump: Dump hexadecimal representation in a pretty format like in a hex editor

  -d, --dump-references
          Dump all references and their final values / locations

  -a, --dump-ast
          Dump the program's abstract syntax tree. This is a debugging feature and most likely not
          useful to the end user.
          
          WARNING: This option will, in specific circumstances, loop forever trying to print
          recursive data structures. This can happen on well-formed programs.

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version

```

spcasm follows the mnemonic conventions from [this APU manual](https://web.archive.org/web/20060208001231/http://www.alpha-ii.com/snesmusic/files/spc700_apu_manual.txt). The directive and macro syntax is a subset of the [VASM oldstyle syntax](http://sun.hasenbraten.de/vasm/release/vasm_6.html#Oldstyle-Syntax-Module) and also supports some [Asar](https://github.com/RPGHacker/asar) features. If you're missing one of the features of vasm or Asar, it is probably appreciated in spcasm!

## [Documentation](doc/src/SUMMARY.md)

For all further information read the above.

## Contributing

See the documentation for developer information, including further information on how to build and configure spcasm.

You can contribute by:

- implementing missing features from Asar or vasm
- reporting and/or fixing bugs
- writing documentation; the code itself is well-documented, but user documentation is lacking.

## Acknowledgements

spcasm owes a lot to the SNES hacking and development scene (which I'm not even a part of!). In particular, I want to thank [IsoFrieze](https://isofrieze.com/) and his [SNES APU video series](https://www.youtube.com/watch?v=zrn0QavLMyo&list=PLHQ0utQyFw5JD2wWda50J8XuzQ2cFr8RX) for nerdsniping me into this project. Also, the various reverse-engineered SPC700 documentation has been a huge help; I've not reversed anything myself.

Software-wise, the three main inspirations, especially in terms of feature set are [Asar](https://github.com/RPGHacker/asar), [BRRTools](https://github.com/Optiroc/BRRtools) and [vasm](http://sun.hasenbraten.de/vasm/).

## License

This project is licensed under BSD 2-clause. This license does not apply to the fonts included for the web frontend; these are licensed under the SIL Open Font License.
