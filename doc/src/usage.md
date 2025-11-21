# Using spcasm

_To run spcasm from source, use `cargo r --` and provide your arguments after the two dashes._

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

          Possible values:
          - elf:      Output the binary data within a .data section of an ELF file
          - plain:    Output just the binary data
          - hex-dump: Dump hexadecimal representation in a pretty format like in a hex editor
          
          [default: elf]

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

- auto-gen TOC;
  {:toc}

## Files, input and output format

spcasm reads the assembly file `INPUT` as its primary input. This file must be UTF-8 coded and can have either Windows (CRLF) or Unix (LF) newlines. Currently, standard input (`-`) cannot be used as an input file.

For other file operations, such as including source code, binary files, or BRR data, it does not matter whether you specify an absolute or relative path to the input file. All relative paths are relative to the file where that path is used, and all absolute paths are absolute. It is recommended to not use absolute paths since they are not compatible between different machines even with the same operating system.

spcasm will assemble the input file it into an image of the SPC700's memory space. The terminology "ROM" is often used for such an image, both within spcasm and elsewhere, but do note that since the SPC700 has no user-controlled ROM but only RAM, all memory is initially loaded and always modifiable at runtime.

The output file is optional; omit it in order to dry-run the assembler and check that the assembly code does indeed compile. Use `-` to output to standard output (the console), this is especially useful in combination with a `hex-dump`.

The output format determines what kind of output spcasm produces. Note that during a dry run, the output conversion is not performed and any associated errors will not be produced. spcasm currently supports these output formats:

- `hex-dump`: Produce a hex dump of the contiguous ROM; a feature primarily intended for debugging purposes and the web interface. In hex dump output, all segments are combined together with zero padding bytes and printed in a human-readable fashion similar to the output of many hex dump or hex editor programs. spcasm produces 16 columns and as many rows as needed, with each byte printed as a two-digit hex number. Note that there are no row or column headers, and no ASCII sidebar. If you want these traditional hex editor features, use the `plain` output format and inspect the resulting binary with a proper, separate hex editor program such as `xxd`. (In fact, since spcasm can output plain data to standard output, you can pipe spcasm's output directly into a command-line tool like `xxd`.)
- `plain`: Produce the contiguous ROM in raw binary form, which is effectively equivalent to a ROM image. All segments are combined together with zero padding bytes, and any offset into the file corresponds exactly to a memory address. This output format is the default of Asar and other assemblers, and it is most convenient for further processing the image with other tools.
- `elf`: Produce an ELF image. This uses ELF in a similar manner to how embedded system ELF images are usually produced:
  - The ELF format is ELF32, little endian, ABI is "embedded" (0xFF), ELF type "executable", machine "Sony DSP Processor" (63). Entry point is always 0xFFC0, the reset address.
  - Each segment is assembled into a separate ELF section and corresponding program segment. The names of these might change, so do not rely on them, but the program segment type will always be `LOAD` and the section type `PROGBITS`.
  - All sections have the `ALLOC` flag. spcasm may intelligently mark some sections as read-execute only or read-write only, depending on the contents, but if that is not possible, the segment will just be read-write-execute.
  - All section physical and virtual addresses correspond to the actual SPC700 addresses.

As an example, here's what `readelf -a` says about the `tests/opcodes.s` file assembled into an ELF:

```notrycmd
ELF Header:
  Magic:   7f 45 4c 46 01 01 01 ff 00 00 00 00 00 00 00 00
  Class:                             ELF32
  Data:                              2's complement, little endian
  Version:                           1 (current)
  OS/ABI:                            <unknown: ff>
  ABI Version:                       0
  Type:                              EXEC (Executable file)
  Machine:                           Sony DSP processor
  Version:                           0x1
  Entry point address:               0xffc0
  Start of program headers:          52 (bytes into file)
  Start of section headers:          620 (bytes into file)
  Flags:                             0x0
  Size of this header:               52 (bytes)
  Size of program headers:           32 (bytes)
  Number of program headers:         1
  Size of section headers:           40 (bytes)
  Number of section headers:         3
  Section header string table index: 2

Section Headers:
  [Nr] Name              Type            Addr     Off    Size   ES Flg Lk Inf Al
  [ 0]                   NULL            00000000 000000 000000 00      0   0  0
  [ 1] .text_0000        PROGBITS        00000000 000054 000202 00 WAX  0   0  1
  [ 2] .shstrtab         STRTAB          00000000 000256 000016 00      0   0  1
Key to Flags:
  W (write), A (alloc), X (execute), M (merge), S (strings), I (info),
  L (link order), O (extra OS processing required), G (group), T (TLS),
  C (compressed), x (unknown), o (OS specific), E (exclude),
  p (processor specific)

Program Headers:
  Type           Offset   VirtAddr   PhysAddr   FileSiz MemSiz  Flg Align
  LOAD           0x000054 0x00000000 0x00000000 0x00202 0x00202     0x1

 Section to Segment mapping:
  Segment Sections...
   00     .text_0000

```

## Errors and diagnostic terminology

If an error occurs during any step of the assembly process, spcasm will provide you with a nice error of what went wrong and possibly how to solve it:

![spcasm error demonstration](https://raw.githubusercontent.com/kleinesfilmroellchen/spcasm/main/doc/error-examples.gif)

There may also be hints and warnings (identifiable by the pointing finger or exclamation mark that replace the red x) which inform you of possible problems with the assembly code that aren't fatal. These warnings can be turned off with `-w` or turned into hard errors with `-W`. Use `all` as a shorthand for all warnings. Note that spcasm will print error "codes" with the prefix `spcasm::` but you do not have to include this prefix when referencing a warning on the command line, meaning that `-w label_shenanigans` and `-w spcasm::label_shenanigans` mean the same thing.

[All errors, warning and advice messages are documented here in detail.](errors.md)

spcasm uses some terminology which might be unfamiliar to you if you haven't written much assembly code so far, and which is also used inconsistently across assemblers and tools. The [Terminology](terminology.md) page provides a brief glossary explaining what those terms mean.

spcasm follows some simple markup conventions within its output:

- \`backticks\` mark code snippets within explanatory text (just like Markdown and other markup languages). It is also used for invalid syntax in parser and lexer errors.
- 'single quotes' mark reference names, including macro names, macro arguments, etc.
- "double quotes" mark file names, as those are usually given within strings anyways. They also mark invalid syntax in parser errors.

### Screenreader-friendly error output

The error formatting library [miette]() has the ability to output errors in a screenreader-friendly format, using the environment variable `NO_GRAPHICS=1`. spcasm never inhibits this, so you can always use it for controlling the error output:

```trycmd
$ NO_GRAPHICS=1 spcasm tests/errors/file-not-found.spcasmtest
? 1
File "[CWD]/tests/errors/this_definitely_does_not_exist" was not found
    Diagnostic severity: error
    Caused by: No such file or directory (os error 2)
Begin snippet for tests/errors/file-not-found.spcasmtest starting at line 1, column 1

snippet line 1: org 0
snippet line 2: include "this_definitely_does_not_exist"
    label at line 2, columns 1 to 40: File was requested here
diagnostic code: spcasm::io::file_not_found


```

## brr

You can use the `brr` binary (`cargo r --profile=spcasm-release --bin=brr --`) for using and testing the BRR encoder and decoder directly.

```trycmd
$ brr --help
Bit Rate Reduced (BRR) / SNES ADPCM tools

Usage: brr[EXE] [OPTIONS] <COMMAND>

Commands:
  encode-block  Encode a single block of samples
  decode-block  Decode a single block of samples
  encode        Encode a WAV file into a BRR file
  decode        Decode a BRR file into a WAV file
  help          Print this message or the help of the given subcommand(s)

Options:
  -v, --verbose  Print detailed information even for non-interactive commands
  -h, --help     Print help
  -V, --version  Print version

```

The `encode-block` subcommand:

```trycmd
$ brr encode-block --help
Encode a single block of samples. Displays various information about the encoding process, including
how accurately the data compresses under various filter modes. This command is intended for
interactive experimenting with BRR encoding.

Usage: brr[EXE] encode-block [OPTIONS] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES]...

Arguments:
  [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES] [SAMPLES]...
          The samples to encode, 16-bit signed integers. There must be exactly 16 samples to encode.

Options:
  -w, --warm-up <WARM_UP> <WARM_UP>
          Override the previous samples to use for encoding. There must be exactly two of these,
          otherwise the previous samples are assumed to be zero.

  -h, --help
          Print help (see a summary with '-h')

```

The `decode-block` subcommand:

```trycmd
$ brr decode-block --help
Decode a single block of samples. Displays various information about the decoding process. This
command is intended for interactive experimenting with BRR decoding.

Usage: brr[EXE] decode-block [OPTIONS] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK]...

Arguments:
  [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK] [BLOCK]...
          The BRR-encoded block to decode, given in its individual bytes. There must be exactly nine
          bytes.

Options:
  -w, --warm-up <WARM_UP> <WARM_UP>
          Set the previous two decoded samples, 16-bit signed integers. There must be exactly two of
          these, otherwise the previous samples are assumed to be zero.

  -h, --help
          Print help (see a summary with '-h')

```

The `decode` subcommand:

```trycmd
$ brr decode --help
Decode a BRR file into a WAV file

Usage: brr[EXE] decode [OPTIONS] <INPUT> [OUTPUT]

Arguments:
  <INPUT>
          The BRR file to decode. Only raw BRR files are supported right now.

  [OUTPUT]
          Output WAV file to write. The format is always mono 16-bit signed integer with a sample
          rate of 32kHz, matching the SNES DSP.

Options:
  -f, --filter
          Emulate the hardware Gaussian filter. This filter is applied by S-SMP sample playback
          hardware after decoding for a good-enough pitch shift, but it applies even if the pitch is
          not shifted. The emulation helps recover audio data the way it would have been heard on
          original hardware.

  -h, --help
          Print help (see a summary with '-h')

```

The `encode` subcommand:

```trycmd
$ brr encode --help
Encode a WAV file into a BRR file

Usage: brr[EXE] encode [OPTIONS] <INPUT> [OUTPUT]

Arguments:
  <INPUT>
          The WAV file to encode. Only uncompressed WAV (integer or float) is supported. Sample rate
          is not converted, so in order for audio to not be pitch-shifted, the input has to be at
          32kHz, matching the SNES DSP sample rate.

  [OUTPUT]
          Output BRR file to write. By default, a file with the same name but a `.brr` extension is
          used as output.

Options:
  -c, --compression <COMPRESSION>
          Compression level to use; higher levels mean better audio fidelity. 0: Only use filter 0,
          1: Use all filters with non-wrapping optimal shift, 2: Use all filters with optimal shift.
          
          [default: 2]

  -f, --filter [<FILTER>]
          The hardware Gaussian filter of the S-SMP intended for better pitch shifting always has
          the effect of a low-pass filter on the input sample. To counteract this, brr can apply
          various pre-emphasis filters on the audio before encoding. If you enable, but do not
          select a filter, 'treble' is used.

          Possible values:
          - treble:   A precise treble filter that inverts the hardware Gaussian filter exactly
          - brrtools: BRRTools' treble filter, which is slightly imprecise, but provided for
            compatibility purposes

  -l, --loop-point <LOOP_POINT>
          Set the sample's loop point. The ending block has its flags set to signal a looping
          sample, and the loop start block uses filter 0 to prevent glitches.

  -h, --help
          Print help (see a summary with '-h')

```
