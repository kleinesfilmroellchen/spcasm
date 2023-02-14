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
          
          [default: elf]

          Possible values:
          - elf:      Output the binary data within a .data section of an ELF file
          - plain:    Output just the binary data
          - hex-dump: Dump hexadecimal representation in a pretty format like in a hex editor

  -d, --dump-references
          Dump all references and their final values / locations

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version

```

spcasm will read the assembly file INPUT and assemble it into a contiguous binary. This binary represents the SPC700's memory space.

The output file is optional; omit it in order to dry-run the assembler and check that the assembly code does indeed compile. Use `-` to output to stdout, this is especially useful in combination with a hexdump.

If an error occurs during any step of the assembly process, spcasm will provide you with a nice error of what went wrong and possibly how to solve it:

![spcasm error demonstration](https://raw.githubusercontent.com/kleinesfilmroellchen/spcasm/main/doc/error-examples.gif)

There may also be hints and warnings (identifiable by the pointing finger or exclamation mark that replace the red x) which inform you of possible problems with the assembly code that aren't fatal. These warnings can be turned off with `-w` or turned into hard errors with `-W`. Use `all` as a shorthand for all warnings. Note that spcasm will print error "codes" with the prefix `spcasm::` but you do not have to include this prefix when referencing a warning on the command line, meaning that `-w label_shenanigans` and `-w spcasm::label_shenanigans` mean the same thing.

[All errors, warning and advice messages are documented here in detail.](errors.md)

spcasm uses some terminology which might be unfamiliar to you if you haven't written much assembly code so far, and which is also used inconsistently across assemblers and tools. The [Terminology](terminology.md) page provides a brief glossary explaining what those terms mean.

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
          - treble:
            A precise treble filter that inverts the hardware Gaussian filter exactly
          - brrtools:
            BRRTools' treble filter, which is slightly imprecise, but provided for compatibility
            purposes

  -h, --help
          Print help (see a summary with '-h')

```
