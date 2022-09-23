# Using spcasm

_To run spcasm from source, use `cargo r --` and provide your arguments after the two dashes._

```
SPC700 assembler

USAGE:
    spcasm.exe [OPTIONS] <INPUT> [OUTPUT]

ARGS:
    <INPUT>
            Assembly file to assemble

    <OUTPUT>
            Binary output file

OPTIONS:
    -f, --output-format <OUTPUT_FORMAT>
            Format to output to.

            - elf: Output the binary data within a .data section of an ELF file.

            - plain: Output just the binary data.

            - hexdump: Dump hexadecimal representation in a pretty format like in a hex editor.

            [default: elf]
            [possible values: elf, plain, hex-dump]

    -h, --help
            Print help information

    -V, --version
            Print version information

    -w, --ignore <IGNORE>
            Warnings to silence

    -W, --error <ERROR>
            Warnings to turn into a hard error
```

spcasm will read the assembly file INPUT and assemble it into a contiguous binary. This binary represents the SPC700's memory space.

The output file is optional; omit it in order to dry-run the assembler and check that the assembly code does indeed compile. Use `-` to output to stdout, this is especially useful in combination with a hexdump.

If an error occurs during any step of the assembly process, spcasm will provide you with a nice error of what went wrong and possibly how to solve it:

![spcasm error demonstration](https://user-images.githubusercontent.com/28656157/164973851-d66c5fa3-8bed-43b6-b7c2-e66cc53592c6.png)

There may also be hints and warnings (identifiable by the pointing finger or exclamation mark that replace the red x) which inform you of possible problems with the assembly code that aren't fatal. These warnings can be turned off with `-w` or turned into hard errors with `-W`. Use `all` as a  shorthand for all warnings. Note that spcasm will print error "codes" with the prefix `spcasm::` but you do not have to include this prefix when referencing a warning on the command line, meaning that `-w label_shenanigans` and `-w spcasm::label_shenanigans` mean the same thing.

[All errors, warning and advice messages are documented here in detail.](errors.md)

## brr

You can use the `brr` binary (`cargo r --spcasm-release --bin=brr --`) for using and testing the BRR encoder and decoder directly. This binary is undocumented here as its features change frequently. Use `brr --help` for help.
