# Using spcasm

*To run spcasm from source, use `cargo r --` and provide your arguments after the two dashes.*

At the moment, the spcasm command line interface is very simple:

```sh
spcasm INPUT OUTPUT
```

spcasm will read the assembly file INPUT and assemble it into a contiguous binary. This binary represents the SPC700's memory space. spcasm then writes an ELF to OUTPUT with a single .data section that contains this binary. Note that no other properties of the output ELF are specified and must be disregarded if the ELF is processed further.

If an error occurs during any step of the assembly process, spcasm will provide you with a nice error of what went wrong and possibly how to solve it:

![spcasm error demonstration](https://user-images.githubusercontent.com/28656157/164973851-d66c5fa3-8bed-43b6-b7c2-e66cc53592c6.png)

There may also be hints and warnings (identifiable by the pointing finger or exclamation mark that replace the red x) which inform you of possible problems with the assembly code that aren't fatal.

## brri

You can use the `brri` binary (`cargo r --release --features=clap --bin=brri --`) for interactively testing the BRR encoder and decoder. This binary is undocumented here as its features change frequently. Use `brri --help` for help.
