# spcasm

spcasm is a simple assembler for the SPC700 processor architecture, famously used in the S-SMP as the sound co-processor of the SNES.

spcasm assembles an ELF that hijacks another architecture (this must be ignored) and puts the SPC700's main memory into a `.data` section. No entry point-related things are handled. You'll have to process this memory dump further, for example you can include it in an SNES ROM and transfer it via the 6502 main processor.

[This project is just for fun](https://justforfunnoreally.dev/). Still, some reasons to use spcasm:
* Extremely fast: spcasm assembles about 124.858 KB/s on a modern computer. This means that the entire SPC700 address space (64KB), if it were entirely program RAM, can be assembled by spcasm in about half a second.
* Nice errors: spcasm reports errors in a nice format, if your terminal supports it:
![image](https://user-images.githubusercontent.com/28656157/164973851-d66c5fa3-8bed-43b6-b7c2-e66cc53592c6.png)
* Helpful warnings and information about possible pitfalls:
![image](https://user-images.githubusercontent.com/28656157/164979501-4ece4431-735e-471c-a9a8-674df64b23c3.png)

## Installation

This is a normal Rust project without special dependencies. Use `cargo` for building, running, testing etc.

## Usage

`spcasm [input file] [output file]`

The input is UTF-8 encoded assembly. The output will be a binary memory dump.

spcasm follows the mnemonic conventions from [this APU manual](https://web.archive.org/web/20060208001231/http://www.alpha-ii.com/snesmusic/files/spc700_apu_manual.txt). The directive and macro syntax is a subset of the [VASM oldstyle syntax](http://sun.hasenbraten.de/vasm/release/vasm_6.html#Oldstyle-Syntax-Module). If you're missing one of the features here, it would definitely be appreciated here!
