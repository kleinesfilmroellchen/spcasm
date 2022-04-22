# spcasm

spcasm is a simple assembler for the SPC700 processor architecture, famously used in the S-SMP as the sound co-processor of the SNES.

spcasm assembles a binary memory dump of the SPC700's main memory. No entry point-related things are handled. You'll have to process this memory dump further, for example you can include it in an SNES ROM and transfer it via the 6502 main processor.

[This project is just for fun](https://justforfunnoreally.dev/)

## Installation

This is a normal Rust project without special dependencies. Use `cargo` for building, running, testing etc.

## Usage

spcasm follows the mnemonic conventions from [this APU manual](https://web.archive.org/web/20060208001231/http://www.alpha-ii.com/snesmusic/files/spc700_apu_manual.txt). The directive and macro syntax is a subset of the [VASM oldstyle syntax](http://sun.hasenbraten.de/vasm/release/vasm_6.html#Oldstyle-Syntax-Module). If you're missing one of the features here, it would definitely be appreciated here!
