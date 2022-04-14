# spcasm

spcasm is a simple assembler for the SPC700 processor architecture, famously used in the S-SMP as the sound co-processor of the SNES.

spcasm assembles a binary memory dump of the SPC700's main memory. No entry point-related things are handled. You'll have to process this memory dump further, for example you can include it in an SNES ROM and transfer it via the 6502 main processor.

## Installation

This is a normal Rust project without special dependencies. Use `cargo` for building, running, testing etc.
