# spcasm

spcasm is a simple assembler for the SPC700 processor architecture, famously used in the S-SMP as the sound co-processor of the SNES.

spcasm assembles an ELF that hijacks another architecture (this must be ignored) and puts the SPC700's main memory into a `.data` section. No entry point-related things are handled. You'll have to process this memory dump further, for example you can include it in an SNES ROM and transfer it via the 65c816 main processor.

[This project is just for fun](https://justforfunnoreally.dev/). Still, some reasons to use spcasm:
* Extremely fast: spcasm assembles about 124.858 KB/s on a modern computer. This means that the entire SPC700 address space (64KB), if it were entirely program RAM, can be assembled by spcasm in about half a second.
* BRR integration: Including S-SMP samples ("BRR" samples) in your binary (useful if all your samples fit in ARAM at once) is as simple as
```assembly
brr "my-sample.wav"
```
* Nice errors: spcasm reports errors in a nice format, if your terminal supports it:
![image](https://user-images.githubusercontent.com/28656157/164973851-d66c5fa3-8bed-43b6-b7c2-e66cc53592c6.png)
* Helpful warnings and information about possible pitfalls:
![image](https://user-images.githubusercontent.com/28656157/164979501-4ece4431-735e-471c-a9a8-674df64b23c3.png)

## Installation

This is a normal Rust project without special dependencies. Use `cargo` for building, running, testing etc.

## Usage

`spcasm [input file] [output file]`

The input is UTF-8 encoded assembly. The output will be an ELF with a single `.data` section containing the SPC700's RAM.

spcasm follows the mnemonic conventions from [this APU manual](https://web.archive.org/web/20060208001231/http://www.alpha-ii.com/snesmusic/files/spc700_apu_manual.txt). The directive and macro syntax is a subset of the [VASM oldstyle syntax](http://sun.hasenbraten.de/vasm/release/vasm_6.html#Oldstyle-Syntax-Module). If you're missing one of the features here, it would definitely be appreciated here!

### Troubleshooting

- *There is no error at the location that spcasm tells me!* While decently good, spcasm still sometimes suffers from incorrect source code indices. Look at the lines above the error and what the error actually says.
- *BRR encoding is slow!* The BRR encoder benefits massively from Rust optimizations. An expected encode speed on modern hardware is only ~700KB/s unoptimized, but up to 50MB/s optimized. Run any BRR-related commands (benchmarks, tests, assembly that invokes the encoder a lot) under an optimized spcasm build. It is usually enough to compile spcasm in release mode by passing `--release` to any cargo command (build, run, test, ...)

## Architectural Overview

With the new parser, spcasm processes assembly code in several steps, which can be thought of as roughly equivalent to "passes" in traditional assemblers.
1. Lexing. A custom lexer with minimal lookahead splits the input text into tokens and already performs some degree of high-level processing, like parsing numbers and test comments (if applicable) as well as distinguishing between macros, mnemonics and labels.
2. Token stream transformation. This step is necessary as the SPC700 assembly language with complex math expressions is not LR(1) (in fact, as far as I can tell it's context-sensitive), so we cannot pass the plain tokens to the parser. Instead, some transformations with significant lookahead need to happen, for example determining what parenthesis are used for (indexing as an addressing mode or math expressions?), combining "+X" and "+Y" and appending a newline to enable the top-level producer grammar. It must be noted that even with this two-step parsing, the new parser is significantly faster than the old (>50%).
3. LR(1) parsing. The parser is an auto-generated [LALRPOP](https://github.com/lalrpop/lalrpop) parser that receives the more context-aware modified token stream. The .lalrpop file additionally contains driver code that builds the AST and performs various early decisions and resolutions, like selecting direct page addressing if possible.
4. Local label resolution. The AST's local labels are not bound to any global labels coming out of the parser. This is fixed in a post-processing step which through deterministic linear traversal can ensure that local labels match up with the correct global labels.
5. Direct page coercion. Wherever possible, addressing modes are coerced to their direct page equivalent, reducing binary size and improving speed.
6. Partially-symbolic assembly. The assembler assembles a mix of high and low-level memory data from instructions and macros. Where possible, concrete memory data is provided, but in other cases, symbolic data like "lowest byte of this label's address" is used instead when necessary. The memory lives in various sections depending on the use of section macros like `org`. It is important to notice that memory addresses are fixed in this step.
7. Final symbol resolution. Because addresses were fixed in the last step, symbolic data can now be resolved to concrete data. This is done in several passes to allow e.g. label references before they are defined. The step both involves obtaining concrete values for symbols based on the fixed memory layout, and emplacing concrete data based on symbols that were newly resolved. For example, a label's value can be resolved to the memory location of the data that is tagged to be the position of the label. Then, references to the label can be replaced by the actual memory address.
8. Section assembly; this step may change in the future. In the last step, each section was concreticised to a plain array of bytes. These sections are now combined into a contiguous memory space with simple copying and zero-padding where memory is left unspecified.

It has not been experimentally verified, but parsing and symbolic assembly are by far the most expensive steps with the most amount of actual code (generated or hand-written) handling them.
