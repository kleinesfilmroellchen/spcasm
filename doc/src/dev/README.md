# Development

This section details information useful for anyone working on or tinkering with spcasm.

## Toolchain and workflow

spcasm is written in Rust (2021 edition). Due to the use of many (really cool!) unstable features, it can only be compiled with a Rust nightly compiler. The latest tested compiler version is nightly 1.65 on both Linux (gnu) and Windows (msvc).

Because of `rust-toolchain.toml`, the nightly toolchain should automatically be selected if you run any rustup-based command (like `cargo` or `rustc`).

The standard binary is the assembler `spcasm` itself. There is an additional binary target `brri`, an interactive BRR functionality explorer. This requires the `clap` feature. 

Tests work through the normal rustc test harness, so they can be run with `cargo test` and `cargo bench`.

The crate has the following features:
- `test_bootrom`: Enable the S-SMP boot ROM assembler test. This is currently broken due to the test system itself being incapable of dealing with sections.
- `expensive_tests`: Enable long-running tests. This is enabled by default, but disabled on CI as it wastes time there.
- `clap`: Enable the `clap` crate and the `brri` binary which depends on it. Clap is an extremely heavy dependency that the main binary doesn't need, so it is disabled by default.

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
