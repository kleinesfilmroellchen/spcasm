# Development

This section details information useful for anyone working on or tinkering with spcasm.

## Components

spcasm is structured into a few related components:

- The `spcasm` crate contains the assembler's core code. It further contains three targets:
  - `spcasm` is the main assembler binary.
  - The crate can also be built as a library; this is mainly used for tests.
  - `brr` is a BRR command-line utility that intends to replace BRRTools.
- The `spcasm-derive` crate contains derive macros, as Rust requires those to be in a separate crate.
- The `spcasm-web` crate contains WebAssembly APIs and a browser frontend allowing the assembler to run in the browser.
- The `doc` folder contains [mdbook](https://rust-lang.github.io/mdBook/)-based documentation. You can read this documentation directly on GitHub, or build a statically servable website from it with mdbook.

## Toolchain and workflow

spcasm is written in Rust (2021 edition). Due to the use of many (really cool!) unstable features, it can only be compiled with a Rust nightly compiler. The minimum supported Rust version (MSRV) is nightly 1.68 on both Linux (gnu) and Windows (msvc).

Because of `rust-toolchain.toml`, the nightly toolchain should automatically be selected if you run any rustup-based command (like `cargo` or `rustc`).

The standard binary is the assembler `spcasm` itself. There is an additional binary target `brr`, a BRR encoder and decoder with additional interactive functionality. Both require the `binaries` feature.

Tests work through the normal rustc test harness, so they can be run with `cargo test` and `cargo bench`.

The crate has the following features:

- `expensive_tests`: Enable long-running tests. This is enabled by default, but disabled on CI as it wastes time there.
- `binaries`: Enable the two binaries `spcasm` and `brr` and their specific dependencies. If you want to run non-CLI tests or use spcasm as a library, this is not necessary, therefore although it is a default feature, it can be disabled.

If you want to create a release build, please compile with the `spcasm-release` profile; the standard `release` profile needs to be reserved for `wasm-pack` as it is too inflexible at the moment.

There's further ways of fiddling with the build config:

- At substantial build time cost, you can enable "fat" LTO for the spcasm-release profile (in Cargo.toml) for additional ~500KiB of space savings in the binaries. This is only recommended for releases.
- Uncommenting the `rustflags` line in `.cargo/config.toml` enables linking with [Mold](https://github.com/rui314/mold) on Linux (you might have to adjust the path; the committed path is known to work for Arch and Manjaro). This currently only saves 0.5s but hasn't been tested extensively; feel free to experiment.

### Creating a release

###### This documentation is both for transparency purposes and also to make sure that releases consistently contain correct files.

- Make sure that the Rust compiler(s) are up-to-date and all (!) tests pass
- Bump the spcasm version if that didn't happen before
- Switch the `spcasm-release` profile to "fat" LTO temporarily
- Clean any existing files for the release build, if necessary: `cargo clean --profile=spcasm-release --all-features`
- Run a build on spcasm-release on both Windows and Linux: `cargo build --profile=spcasm-release`
- Make sure that the four (!) binaries have correct build times and version numbers
- Copy the four binaries into a temporary release folder named `spcasm-<version>`
- Make sure the documentation is up-to-date, especially check the usage output
- Run `mdbook build` (make sure to have the PDF backend installed!) and copy the generated PDF as "spcasm-manual.pdf" into the release folder
- Copy the `include` folder into the release folder
- Create a .zip archive (Linux binaries removed) and a .tar.gz archive (Windows binaries removed), and name them the same as the folder
- Create a GitHub release and attach the two archives as well as all the four binaries individually

### `spcasm-web`

Working with the WebAssembly bindings and webpage requires a little more effort. You need to have `wasm-pack` and `npm`/node.js. In the `spcasm-web` directory:

- Use `wasm-pack build --features wee_alloc,console_error_panic_hook --no-default-features` to generate the Wasm module and bindings. The two extra features are optional; `wee_alloc` reduces code size and `console_error_panic_hook` helps debugging by printing panics to the console.
- In the `www` directory: Use `npm install` to deal with the necessary evil of several hundred JavaScript modules. (This unfortunately is the best way of making things "just work", plus you get a development server for free.)
- Use `npm run start` for a development server, or `npm run build` to build files for a static web server into the `dist` directory.

## Architectural Overview

With the new parser, spcasm processes assembly code in several steps, which can be thought of as roughly equivalent to "passes" in traditional assemblers. The processing can be split up into three main steps: lexing and parsing (1-3), AST concretization and expansion (4-9), and assembly (10-12).

1. **Lexing.** A custom lexer with minimal lookahead splits the input text into tokens and already performs some degree of high-level processing, like parsing numbers and test comments (if applicable) as well as distinguishing between directives, mnemonics and references.
2. **Token stream transformation.** This step is necessary as the SPC700 assembly language with complex math expressions is not LR(1) (in fact, as far as I can tell it's context-sensitive), so we cannot pass the plain tokens to the parser. Instead, some transformations with significant lookahead need to happen, for example determining what parenthesis are used for (indexing as an addressing mode or math expressions?), combining "+X" and "+Y" and appending a newline to enable the top-level producer grammar. It must be noted that even with this two-step parsing, the new parser is significantly faster than the old (>50%).
3. **LR(1) parsing.** The parser is an auto-generated [LALRPOP](https://github.com/lalrpop/lalrpop) parser that receives the more context-aware modified token stream. The .lalrpop file additionally contains driver code that builds the AST and performs various early decisions and resolutions, like selecting direct page addressing if possible.
4. **Local label resolution.** The AST's local labels are not bound to any global labels coming out of the parser. This is fixed in a post-processing step which through deterministic linear traversal can ensure that local labels match up with the correct global labels. As an implementation detail, the reference-counted label objects are "merged" with one another so that resolving a label later on makes that event easily visible to other referrers of the label.
5. **Direct page coercion.** Wherever possible, addressing modes are coerced to their direct page equivalent, reducing binary size and improving speed.
6. **Assembly source inclusion.** The `include` directives include the assembly source code from other files, and after the AST is finalized, all requested additional files are resolved. The assembler keeps a memory cache of already-parsed files, so that if you have commonly-included files, the cost of including them more than once is reduced. Handling other files goes through the exact same pipeline as the main file, so it repeats steps 1-5 and possibly 6 if there are included files in the included file. Circular includes are not possible for obvious reasons (though non-circular repeated includes are allowed as there are many valid use cases) and will be detected even before the lexer runs. After an included file's AST is finalized, it is copied into the original AST where it replaces the `include` directive.
7. **User-defined macro resolution.** The assembler detects all user-defined macros and resolves them by replacing them with generated AST. This also means that spcasm's macro functionality is far more sophisticated than simple copy-paste, it can rather be thought of as a function inliner in a traditional compiler.
8. **Segmentization.** The assembler splits the program into its physical segments by linearly traversing the fully expanded AST and handling segment control macros, such as `org` or `pushpc`. The result is a collection of segments with known starting addresses and the contained program elements, still in AST form.
9. **Direct page references optimization.** Given the new knowledge of segments, the assembler can infer memory addresses for most kinds of program elements. Therefore, the assembler is able to deduce which references will live in the direct page. This is important as many instructions are smaller and faster when using direct page addressing, so they should be used whenever possible. The assembler uses an iterative optimization process that keeps as many labels as possible in the direct page. For a detailed explanation, see the well-commented `spcasm::parser::AssemblyFile::optimize_direct_page_labels` function. The optimization problem solved here has been compared to linker relaxation for jump offsets and thunks; however while optimal linker relaxation is most likely NP-hard, spcasm (minus some bugs) solves the direct page reference optimization problem, which is in P, optimally.
10. **Partially-symbolic assembly.** The assembler assembles a mix of high and low-level memory data from instructions and directives. Where possible, concrete memory data is provided, but in other cases, symbolic data like "lowest byte of this reference's address" is used instead when necessary. It is important to notice that memory addresses are fixed after this step.
11. **Final symbol resolution.** Because addresses were fixed in the last step, symbolic data can now be resolved to concrete data. This is done in several passes to allow e.g. mention of references before they are defined. The step both involves obtaining concrete values for symbols based on the fixed memory layout, and emplacing concrete data based on symbols that were newly resolved. For example, a reference's value can be resolved to the memory location of the data that is tagged to be the position of the reference. Then, references to the reference can be replaced by the actual memory address.
12. **Section assembly**; this step may change in the future. In the last step, each section was concreticised to a plain array of bytes. These sections are now combined into a contiguous memory space with simple copying and zero-padding where memory is left unspecified.

It has not been experimentally verified, but parsing and symbolic assembly are by far the most expensive steps with the most amount of actual code (generated or hand-written) handling them.
