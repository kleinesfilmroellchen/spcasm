# Development

This section details information useful for anyone working on or tinkering with spcasm.

- auto-gen TOC;
  {:toc}

## Components

Over time, spcasm has evolved from "just an SPC700 assembler" to a whole toolchain for the SPC700 platform. It now consists of several components, mainly a few Rust crates, documentation and build/test tooling.

- The `spcasm` crate contains the assembler's core code. It further contains three targets:
  - The crate is a library in itself, which is used by most other non-core tools. Some design decisions within the crate are based on other tools' requirements. For example: concurrency-safety is not needed by the single-threaded assembler, but by the language server.
  - `spcasm` is the main assembler binary.
  - `brr` is BRR command-line tooling that exposes a thin interface to the BRR support.
- The `spcasm_derive` crate contains derive macros, as Rust requires those to be in a separate crate.
- The `spcasm-web` crate contains WebAssembly APIs and a browser frontend allowing the assembler to run in the browser.
- The `sals` crate contains the **s**pc**a**sm **l**anguage **s**erver, an experimental language server for SPC-700 assembly.
- The `sapemu` crate contains the (WIP) cycle-accurate S-APU emulator.
- The `doc` folder contains [mdbook](https://rust-lang.github.io/mdBook/)-based documentation. You can read this documentation directly on GitHub, or build a statically servable website from it with mdbook.

spcasm itself contains by far the most code, and [its entire documentation is available online](https://kleinesfilmroellchen.github.io/spcasm/doc/api/spcasm/index.html).

## Toolchain and workflow

> `$ just spcasm`

spcasm is written in Rust (2021 edition). Due to the use of many (really cool!) unstable features, it can only be compiled with a Rust nightly compiler. The minimum supported Rust version (MSRV) can be found in Cargo.toml.

Because of `rust-toolchain.toml`, the nightly toolchain should automatically be selected if you run any Rustup-based command (like `cargo` or `rustc`).

The standard binary is the assembler `spcasm` itself. There is an additional binary target `brr`, a BRR encoder and decoder with additional interactive functionality. Both require the `binaries` feature. The other crates have their own (singular) targets.

> `$ just test`

Tests work through the normal rustc test harness, so they can be run with `cargo test` and `cargo bench`.

The crate has the following features:

- `binaries`: Enable the two binaries `spcasm` and `brr` and their specific dependencies. If you want to run non-CLI tests or use spcasm as a library, this is not necessary, therefore although it is a default feature, it can be disabled.

If you want to create an optimized build, please compile with the `spcasm-fastrelease` profile; the standard `release` profile needs to be reserved for `wasm-pack` as it is too inflexible at the moment.

There's further ways of fiddling with the build config:

- At substantial build time cost, you can enable "fat" LTO via the `spcasm-release` profile for additional ~500KiB of space savings in the binaries. This is only recommended for releases, not normal optimized builds, as it doesn't improve performance measurably!
- Uncommenting the `rustflags` line in `.cargo/config.toml` enables linking with [Mold](https://github.com/rui314/mold) on Linux (you might have to adjust the path; the committed path is known to work for Arch and Manjaro). This currently only saves 0.5s but hasn't been tested extensively; feel free to experiment.

### Creating a release

The release process requires `just` and a Windows machine with WSL, and it can take many minutes to run the multitude of compilations and checks.

```shell
# Ensure up-to-date toolchains.
$ rustup update && wsl rustup update

$ just clean
$ just release $version
```

Towards the end, the process will run the released binaries to show their version information. Check that this version information is up-to-date, and if not, update the version information in the main Cargo.toml and run the release again.

Finally, create a GitHub release and attach the two archives and four binaries from the `spcasm-$version` folder that was created. Make sure that the main branch from which the release build was run is up-to-date in the repository before releasing.

### Experimental profile-guided optimization builds

> `$ just pgo-release`

For this to work, you need to have llvm-profdata from the llvm-tools-preview Rustup component on your PATH. Remember to delete the target/pgo folder after every run so that stale profiling data doesn't stick around. As with any high-optimization build, this one takes a while.

### Collect coverage data

> `$ just coverage`

For LLVM-based test coverage via `cargo llvm-cov`, care needs to be taken that build and coverage artifacts are not cleaned after building the binaries. If that were to be done, trycmd wouldn't execute the end-to-end binary tests and quite a bit of their coverage would be missing. Coverage data is only collected for the core library, since everything else is just glue and interface code that's not of interest.

Currently, coverage data is not collected on CI.

### `spcasm-web`

> `$ just web`

Working with the WebAssembly bindings and webpage is quite straightforward, but you need to have `trunk` installed (`cargo install trunk`). Then, run `trunk build` in the `spcasm-web` folder. For a development server with auto-reload, use `trunk serve`; a browser window will open automatically.

## Run HUDPAGS Analysis

[hyper-unspecific-data-parsing-and-graphing-scripts](https://github.com/linusg/hyper-unspecific-data-parsing-and-graphing-scripts) (HUDPAGS) is a tool for collecting interesting statistics on a repository over time, such as lines of code, occurrences of certain words in source code and commit messages, etc. spcasm has a HUDPAGS configuration (hudpags.toml) that runs several interesting analyses. Results can then be extracted from the hudpags-analysis.json file, for example via the provided scripts, or via your own tooling. I recommend using the matplotlib-based tools to look at some pretty graphs.
