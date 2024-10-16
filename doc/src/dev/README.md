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
- The `spcfile` crate contains a generic .spc file format parser and writer. This is a common library of `spcasm` (for .spc writing) and `sapemu` (for .spc playback).
- The `doc` folder contains [mdbook](https://rust-lang.github.io/mdBook/)-based documentation. You can read this documentation directly on GitHub, or build a statically servable website from it with mdbook.

spcasm itself contains by far the most code, and [its entire documentation is available online](../api/spcasm/index.html).

## Toolchain and workflow

> `$ just spcasm`

spcasm is written in Rust (2021 edition). Due to the use of many (really cool!) unstable features, it can only be compiled with a Rust nightly compiler.

Because of `rust-toolchain.toml`, the nightly toolchain should automatically be selected if you run any Rustup-based command (like `cargo` or `rustc`). This file also pins a known working release of the nightly compiler, which is updated occasionally. (I try to update to the newest nightly after most stable releases.)

The standard binary is the assembler `spcasm` itself. There is an additional binary target `brr`, a BRR encoder and decoder with additional interactive functionality. Both require the `binaries` feature. The other crates have their own (singular) targets.

> `$ just test`

Tests work through the normal rustc test harness, so they can be run with `cargo test` and `cargo bench`.

The crate has the following features:

- `binaries`: Enable the two binaries `spcasm` and `brr` and their specific dependencies. If you want to run non-CLI tests or use spcasm as a library, this is not necessary, therefore although it is a default feature, it can be disabled.

If you want to create an optimized build, please compile with the `spcasm-fastrelease` profile; the standard `release` profile needs to be reserved for spcasm-web as it is too inflexible at the moment.

There's further ways of fiddling with the build config:

- At substantial build time cost, you can enable "fat" LTO via the `spcasm-release` profile for additional ~500KiB of space savings in the binaries. This is only recommended for releases, not normal optimized builds, as it doesn't improve performance measurably!
- Uncommenting the `rustflags` line in `.cargo/config.toml` enables linking with [Mold](https://github.com/rui314/mold) on Linux (you might have to adjust the path; the committed path is known to work for Arch and Manjaro). This currently only saves 0.5s but hasn't been tested extensively; feel free to experiment.

### Creating a release

The release process requires `just`, and it can take many minutes to run the multitude of compilations and checks.

```shell
# Ensure up-to-date toolchain.
$ rustup update

$ just clean
$ just release $version
```

Towards the end, the process will run the released binaries to show their version information. Check that this version information is up-to-date, and if not, update the version information in the main Cargo.toml and run the release again.

Release binaries are automatically compiled and uploaded when a GitHub release is created.

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

### `sapemu`

_Important note: sapemu is *very* work-in-progress. The current executable serves the sole purpose of verifying emulator behavior and is not at all practical or useful to end users. The plan is to eventually add a GUI frontend to sapemu, as well as audio playback and WAV file output. Only concern yourself with sapemu at the present stage if you want to help with completing it._

sapemu contains a work-in-progress cycle-accurate emulator of the entire SNES sound system, technically referred to as the S-APU (therefore the portmanteau name). The emulator binary has a WIP command-line interface (changes may happen at any time, and it is likely that a major revamp will happen before sapemu's first inclusion in releases) that provides help output with `--help`, so `cd sapemu && cargo run -- --help`. The binary can print a lot of logging information to the console if you use high verbosity levels, so beware.

sapemu's functionality is currently mostly verified through tests. We use the test files provided by [SingleStepTests](https://github.com/SingleStepTests/spc700/tree/main/v1), which contain before and after CPU+RAM state as well as per-cycle bus activity that can be checked against. Simply running the crate tests will do everything necessary to perform the near-exhaustive tests (you need an internet connection, since it downloads the tests before every run). The command line `cargo nextest run --no-fail-fast --failure-output never` is very useful for getting an overview over total test state, and `cargo nextest run -E 'test(/.*?xAB$/)'` can be used to run a single opcode's test (replace `AB` with the hex digit of your opcode, e.g. `f`).

## Run HUDPAGS Analysis

[hyper-unspecific-data-parsing-and-graphing-scripts](https://github.com/linusg/hyper-unspecific-data-parsing-and-graphing-scripts) (HUDPAGS) is a tool for collecting interesting statistics on a repository over time, such as lines of code, occurrences of certain words in source code and commit messages, etc. spcasm has a HUDPAGS configuration (hudpags.toml) that runs several interesting analyses. Results can then be extracted from the hudpags-analysis.json file, for example via the provided scripts, or via your own tooling. I recommend using the matplotlib-based tools to look at some pretty graphs.

## Using spcasm as a library

Since spcasm provides a library crate, it can be reused in other crates. This is primarily important for the other crates in the spcasm project, like `spcasm-web` or `sals`. However, since you can use Rust crates from any GitHub repository (see the Cargo documentation), it is fairly easy to use spcasm as a library in an external project.

spcasm’s APIs are fairly stable, but since it has not been designed carefully for usability in other projects, there may be internals exposed that can change in violation of semver guarantees. High-level APIs, such as the ones that return an assembled ROM from given assembler source code, are expected to stay relatively stable. The semver versioning strongly applies to stability guarantees in the end-user applications, such as a minor version bump when new options and assembler directives are introduced, and a major version bump when incompatible changes to assembler syntax are introduced. If you plan to use spcasm as a library, please pin the dependency to a specific commit or version, and prepare to rework a bunch of your spcasm API usage when you do upgrade.

PRs and issues changing the public API may be accepted if the maintenance effort is not significant. In general, exposing more internal APIs is not a problem, but the lower-level APIs are even more in flux than higher-level ones.

spcfile provides strong semver guarantees, as it is designed specifically as a reusable library.
