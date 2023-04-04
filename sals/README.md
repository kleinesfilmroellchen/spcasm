# sals

> The SPC700 Assembler Language Server

sals is a language server for SPC700 assembly using the LSP protocol. It comes with a VSCode extension (`vscode`).

## Configuration

The sals VSCode extension comes pre-bundled with the sals server for Windows, Linux, and both architectures of macOS. If this does not include your system, or you need a newer version of sals, you can build a sals executable yourself and point the client to use that instead. The setting `sals.executable.override` is used to enable the override path, and the setting `sals.executable.path` specifies a path to your replacement executable.

## Build, Development and Packaging

The server can be built with `cargo build`; use the `spcasm-release` profile for release builds since the regular `release` build is reserved for the inflexible `wasm-pack`.

To build the VSCode client in the `vscode` subdirectory, first use `yarn install` to install dependencies, then use `yarn esbuild` to build both single-file client script as well as the Rust server. `yarn esbuild-watch` can be used to watch for changes of the client, but it doesn't build the server.

A distributable VSCode package can be created and managed with `vsce` (execute it in the `vscode` subdirectory). Note that before this, you should build server binaries for the four common architectures; namely Windows (MSVC ABI), Linux (GNU ABI), and macOS with both x86 and ARM. The scripts for this are `server-compile-win` (run on Windows) and `server-compile-nowin` (run on any platform except Windows). (It is not known how to either run the Windows linker with proper arguments under WSL as rustc uses wrong paths; or the Clang linker under Windows since it exceeds Windows' command line length limit.)

VSCode launch configurations are provided for testing the server. Use the "Test sals in extension host" configuration to launch an extension host window of VSCode with the sals language server running. This configuration automatically opens the `opcodes.s` source file, so it should activate the extension immediately.

## Attribution

Many parts of this language server are based on the tower-lsp template by IWANABETHATGUY: <https://github.com/IWANABETHATGUY/tower-lsp-boilerplate>.
