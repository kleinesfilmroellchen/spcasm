
# PowerShell Core has all the Unix shell skills we need, such as || and &&
set windows-shell := ["pwsh.exe", "-NoLogo", "-Command"]

# Build all Rust binaries and run all tests
test: build
	cargo nextest run || cargo test

# Run checks and formatting
check:
	-cargo clippy --workspace
	-cargo fmt --all

# Run spcasm
spcasm *ARGS: build-spcasm
	cargo run -- {{ARGS}}

# Build everything
build:
	cargo build --workspace
# Build spcasm
build-spcasm:
	cargo build --bin spcasm

web:
	cd spcasm-web && trunk serve --features wee_alloc,console_error_panic_hook

# Build all documentation
doc:
	cargo doc --no-deps --bin spcasm
	cd doc && mdbook build

# Build and serve documentation locally
doc-watch:
	cd doc && mdbook serve

# Assemble the GitHub Pages site
github-pages:
	cd spcasm-web && trunk build --dist ../site --release --features wee_alloc --no-default-features --public-url spcasm
	chmod +x doc/copy-readme-for-gh-pages.sh && doc/copy-readme-for-gh-pages.sh
	just doc
	cp -rT doc/book/html site/doc
	cp -rT target/doc site/doc/api
	