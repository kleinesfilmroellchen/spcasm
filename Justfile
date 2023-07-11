
# PowerShell Core has all the Unix shell skills we need, such as || and &&
set windows-shell := ["pwsh.exe", "-NoLogo", "-Command"]

# Build all Rust binaries and run all tests
test: build
	cargo nextest run || cargo test

coverage:
	# The first two commands are expected to fail, since "run" is just a hack to get llvm-cov to compile the binaries.
	-cargo llvm-cov --all-features --hide-instantiations --bin=brr run -- x
	-cargo llvm-cov --all-features --hide-instantiations --no-clean run -- x
	cargo llvm-cov --open --all-features --hide-instantiations --no-clean nextest

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
fastrelease:
	cargo build --profile=spcasm-fastrelease

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

# Create a new spcasm release from a Windows machine
[windows]
release version: release-build-common && (release-finalize-common version)
	wsl just release-build-common

# Create a new spcasm release from a Unix machine
[unix]
release version: release-build-common && (release-finalize-common version)

release-build-common: test check
	cargo build -q --profile=spcasm-release

release-finalize-common version:
	mkdir 'spcasm-{{version}}'
	cp target/spcasm-release/spcasm.exe 'spcasm-{{version}}'
	cp target/spcasm-release/spcasm 'spcasm-{{version}}'
	cp target/spcasm-release/brr.exe 'spcasm-{{version}}'
	cp target/spcasm-release/brr 'spcasm-{{version}}'
	@echo '======================================================================='
	@echo 'spcasm and brr version(s)'
	wsl './spcasm-{{version}}/spcasm' --version
	./spcasm-{{version}}/spcasm.exe --version
	wsl './spcasm-{{version}}/brr' --version
	./spcasm-{{version}}/brr.exe --version
	cd doc && mdbook build
	cp doc/book/pdf/output.pdf 'spcasm-{{version}}/spcasm-manual.pdf'
	wsl -e cp -rT include 'spcasm-{{version}}/include'
	cd spcasm-{{version}} && zip spcasm-{{version}}.zip '*' 'include/*' -x spcasm brr
	wsl -e tar -caz --exclude '*.exe' --exclude '*.zip' -f spcasm-{{version}}.tar.xz -C spcasm-{{version}} .
	mv spcasm-{{version}}.tar.xz spcasm-{{version}}

clean:
	cargo clean -r
