
# PowerShell Core has all the Unix shell skills we need, such as || and &&
set windows-shell := ["pwsh.exe", "-NoLogo", "-Command"]

# Build all Rust binaries and run all tests
test: build
	cargo nextest run --workspace

coverage:
	# The first two commands are expected to fail, since "run" is just a hack to get llvm-cov to compile the binaries.
	-cargo llvm-cov --all-features --hide-instantiations --bin=brr run -- x
	-cargo llvm-cov --all-features --hide-instantiations --no-clean run -- x
	cargo llvm-cov --open --all-features --hide-instantiations --no-clean nextest --workspace

# Run checks and formatting
check:
	-cargo clippy --workspace --tests
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
	cargo build --workspace --profile=spcasm-fastrelease

pgo-release:
	cargo pgo build
	cargo pgo test
	cargo pgo bench --keep-profiles
	cargo pgo optimize

web:
	cd spcasm-web && trunk serve --features console_error_panic_hook

# Build all documentation
doc:
	cargo doc --no-deps --bin spcasm
	cd doc && mdbook build

# Build and serve documentation locally
doc-watch:
	cd doc && mdbook serve

# Assemble the website
website url="/":
	cd spcasm-web && trunk build --dist ../site --release --no-default-features --public-url {{url}}
	just doc
	cp -rT doc/book/html site/doc
	cp -rT target/doc site/doc/api

# Create a new spcasm release for the specified target.
release version target: (release-build-common target) && (release-finalize version target)

# Common build steps for all targets.
release-build-common target:
	# ignore test failures, these are expected for cross-compiled musl
	-cargo nextest run --cargo-profile=spcasm-release --target {{target}}
	cargo build --workspace -q --profile=spcasm-release --features human-panic --target {{target}}

# Specific finalization steps for Windows
[windows]
release-finalize version target:
	New-Item -Force -ItemType directory 'spcasm-{{version}}'
	Copy-Item target/{{target}}/spcasm-release/spcasm.exe -Destination 'spcasm-{{version}}'
	Copy-Item target/{{target}}/spcasm-release/brr.exe -Destination 'spcasm-{{version}}'
	@echo '======================================================================='
	@echo 'spcasm and brr version(s)'
	./spcasm-{{version}}/spcasm.exe --version
	./spcasm-{{version}}/brr.exe --version
	Copy-Item -Recurse include -Destination 'spcasm-{{version}}'
	cd spcasm-{{version}} && 7z a -tzip ../spcasm-{{version}}-{{target}}.zip '*.exe' 'include/*'

# Specific finalization steps for Unix
[unix]
release-finalize version target:
	mkdir 'spcasm-{{version}}'
	cp target/{{target}}/spcasm-release/spcasm 'spcasm-{{version}}'
	cp target/{{target}}/spcasm-release/brr 'spcasm-{{version}}'
	@echo '======================================================================='
	@echo 'spcasm and brr version(s)'
	'./spcasm-{{version}}/spcasm' --version
	'./spcasm-{{version}}/brr' --version
	cp -r include 'spcasm-{{version}}'
	tar caz -f spcasm-{{version}}-{{target}}.tar.xz -C spcasm-{{version}} .


clean:
	cargo clean -r
