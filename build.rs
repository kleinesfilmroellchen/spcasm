//! spcasm build script. This script's responsibility is to run the LALRPOP parser generator, and generate `shadow_rs`
//! based build information for binaries.

#[allow(unused_extern_crates)]
extern crate lalrpop;

fn main() {
	// Make Cargo rebuild when the parser generator input file changes.
	println!("cargo:rerun-if-changed=src/parser/asm.lalrpop");
	lalrpop::Configuration::new().use_cargo_dir_conventions().process_file("src/parser/asm.lalrpop").unwrap();

	#[cfg(feature = "binaries")]
	{
		// hide sensitive information
		let denied = std::collections::BTreeSet::from([
			shadow_rs::CARGO_MANIFEST_DIR,
			shadow_rs::COMMIT_EMAIL,
			shadow_rs::CARGO_TREE,
			shadow_rs::CARGO_METADATA,
			shadow_rs::GIT_STATUS_FILE,
		]);

		#[cfg(debug_assertions)]
		const BUILD_PATTERN: shadow_rs::BuildPattern = shadow_rs::BuildPattern::Lazy;
		#[cfg(not(debug_assertions))]
		const BUILD_PATTERN: shadow_rs::BuildPattern = shadow_rs::BuildPattern::RealTime;

		shadow_rs::ShadowBuilder::builder().deny_const(denied).build_pattern(BUILD_PATTERN).build().unwrap();
	}
}
