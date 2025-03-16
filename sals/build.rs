//! sals build script, adding `shadow_rs` variables.

#[cfg(debug_assertions)]
const BUILD_PATTERN: shadow_rs::BuildPattern = shadow_rs::BuildPattern::Lazy;
#[cfg(not(debug_assertions))]
const BUILD_PATTERN: shadow_rs::BuildPattern = shadow_rs::BuildPattern::RealTime;

fn main() {
	let denied = std::collections::BTreeSet::from([
		shadow_rs::CARGO_MANIFEST_DIR,
		shadow_rs::COMMIT_EMAIL,
		shadow_rs::CARGO_TREE,
		shadow_rs::GIT_STATUS_FILE,
		// FIXME: BRANCH cannot be excluded, since that breaks the "CLAP_VERSION" constant, among others.
		shadow_rs::BUILD_OS,
		shadow_rs::BUILD_TARGET,
		shadow_rs::BUILD_TARGET_ARCH,
		shadow_rs::CARGO_VERSION,
		shadow_rs::CARGO_METADATA,
	]);

	shadow_rs::ShadowBuilder::builder().deny_const(denied).build_pattern(BUILD_PATTERN).build().unwrap();
}
