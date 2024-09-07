//! sals build script, adding `shadow_rs` variables.

fn main() {
	use std::collections::BTreeSet;
	let mut denied = BTreeSet::new();
	denied.insert(shadow_rs::CARGO_MANIFEST_DIR);
	denied.insert(shadow_rs::COMMIT_EMAIL);
	denied.insert(shadow_rs::CARGO_TREE);
	denied.insert(shadow_rs::GIT_STATUS_FILE);
	// FIXME: BRANCH cannot be excluded, since that breaks the "CLAP_VERSION" constant, among others.
	denied.insert(shadow_rs::BUILD_OS);
	denied.insert(shadow_rs::BUILD_TARGET);
	denied.insert(shadow_rs::BUILD_TARGET_ARCH);
	denied.insert(shadow_rs::CARGO_VERSION);
	shadow_rs::new_deny(denied).unwrap();
}
