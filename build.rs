extern crate lalrpop;

fn main() {
	lalrpop::process_root().unwrap();

	#[cfg(feature = "binaries")]
	{
		use std::collections::BTreeSet;
		// hide sensitive information
		let mut denied = BTreeSet::new();
		denied.insert(shadow_rs::CARGO_MANIFEST_DIR);
		denied.insert(shadow_rs::COMMIT_EMAIL);
		denied.insert(shadow_rs::CARGO_TREE);
		denied.insert(shadow_rs::GIT_STATUS_FILE);

		shadow_rs::new_deny(denied).unwrap();
	}
}
