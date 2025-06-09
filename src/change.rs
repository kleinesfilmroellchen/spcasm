//! Enum specifying whether a change occurred or not.

#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
#[repr(u8)]
pub enum Change {
	#[default]
	Unmodified,
	Modified,
}

impl std::ops::BitOrAssign for Change {
	fn bitor_assign(&mut self, rhs: Self) {
		*self = *self | rhs;
	}
}

impl std::ops::BitOr for Change {
	type Output = Self;

	fn bitor(self, rhs: Self) -> Self::Output {
		if self == Self::Modified || rhs == Self::Modified { Self::Modified } else { Self::Unmodified }
	}
}
