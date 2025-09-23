//! The Register struct.
// this is a bug, see <https://github.com/rust-lang/rust-clippy/issues/6902>
#![allow(clippy::use_self)]

use std::fmt::Display;
use std::marker::ConstParamTy;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use spcasm_derive::Parse;

use crate::VariantName;

/// Registers.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Parse, VariantName, ConstParamTy)]
pub enum Register {
	/// Accumulator.
	A,
	/// First index register.
	X,
	/// Second index register.
	Y,
	/// Stack pointer.
	SP,
	/// Program status word (flag register).
	PSW,
	/// Alternate form of PSW.
	P,
	/// Combined 16-bit register from Y and A.
	YA,
	/// Carry flag, a pseudo-register.
	C,
}

impl Register {
	/// Coerces alternative register names into their canonical form that the assembler uses internally.
	#[must_use]
	pub const fn coerce_alternate_registers(self) -> Self {
		match self {
			Self::P => Self::PSW,
			_ => self,
		}
	}
}

impl Display for Register {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", self.variant_name())
	}
}
