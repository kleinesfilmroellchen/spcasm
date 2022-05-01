//! The Register struct.
// this is a bug, see <https://github.com/rust-lang/rust-clippy/issues/6902>
#![allow(clippy::use_self)]

use std::fmt::Display;
use std::sync::Arc;

use miette::SourceSpan;
use serde::Serialize;
use serde_variant::to_variant_name;
use spcasm_derive::Parse;

use crate::error::{AssemblyCode, AssemblyError};

/// Registers.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Parse, Serialize)]
pub enum Register {
	/// Accumulator.
	A,
	/// First index register.
	X,
	/// Second index register.
	Y,
	/// Stack pointer.
	SP,
	/// Program status word (instruction pointer).
	PSW,
	/// Combined 16-bit register from Y and A.
	YA,
	/// Carry flag, a pseudo-register.
	C,
}

impl Display for Register {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", to_variant_name(self).unwrap())
	}
}
