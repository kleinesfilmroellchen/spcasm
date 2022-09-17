//! HACK: There's two things we need to do here:
//! 1. Circumvent Rust's orphaning rules by writing our own Default trait we can implement for third-party types.
//! 2. Prevent "implementation collisions" that occur because we want all actually Default-implementing types to also
//! implement the custom Default. There are no collisions at the moment; Rust is just worried there might be in the
//! future. So we just tell Rust what types we want to copy the Default implementation for and add to that list if
//! necessary.

use std::num::ParseIntError;
use std::sync::Arc;

use crate::mcro::MacroSymbol;
use crate::parser::instruction::Mnemonic;
use crate::parser::Token;
use crate::AssemblyCode;

/// This is a default-like trait whose values are wholly ignored. It's just necessary for a discriminator-related hack:
/// ``std::mem::discriminator`` has to obtain an enum instance, because enum variants are not types. In order to
/// instantiate all the variants of e.g. ``crate::error::AssemblyError``, we just have to provide values to their
/// fields, it does not matter at all what those values are. If possible we're just gonna grab ``Default``'s data, but
/// not all field types implement that.
pub trait FakeDefaultForIgnoredValues {
	fn default() -> Self;
}
trait InheritFromDefault: Default {}
impl<T> FakeDefaultForIgnoredValues for T
where
	T: InheritFromDefault,
{
	fn default() -> Self {
		<Self as Default>::default()
	}
}

impl InheritFromDefault for String {}
impl InheritFromDefault for AssemblyCode {}
impl InheritFromDefault for bool {}
impl InheritFromDefault for u8 {}
impl InheritFromDefault for char {}
impl InheritFromDefault for usize {}
impl InheritFromDefault for i64 {}
impl<T> InheritFromDefault for Arc<T> where T: InheritFromDefault {}
impl<T> InheritFromDefault for Option<T> {}
impl<T> InheritFromDefault for Vec<T> {}

impl FakeDefaultForIgnoredValues for miette::SourceSpan {
	fn default() -> Self {
		(0, 0).into()
	}
}

impl FakeDefaultForIgnoredValues for std::io::Error {
	fn default() -> Self {
		std::io::ErrorKind::Other.into()
	}
}

impl FakeDefaultForIgnoredValues for ParseIntError {
	fn default() -> Self {
		"haxor".parse::<i64>().unwrap_err()
	}
}

impl FakeDefaultForIgnoredValues for Token {
	fn default() -> Self {
		Self::Newline(0.into())
	}
}

impl FakeDefaultForIgnoredValues for MacroSymbol {
	fn default() -> Self {
		Self::Byte
	}
}

impl FakeDefaultForIgnoredValues for Mnemonic {
	fn default() -> Self {
		Self::Adc
	}
}
