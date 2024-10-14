/// Trace message that is compiled out on release builds.
#[macro_export]
macro_rules! trace {
	($($arg:tt)+) => {
		#[cfg(debug_assertions)]
		::log::trace!($($arg)+);
	};
}
