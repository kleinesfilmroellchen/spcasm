//! Source code infrastructure for error reporting.

use std::path::{Path, PathBuf};
use std::sync::Arc;

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};
use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};

use crate::AssemblyError;

/// The source code for an assembly error.
#[derive(Debug, Clone, Default)]
pub struct AssemblyCode {
	/// The text content of the assembly code.
	pub text:         SharedStr,
	/// The source code location must be canonicalized!
	pub name:         PathBuf,
	/// The include path of the file.
	pub include_path: Vec<PathBuf>,
}

impl AssemblyCode {
	/// Create a new source code struct by loading a file's contents.
	///
	/// # Errors
	/// If reading the file fails (doesn't exist, permissions wrong, I/O error etc.)
	pub fn from_file(filename: &str) -> Result<Arc<Self>, std::io::Error> {
		let mut path = PathBuf::from(filename);
		if path.is_relative() {
			path = std::env::current_dir()?.join(path);
		}
		path = uniform_canonicalize(&path)?;

		// Optimization: Pre-allocate theSharedStr with the known size of the source code.
		let text = std::fs::read_to_string(&path)?;
		let text_size = text.len();
		let mut contents = String::new();
		<String as Extend<char>>::extend_reserve(&mut contents, text_size);
		contents.extend(text.chars().filter(|c| c != &'\r'));

		Ok(Arc::new(Self { name: path, text: contents.into(), include_path: Vec::new() }))
	}

	/// Create a new source code struct by loading a file's contents, and immediately create an assembler error if that
	/// fails.
	///
	/// # Errors
	/// If reading the file fails (doesn't exist, permissions wrong, I/O error etc.)
	pub fn from_file_or_assembly_error(file_name: &str) -> Result<Arc<Self>, Box<AssemblyError>> {
		Self::from_file(file_name).map_err(|os_error| {
			AssemblyError::FileNotFound {
				os_error:  Arc::new(os_error),
				file_name: file_name.to_string().into(),
				src:       std::sync::Arc::new(Self {
					name: std::path::PathBuf::from("<<arguments>>"),
					text: file_name.to_string().into(),
					..Default::default()
				}),
				location:  (0, file_name.len()).into(),
			}
			.into()
		})
	}

	/// Create a new source code struct from source code text and a (possibly fake) name.
	#[must_use]
	#[allow(clippy::missing_const_for_fn)]
	pub fn new(text: &str, name: &str) -> Self {
		Self {
			text:         text.chars().filter(|c| c != &'\r').collect(),
			name:         PathBuf::from(name.to_owned()),
			include_path: Vec::new(),
		}
	}

	/// Create a new source code struct from source code text and a file system path.
	#[must_use]
	#[allow(clippy::missing_const_for_fn)]
	pub fn new_from_path(text: &str, name: &Path) -> Self {
		Self {
			text:         text.chars().filter(|c| c != &'\r').collect(),
			name:         name.to_owned(),
			include_path: Vec::new(),
		}
	}

	/// Returns a pretty-printed variant of the file name of this source code.
	///
	/// The pretty-printing rules are as follows:
	/// - If the file is relative to the working directory, print a relative file name without leading `./`.
	/// - If the file is not relative, i.e. its canonical path does not contain the working directory, print an absolute
	///   file name. On Windows, extended path length syntax (`\\?\`) is omitted.
	///
	/// # Panics
	/// Programming bugs.
	#[must_use]
	pub fn file_name(&self) -> SharedStr {
		Self::file_name_for(&self.name)
	}

	/// Returns a pretty-printed variant of the given path.
	///
	/// The pretty-printing rules are as follows:
	/// - If the file is relative to the working directory, print a relative file name without leading `./`.
	/// - If the file is not relative, i.e. its canonical path does not contain the working directory, print an absolute
	///   file name. On Windows, extended path length syntax (`\\?\`) is omitted.
	///
	/// # Panics
	/// Programming bugs.
	#[must_use]
	pub fn file_name_for(path: &Path) -> SharedStr {
		let cwd = uniform_canonicalize(&PathBuf::from(".")).unwrap();
		if path.starts_with(&cwd) {
			path.strip_prefix(cwd).unwrap().to_string_lossy().to_string().into()
		} else {
			path.as_os_str().to_string_lossy().to_string().into()
		}
	}
}

/// Implements a more uniform canonicalization. The main difference to ``std::fs::canonicalize`` is that it doesn't
/// create the extended length path syntax on Windows. This is for better compatibility with file link-supporting
/// terminals and the `trycmd` integration tests.
#[cfg(windows)]
#[inline]
pub fn uniform_canonicalize(path: &Path) -> std::io::Result<PathBuf> {
	// All extended length paths start with '\\?\' (length 4), see https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#maximum-path-length-limitation
	Ok(PathBuf::from(path.canonicalize()?.into_os_string().to_string_lossy()[4 ..].to_owned()))
}

/// Implements a more uniform canonicalization. The main difference to ``std::fs::canonicalize`` is that it doesn't
/// create the extended length syntax on Windows. This is for better compatibility with file link-supporting terminals
/// and the `trycmd` integration tests.
#[cfg(not(any(windows, target_family = "wasm")))]
#[inline]
pub fn uniform_canonicalize(path: &Path) -> std::io::Result<PathBuf> {
	path.canonicalize()
}

#[cfg(target_family = "wasm")]
#[inline]
pub fn uniform_canonicalize(path: &Path) -> std::io::Result<PathBuf> {
	Ok(path.to_owned())
}

impl SourceCode for AssemblyCode {
	fn read_span<'a>(
		&'a self,
		span: &SourceSpan,
		context_lines_before: usize,
		context_lines_after: usize,
	) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
		let result = self.text.read_span(span, context_lines_before, context_lines_after)?;
		let retval = Box::new(MietteSpanContents::new_named(
			self.file_name().as_str().to_owned(),
			result.data(),
			*result.span(),
			result.line(),
			result.column(),
			result.line_count(),
		));
		Ok(retval)
	}
}
