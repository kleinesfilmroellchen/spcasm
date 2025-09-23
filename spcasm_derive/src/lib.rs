//! Derive proc macros for spcasm.

#[allow(unused)]
use flexstr::{FlexStr, IntoSharedStr, ToSharedStr, shared_str};

#[allow(unused_extern_crates)]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{ToTokens, quote};

/// # Panics
///
/// The derive macro can only be used for enums, and only for enums containing unit variants.
#[proc_macro_derive(Parse)]
pub fn parse_macro_derive(input: TokenStream) -> TokenStream {
	let type_ = syn::parse::<syn::DeriveInput>(input).unwrap();

	match type_.data {
		syn::Data::Enum(enum_) => {
			let name = type_.ident;
			let name_string = name.to_string().to_lowercase();
			let (variant_identifiers, variant_strings): (Vec<_>, Vec<_>) = enum_
				.variants
				.iter()
				.map(|variant| match variant.fields {
					syn::Fields::Unit =>
						(variant.ident.clone(), variant.ident.to_string().to_lowercase().replace('_', " ")),
					_ => panic!(
						"Parse cannot be derived for enums containing non-unit variants; variant {} is not a unit.",
						variant.to_token_stream()
					),
				})
				.unzip();

			quote! {
				#[automatically_derived]
				#[allow(missing_docs)]
				impl crate::parser::Parse for #name {
					fn parse(value: &str, location: miette::SourceSpan, src: std::sync::Arc<crate::AssemblyCode>) -> Result<Self, Box<crate::error::AssemblyError>> {
						Ok(match value {
							#( #variant_strings => Self::#variant_identifiers, )*
							_ => return Err(crate::error::AssemblyError::InvalidConstant { constant: value.into(), typename: #name_string.into(), location, src }.into()),
						})
					}

					fn is_valid(value: &str) -> bool {
						[
							#( #variant_strings, )*
						]
						.contains(&value.to_lowercase().as_str())
					}
				}
			}
			.into()
		},
		_ => panic!("Parse cannot be derived for non-enum types."),
	}
}
