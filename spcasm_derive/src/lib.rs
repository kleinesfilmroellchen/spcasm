//! Derive proc macros for spcasm.

#[allow(unused)]
use flexstr::{FlexStr, IntoSharedStr, ToSharedStr, shared_str};

#[allow(unused_extern_crates)]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{ToTokens, quote};
use syn::{Data, DeriveInput, Fields, parse_macro_input};

/// # Panics
///
/// The derive macro can only be used for enums, and only for enums containing unit variants.
#[proc_macro_derive(Parse)]
pub fn parse_macro_derive(input: TokenStream) -> TokenStream {
	let type_ = syn::parse::<syn::DeriveInput>(input).unwrap();

	match type_.data {
		syn::Data::Enum(enum_) => {
			let name = type_.ident;
			let name_string = format!("{name}").to_lowercase();
			let variant_identifiers_and_strings = enum_
				.variants
				.iter()
				.map(|variant| match variant.fields {
					syn::Fields::Unit =>
						(variant.ident.clone(), format!("{}", variant.ident).to_lowercase().replace('_', " ")),
					_ => panic!(
						"Parse cannot be derived for enums containing non-unit variants; variant {} is not a unit.",
						variant.to_token_stream()
					),
				})
				.collect::<Vec<(syn::Ident, std::string::String)>>();
			let variant_identifiers = variant_identifiers_and_strings.iter().map(|(identifier, _)| identifier);
			let variant_strings = variant_identifiers_and_strings
				.iter()
				.map(|(_, string)| string.clone())
				.collect::<Vec<std::string::String>>();

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

/// # Panics
///
/// The derive macro can only be implemented for enums, and does not support unnamed fields.
#[proc_macro_derive(ErrorCodes)]
pub fn error_codes_derive(input: TokenStream) -> TokenStream {
	let type_ = syn::parse::<syn::DeriveInput>(input).unwrap();

	match type_.data {
		syn::Data::Enum(enum_) => {
			let name = type_.ident;
			let variant_identifiers_and_strings = enum_
				.variants
				.iter()
				.map(|variant| {
					(variant.ident.clone(), format!("{}", variant.ident).to_lowercase(), variant.fields.clone())
				})
				.collect::<Vec<(syn::Ident, std::string::String, syn::Fields)>>();
			let variant_identifiers = variant_identifiers_and_strings.iter().map(|(identifier, _, _)| identifier);
			let variant_fields = variant_identifiers_and_strings
				.iter()
				.map(|(_, _, fields)| {
					fields.iter().map(|field| {
						field.clone().ident.expect("Unnamed fields, e.g. with tuple elements, are not allowed!")
					})
				})
				.map(|fields| quote! { #( #fields: crate::default_hacks::FakeDefaultForIgnoredValues::default(), )*})
				.collect::<Vec<_>>();

			quote! {
				#[automatically_derived]
				#[allow(missing_docs)]
				impl crate::error::ErrorCodes for #name {
					fn all_codes() -> std::collections::HashMap<std::mem::Discriminant<crate::error::AssemblyError>, flexstr::SharedStr> {
						let mut map = std::collections::HashMap::new();
						#( let error_instance = &Self::#variant_identifiers {
								#variant_fields
							};
							map.insert(std::mem::discriminant(error_instance), error_instance.code().expect("error must have a code").to_string().into()); )*
						return map;
					}
				}
			}
			.into()
		},
		_ => panic!("ErrorCodes cannot be derived for non-enum types."),
	}
}

/// # Panics
///
/// This derive macro can only be used on enums.
#[proc_macro_derive(VariantName)]
pub fn enum_name_derive(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);

	let identifier = input.ident;
	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
	let variants = match input.data {
		Data::Enum(data) => data.variants.into_iter().map(|variant| {
			let identifier = variant.ident;
			let identifier_string = identifier.to_string();
			let fields = match variant.fields {
				Fields::Named(_) => quote!({ .. }),
				Fields::Unnamed(_) => quote!((..)),
				Fields::Unit => quote!(),
			};

			quote! {
				Self::#identifier #fields => #identifier_string
			}
		}),
		_ => panic!("not an enum"),
	};

	(quote! {
		impl #impl_generics crate::VariantName for #identifier #ty_generics #where_clause {
			fn variant_name(&self) -> &'static str {
				match self {
					#(#variants),*
				}
			}
		}
	})
	.into()
}
