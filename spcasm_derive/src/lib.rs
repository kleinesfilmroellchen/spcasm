extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn;

#[proc_macro_derive(Parse)]
pub fn parse_macro_derive(input: TokenStream) -> TokenStream {
	let type_ = syn::parse::<syn::DeriveInput>(input).unwrap();

	match type_.data {
		syn::Data::Enum(enum_) => {
			let name = type_.ident;
			let name_string = format!("{}", name).to_lowercase();
			let variant_identifiers_and_strings = enum_
				.variants
				.iter()
				.map(|variant| match variant.fields {
					syn::Fields::Unit => (variant.ident.clone(), format!("{}", variant.ident).to_lowercase()),
					_ => panic!(
						"Parse cannot be derived for enums containing non-unit variants; variant {} is not a unit.",
						variant.to_token_stream()
					),
				})
				.collect::<Vec<(syn::Ident, String)>>();
			let variant_identifiers = variant_identifiers_and_strings.iter().map(|(identifier, _)| identifier);
			let variant_strings = variant_identifiers_and_strings.iter().map(|(_, string)| string);

			quote! {
				#[automatically_derived]
				#[allow(missing_docs)]
				impl crate::parser::Parse for #name {
					fn parse(value: &str, location: SourceSpan, src: Arc<AssemblyCode>) -> Result<Self, AssemblyError> {
						Ok(match value {
							#( #variant_strings => Self::#variant_identifiers, )*
							_ => return Err(AssemblyError::InvalidConstant { constant: value.to_owned(), typename: #name_string.to_owned(), location, src }),
						})
					}
				}
			}
			.into()
		},
		_ => panic!("Parse cannot be derived for non-enum types."),
	}
}
