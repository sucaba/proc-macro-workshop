use proc_macro;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use syn::{parse_macro_input, Data, DeriveInput};

struct Foo<'a> {
    name: &'a str,
    bitmask: u8,
}

impl<'a> ::std::fmt::Debug for Foo<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Foo")
            .field("name", &self.name)
            .field("bitmask", &self.bitmask)
            .finish()
    }
}

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let Data::Struct(data) = input.data else { panic!("Expected 'struct'") };
    let fields = data.fields.iter().map(|f| {
        let n = f.ident.as_ref().expect("Struct should have identifier");
        quote_spanned!( f.span() =>
            .field(stringify!(#n), &self.#n)
        )
    });
    let result: proc_macro2::TokenStream = quote!(
        impl ::std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                    #( #fields )*
                .finish()
            }
        }
    );
    // panic!("result = {result}");
    result.into()
}
