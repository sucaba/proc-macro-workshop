use proc_macro;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use syn::{parse_macro_input, Data, DeriveInput, Lit, LitStr};

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
    let name_str = LitStr::new(&name.to_string(), name.span());
    let Data::Struct(data) = input.data else { panic!("Expected 'struct'") };
    let fields = data.fields.iter().map(|f| {
        let n = f.ident.as_ref().expect("Struct should have identifier");
        let n_str = LitStr::new(&n.to_string(), n.span());
        quote_spanned!( f.span() =>
            .field(#n_str, &self.#n)
        )
    });
    let result: proc_macro2::TokenStream = quote!(
        impl ::std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_str)
                    #( #fields )*
                .finish()
            }
        }
    );
    // panic!("result = {result}");
    result.into()
}
