use proc_macro;
use proc_macro2;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use syn::{
    parse_macro_input, parse_quote, Data, DeriveInput, Expr, ExprLit, Field, GenericParam,
    Generics, Lit, LitStr,
};

mod use_case {
    struct Field<'a> {
        name: &'a str,
        bitmask: u8,
    }

    impl<'a> ::std::fmt::Debug for Field<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("Field")?;
            f.write_str(" { ")?;
            f.write_fmt(format_args!("name: {:?}, ", self.name))?;
            f.write_fmt(format_args!("bitmask: 0b{:08b}", self.bitmask))?;
            f.write_str(" }")
        }
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let name_str = LitStr::new(&name.to_string(), name.span());
    let Data::Struct(data) = input.data else { panic!("Expected 'struct'") };

    let fields = data.fields.iter().enumerate().map(|(i, f)| {
        let needs_comma = i != 0;
        let writing_comma = needs_comma.then(|| {
            quote!(
                f.write_str(", ")?;
            )
        });

        let info = DebugFieldInfo(f);
        let n = info.name();
        let n_str = info.name_str();
        let attr = info.debug_attr().expect("unable to parse debug attr");
        let fmt = attr
            .map(|attr| attr.format)
            .unwrap_or_else(|| LitStr::new("{:?}", f.span()));
        quote_spanned!( f.span() =>
            #writing_comma
            f.write_fmt(format_args!(concat!(#n_str, ": ", #fmt), self.#n))?;
        )
    });

    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let result: proc_macro2::TokenStream = quote!(
        impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(#name_str)?;
                f.write_str(" { ")?;
                #( #fields )*
                f.write_str(" }")
            }
        }
    );
    // panic!("result = {result}");
    result.into()
}

// Add a bound `T: Debug` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(::std::fmt::Debug));
        }
    }
    generics
}

#[derive(Debug)]
struct DebugFieldAttr {
    format: LitStr,
}

struct DebugFieldInfo<'a>(&'a Field);

impl<'a> DebugFieldInfo<'a> {
    pub fn name(&self) -> &proc_macro2::Ident {
        self.0
            .ident
            .as_ref()
            .expect("Field should have an identifier")
    }

    pub fn name_str(&self) -> LitStr {
        let n = self.name();
        LitStr::new(&n.to_string(), n.span())
    }

    pub fn debug_attr(&self) -> syn::Result<Option<DebugFieldAttr>> {
        let f: &Field = self.0;

        let name_value = f
            .attrs
            .iter()
            .filter_map(|attr| attr.meta.require_name_value().ok())
            .find(|name_value| name_value.path.is_ident("debug"));
        let Some(name_value) = name_value else { return Ok(None); };

        let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = &name_value.value else { return Ok(None) };

        return Ok(Some(DebugFieldAttr {
            format: lit.clone(),
        }));
    }
}
