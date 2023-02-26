use proc_macro;
use proc_macro2;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Index};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = format_ident!("{}{}", name, "Builder");

    let builder_field_defs = get_field_defs(&input.data);
    let builder_field_inits = get_field_inits(&input.data);

    let builder_method = quote!(
      impl #name {
          pub fn builder() -> #builder_name {
              #builder_name {
                  #builder_field_inits
              }
          }
      }
    );

    let builder_struct = quote!(
        pub struct #builder_name {
          #builder_field_defs
        }
    );

    let result = proc_macro::TokenStream::from(quote!(
      #builder_method

      #builder_struct
    ));

    result
}

fn get_field_defs(data: &syn::Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    quote_spanned! {f.span()=>
                        #name : Option<#ty>,
                    }
                });
                quote! {
                    #(#recurse)*
                }
            }
            Fields::Unnamed(ref fields) => {
                let recurse = fields.unnamed.iter().enumerate().map(|(_i, f)| {
                    let ty = &f.ty;
                    quote_spanned! {f.span()=>
                        Option<#ty>
                    }
                });
                quote! {
                    #(#recurse),*
                }
            }
            Fields::Unit => {
                quote!()
            }
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn get_field_inits(data: &syn::Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! {f.span()=>
                        #name : None
                    }
                });
                quote! {
                    #(#recurse),*
                }
            }
            Fields::Unnamed(ref fields) => {
                let recurse = fields.unnamed.iter().enumerate().map(|(_i, f)| {
                    let ty = &f.ty;
                    quote_spanned! {f.span()=>
                        Option<#ty>
                    }
                });
                quote! {
                    #(#recurse),*
                }
            }
            Fields::Unit => {
                quote!()
            }
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}
