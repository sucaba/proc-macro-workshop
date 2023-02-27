use proc_macro;
use proc_macro2;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Ident, Index, PathArguments,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = format_ident!("{}{}", name, "Builder");

    let builder_field_defs = get_field_defs(&input.data);
    let builder_field_inits = get_field_inits(&input.data);
    let builder_methods = get_builder_methods(&input.data);
    let build_method_code = get_build_method_code(&input.data);

    let result = quote!(
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #builder_field_inits
                }
            }
        }

        pub struct #builder_name {
            #builder_field_defs
        }

        impl #builder_name {
            #builder_methods

            fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #build_method_code
                })
            }
        }
    )
    .into();

    // panic!("HERE\n{}", result);
    result
}

fn get_build_method_code(data: &syn::Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    if is_optional_field(f) {
                        quote_spanned! {f.span()=>
                            #name : self.#name.take()
                        }
                    } else {
                        quote_spanned! {f.span()=>
                            #name : self.#name.take().ok_or(std::sync::mpsc::RecvError)?
                        }
                    }
                });
                quote! {
                    #( #recurse ),*
                }
            }
            Fields::Unnamed(ref fields) => {
                let recurse = fields.unnamed.iter().enumerate().map(|(i, f)| {
                    let index = Index::from(i);
                    quote_spanned! {f.span()=>
                        self.#index.take().ok_or(std::sync::mpsc::RecvError)?
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

fn is_optional_field(f: &syn::Field) -> bool {
    match &f.ty {
        syn::Type::Path(path) => {
            let left = &path.path.segments.first().unwrap().ident;
            let right = Ident::new("Option", proc_macro2::Span::call_site());
            left == &right
        }
        _ => false,
    }
}

fn unwrap_option_ty(ty: &syn::Type) -> &syn::Type {
    match ty {
        syn::Type::Path(path) => {
            let left = &path.path.segments.first().unwrap().ident;
            let right = Ident::new("Option", proc_macro2::Span::call_site());
            if left == &right {
                match &path.path.segments[0].arguments {
                    PathArguments::AngleBracketed(a) => {
                        let generic = a.args.first().unwrap();
                        match generic {
                            GenericArgument::Type(t) => t,
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            } else {
                ty
            }
        }
        _ => todo!(),
    }
}

fn get_builder_methods(data: &syn::Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if is_optional_field(&f) {
                        let unwrapped_ty = unwrap_option_ty(ty);
                        quote_spanned! {f.span()=>
                            fn #name(&mut self, value: #unwrapped_ty) -> &mut Self {
                                self.#name = Some(value);
                                self
                            }
                        }
                    } else {
                        quote_spanned! {f.span()=>
                            fn #name(&mut self, value: #ty) -> &mut Self {
                                self.#name = Some(value);
                                self
                            }
                        }
                    }
                });
                quote! {
                    #(
                    #recurse
                    )*
                }
            }
            Fields::Unnamed(ref fields) => {
                let recurse = fields.unnamed.iter().enumerate().map(|(i, f)| {
                    let ty = &f.ty;
                    let index = Index::from(i);
                    let method_name = format_ident!("set_{}", i);
                    quote_spanned! {f.span()=>
                        fn #method_name(&mut self, value: #ty) -> &mut Self {
                            self.#index = Some(value);
                            self
                        }
                    }
                });
                quote! {
                    #(
                    #recurse
                    )*
                }
            }
            Fields::Unit => {
                quote!()
            }
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn get_field_defs(data: &syn::Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if is_optional_field(f) {
                        quote_spanned! {f.span()=>
                            #name : #ty,
                        }
                    } else {
                        quote_spanned! {f.span()=>
                            #name : Option<#ty>,
                        }
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
