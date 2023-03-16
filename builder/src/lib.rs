use proc_macro;
use proc_macro2;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Ident, Index, PathArguments,
};

#[proc_macro_derive(Builder, attributes(builder))]
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
                let recurse = fields
                    .named
                    .iter()
                    .map(|f| BuilderFieldInfo(f).build_field_init_syntax());
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

#[derive(Clone, Copy)]
enum FieldKind<'a> {
    Other,
    Option(&'a syn::Type),
    Vec(&'a syn::Type),
}

enum FieldAttrKind<'a> {
    Generic,
    Plural { each: &'a str },
}

fn extract_path_generic_param(path: &syn::TypePath) -> Option<&syn::Type> {
    match &path.path.segments[0].arguments {
        PathArguments::AngleBracketed(a) => {
            let generic = a.args.first()?;
            match generic {
                GenericArgument::Type(t) => Some(t),
                _ => None,
            }
        }
        _ => None,
    }
}

fn get_builder_methods(data: &syn::Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields
                    .named
                    .iter()
                    .map(|f| BuilderFieldInfo(f).builder_method_syntax());
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

#[derive(Debug)]
struct BuilderFieldAttr {
    each: String,
}

struct BuilderFieldInfo<'a>(&'a syn::Field);

impl<'a> BuilderFieldInfo<'a> {
    pub fn kind(&self) -> FieldKind<'a> {
        let f = self.0;
        match &f.ty {
            syn::Type::Path(path) => {
                let left = &path.path.segments.first().unwrap().ident;

                let Some(param) = extract_path_generic_param(path) else { return FieldKind::Other; };

                let right = Ident::new("Option", proc_macro2::Span::call_site());
                if left == &right {
                    return FieldKind::Option(param);
                }
                let right = Ident::new("Vec", proc_macro2::Span::call_site());
                if left == &right {
                    return FieldKind::Vec(param);
                }

                FieldKind::Other
            }
            _ => todo!("Not supported field type"),
        }
    }

    pub fn builder_attr(&self) -> Option<BuilderFieldAttr> {
        let f: &syn::Field = self.0;
        for attr in &f.attrs {
            let attr_name = attr.path.get_ident().map(|s| s.to_string());
            if attr_name != Some("builder".to_owned()) {
                continue;
            }

            let Some(meta) = attr.parse_meta().ok() else { continue; };
            let syn::Meta::List(meta_list) = meta else { continue; };
            let Some(attr_name) = meta_list.path.get_ident() else { continue; };
            if attr_name.to_string() != "builder" {
                continue;
            }

            let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(name_value))) = meta_list.nested.first() else { continue; };
            let Some(name) = name_value.path.get_ident() else { continue; };
            if name.to_string() != "each" {
                continue;
            }
            let syn::Lit::Str(s) = &name_value.lit else { continue; };

            return Some(BuilderFieldAttr { each: s.value() });
        }
        None
    }

    pub fn field_init_syntax(&self) -> proc_macro2::TokenStream {
        let f = self.0;
        let name = &f.ident;
        match BuilderFieldInfo(f).kind() {
            FieldKind::Vec(_) => {
                quote_spanned! {f.span()=>
                    #name : Vec::new()
                }
            }
            _ => {
                quote_spanned! {f.span()=>
                    #name : None
                }
            }
        }
    }

    pub fn field_def_syntax(&self) -> proc_macro2::TokenStream {
        let f = self.0;
        let name = &f.ident;
        let ty = &f.ty;
        match BuilderFieldInfo(f).kind() {
            FieldKind::Other => {
                quote_spanned! {f.span()=>
                    #name : Option<#ty>,
                }
            }
            FieldKind::Option(_) => {
                quote_spanned! {f.span()=>
                    #name : #ty,
                }
            }
            FieldKind::Vec(_) => {
                quote_spanned! {f.span()=>
                    #name : #ty,
                }
            }
        }
    }

    pub fn build_field_init_syntax(&self) -> proc_macro2::TokenStream {
        let f = self.0;
        let name = &f.ident;
        match BuilderFieldInfo(f).kind() {
            FieldKind::Option(_) | FieldKind::Vec(_) => {
                quote_spanned! {f.span()=>
                    #name : ::std::mem::take(&mut self.#name)
                }
            }
            FieldKind::Other => {
                quote_spanned! {f.span()=>
                    #name : self.#name.take().ok_or(std::sync::mpsc::RecvError)?
                }
            }
        }
    }

    pub fn builder_method_syntax(&self) -> proc_macro2::TokenStream {
        let f = self.0;
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        match BuilderFieldInfo(f).kind() {
            FieldKind::Other => {
                quote_spanned! {f.span()=>
                    fn #name(&mut self, value: #ty) -> &mut Self {
                        self.#name = Some(value);
                        self
                    }
                }
            }
            FieldKind::Option(unwrapped_ty) => {
                quote_spanned! {f.span()=>
                    fn #name(&mut self, value: #unwrapped_ty) -> &mut Self {
                        self.#name = Some(value);
                        self
                    }
                }
            }
            FieldKind::Vec(unwrapped_ty) => {
                if let Some(a) = BuilderFieldInfo(f).builder_attr() {
                    let method_name = Ident::new(&a.each, f.span());

                    quote_spanned! {f.span()=>
                        fn #method_name(&mut self, value: #unwrapped_ty) -> &mut Self {
                            self.#name.push(value);
                            self
                        }
                    }
                } else {
                    quote_spanned! {f.span()=>
                        fn #name(&mut self, value: #ty) -> &mut Self {
                            self.#name = value;
                            self
                        }
                    }
                }
            }
        }
    }
}

fn get_field_defs(data: &syn::Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields
                    .named
                    .iter()
                    .map(|f| BuilderFieldInfo(f).field_def_syntax());
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
                let recurse = fields
                    .named
                    .iter()
                    .map(|f| BuilderFieldInfo(f).field_init_syntax());
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
