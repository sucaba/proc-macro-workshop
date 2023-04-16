use proc_macro;
use proc_macro2;
use quote::{quote, quote_spanned};
use std::collections::HashSet;
use syn::spanned::Spanned;
use syn::GenericArgument;

use syn::{
    parse_macro_input, parse_quote, Data, DeriveInput, Expr, ExprLit, Field, GenericParam,
    Generics, Ident, Lit, LitStr, PathArguments, Type, TypePath,
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
    let mut input = parse_macro_input!(input as DeriveInput);
    let bound = match input.debug_struct_attr() {
        Ok(Some(v)) => Some(v.bound),
        Ok(None) => None,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

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

        let info = DebugFieldInfo::new(f);
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
    let mut bc = DebugBounds::new();
    data.fields.iter().for_each(|f| bc.collect(&f.ty));

    let generics;
    // panic!("**bc = {:#?}", bc);
    let wc = input.generics.make_where_clause();
    if let Some(bound) = bound {
        wc.predicates.push(syn::parse_str(&bound).unwrap());
        generics = input.generics;
    } else {
        generics = bc.apply(input.generics);
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    // let where_clause = add_field_type_bounds(where_clause, field_types);

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

#[derive(Debug)]
struct DebugStructAttr {
    pub bound: String,
}

trait HasStructDebugAttr {
    fn debug_struct_attr(&self) -> syn::Result<Option<DebugStructAttr>>;
}

impl HasStructDebugAttr for DeriveInput {
    fn debug_struct_attr(&self) -> syn::Result<Option<DebugStructAttr>> {
        let Some(l) = self.attrs.iter().find_map(|a| {
            let Ok(l) = a.meta.require_list() else { return None; };
            l.path.is_ident("debug").then_some(l)
        }) else { return Ok(None); };

        let mut result = None;
        l.parse_nested_meta(|meta| {
            if meta.path.is_ident("bound") {
                let v: LitStr = meta.value()?.parse()?;
                result = Some(DebugStructAttr { bound: v.value() });
            }
            Ok(())
        })?;
        Ok(result)
    }
}

#[derive(Debug)]
struct DebugFieldAttr {
    format: LitStr,
}

struct DebugFieldInfo<'a>(&'a Field);

impl<'a> DebugFieldInfo<'a> {
    fn new(field: &'a Field) -> Self {
        Self(field)
    }

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

trait TypeVisitor<'a> {
    fn visit_phantom_type(&mut self, ident: &'a TypePath);
    fn visit_associated_type(&mut self, path: &'a TypePath);
    fn visit_generic_type(&mut self, path: &'a TypePath, args: Vec<&'a Type>);
    fn visit_non_generic_type(&mut self, path: &'a TypePath);

    //fn dispatch_generic_type(&mut self, );

    fn dispatch(&mut self, ty: &'a Type) {
        match ty {
            Type::Path(tp) => {
                if tp.path.segments.len() == 1 {
                    let is_phantom_data = tp
                        .path
                        .segments
                        .first()
                        .map(|sg| sg.ident == "PhantomData")
                        .unwrap_or(false);
                    // panic!("tp.path={:#?} is phantom={}", tp.path, is_phantom_data);
                    if is_phantom_data {
                        self.visit_phantom_type(tp);
                    } else {
                        if let Some(args) = generic_type_args(tp) {
                            self.visit_generic_type(tp, args);
                        } else {
                            self.visit_non_generic_type(tp);
                        }
                    }
                } else {
                    self.visit_associated_type(tp);
                }
            }
            _ => {}
        }
    }
}

fn generic_type_args(tp: &TypePath) -> Option<Vec<&Type>> {
    let Some(segm) = tp.path.segments.last() else { return None; };
    let PathArguments::AngleBracketed(angle_bracketed)
        = &segm.arguments
        else { return None; };
    let mut result = Vec::new();
    for arg in &angle_bracketed.args {
        let GenericArgument::Type(ty)
            = arg
            else { continue; };
        result.push(ty);
    }

    Some(result)
}

fn first_generic_arg(tp: &TypePath) -> Option<&Ident> {
    let Some(segm) = tp.path.segments.last() else { return None; };
    let PathArguments::AngleBracketed(angle_bracketed)
        = &segm.arguments
        else { return None; };
    let Some(GenericArgument::Type(Type::Path(param)))
        = angle_bracketed.args.first()
        else { return None; };
    let Some(ident) = param.path.get_ident() else { return None; };
    Some(ident)
}

#[derive(Debug)]
struct DebugBounds<'a> {
    phantom_generic_types: HashSet<&'a Ident>,
    associated_types: HashSet<&'a TypePath>,
    used_generic_types: HashSet<&'a Ident>, // Non-generic
}

impl<'a> DebugBounds<'a> {
    fn new() -> Self {
        Self {
            phantom_generic_types: HashSet::new(),
            used_generic_types: HashSet::new(),
            associated_types: HashSet::new(),
        }
    }

    fn collect(&mut self, ty: &'a Type) {
        self.dispatch(ty)
    }

    // Add a bound `T: Debug` to every type parameter T.
    fn apply(&mut self, mut generics: Generics) -> Generics {
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                if self.used_generic_types.contains(&type_param.ident)
                    && !self.phantom_generic_types.contains(&type_param.ident)
                {
                    type_param.bounds.push(parse_quote!(::std::fmt::Debug));
                }
            }
        }
        let w = generics.make_where_clause();
        for t in &self.associated_types {
            w.predicates.push(parse_quote!(#t : ::std::fmt::Debug));
        }
        generics
    }
}

impl<'a> Default for DebugBounds<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> TypeVisitor<'a> for DebugBounds<'a> {
    fn visit_phantom_type(&mut self, tp: &'a TypePath) {
        if let Some(ident) = first_generic_arg(tp) {
            self.phantom_generic_types.insert(ident);
        }
    }

    fn visit_associated_type(&mut self, path: &'a TypePath) {
        self.associated_types.insert(path);
    }

    fn visit_non_generic_type(&mut self, path: &'a TypePath) {
        if let Some(ident) = path.path.get_ident() {
            self.used_generic_types.insert(ident);
        }
    }

    fn visit_generic_type(&mut self, _path: &'a TypePath, args: Vec<&'a Type>) {
        for arg in args {
            self.dispatch(arg);
        }
    }
}
