extern crate proc_macro;
extern crate proc_macro2;

use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::*;

use proc_macro::TokenStream;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let ident_builder = format_ident!("{}Builder", ident);

    let (req_field_names, req_field_types, opt_field_names, opt_field_types) =
        get_field_names_and_types(&input);

    let opt_field_stripped_types = opt_field_types.iter().map(|ty| get_stripped_type(&ty));

    let builder_struct = quote!(
        pub struct #ident_builder {
            #( #req_field_names: Option<#req_field_types> , )*
            #( #opt_field_names: #opt_field_types , )*
        }
    );

    let builder_setters = quote!(
        #(
        fn #req_field_names(&mut self, #req_field_names: #req_field_types) -> &mut Self {
            self.#req_field_names = Some(#req_field_names);
            self
        }
        )*

        // optional types, uses stripped (Optional<> removed) type for input arg type
        #(
        fn #opt_field_names(&mut self, #opt_field_names: #opt_field_stripped_types) -> &mut Self {
            self.#opt_field_names = Some(#opt_field_names);
            self
        }
        )*
    );

    let builder_build_func = quote!(
        pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
            // None checks
            #(
            if self.#req_field_names.clone() == None {
                return Err(concat!(stringify!(#req_field_names), " is None !!").into());
            }
            )*

            // Return Ok() struct
            Ok( #ident {
                #( #req_field_names: self.#req_field_names.clone().unwrap(), )*
                #( #opt_field_names: self.#opt_field_names.clone(), )*
            } )
        }
    );

    let builder_funcs = quote!(
        impl #ident_builder {
            #builder_setters
            #builder_build_func
        }
    );

    let builder_constructor = quote!(
        impl #ident {
            pub fn builder() -> #ident_builder {
                #ident_builder {
                    #( #req_field_names: None , )*
                    #( #opt_field_names: None , )*
                }
            }
        }
    );

    proc_macro::TokenStream::from(quote! {
        #builder_struct
        #builder_constructor
        #builder_funcs
    })
}

fn get_field_names_and_types<'a>(
    input: &'a DeriveInput,
) -> (
    Vec<&'a Option<syn::Ident>>,
    Vec<&'a syn::Type>,
    Vec<&'a Option<syn::Ident>>,
    Vec<&'a syn::Type>,
) {
    let named_fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(named_fields),
            ..
        }) => &named_fields.named,
        _ => panic!("expected a struct with named fields"),
    };

    let is_optional = |named_field: &syn::Field| {
        if let syn::Type::Path(path) = &named_field.ty {
            path.path.segments[0].ident == syn::Ident::new("Option", Span::call_site())
        } else {
            false
        }
    };

    let (opt_fields, req_fields): (_, Vec<_>) =
        named_fields.iter().partition(|field| is_optional(field));

    let opt_field_names = opt_fields.iter().map(|field| &field.ident).collect();
    let opt_field_types = opt_fields.iter().map(|field| &field.ty).collect();

    let req_field_names = req_fields.iter().map(|field| &field.ident).collect();
    let req_field_types = req_fields.iter().map(|field| &field.ty).collect();

    (
        req_field_names,
        req_field_types,
        opt_field_names,
        opt_field_types,
    )
}

// Given a type Optional<T> extract the ident of T
fn get_stripped_type(opt_field_type: &syn::Type) -> &proc_macro2::Ident {
    if let Type::Path(path) = &opt_field_type {
        if let PathArguments::AngleBracketed(args) = &path.path.segments[0].arguments {
            if let GenericArgument::Type(Type::Path(p)) = &args.args[0] {
                return &p.path.segments[0].ident;
            }
        }
    }
    panic!(concat!("Can't extract T from Option<T>: ", stringify!(opt_field_type)));
}
