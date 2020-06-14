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

    let named_fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(named_fields),
            ..
        }) => &named_fields.named,
        _ => panic!("expected a struct with named fields"),
    };

    let builder_fields: Vec<_> = named_fields.iter().map(|f| get_builder_field(f)).collect();

    let builder_struct = builder_struct(&ident_builder, &builder_fields);
    let builder_constructor = builder_constructor(&ident, &ident_builder, &builder_fields);
    let builder_setters: Vec<_> = builder_fields.iter().map(|f| builder_setter(f)).collect();
    let builder_build_func = builder_build_func(&ident, &builder_fields);

    let builder_funcs = quote!(
        impl #ident_builder {
            #(#builder_setters)*
            #builder_build_func
        }
    );

    proc_macro::TokenStream::from(quote! {
        #builder_struct
        #builder_constructor
        #builder_funcs
    })
}

// The fields that the builder operates on, with relevant type data
struct BuilderField {
    ident: syn::Ident,
    typ: syn::Type,
    is_opt: bool
}

// Given an Option<T> type strip the option and return T
fn strip_opt(path: &syn::TypePath) -> syn::Type {
    if let PathArguments::AngleBracketed(args) = &path.path.segments[0].arguments {
        if let GenericArgument::Type(Type::Path(p)) = &args.args[0] {
            return syn::Type::Path(p.clone());
        }
    }
    panic!(concat!("Can't extract T from Option<T> for type", stringify!(path)));
}

// get the type of a field. If the field is an Option<> then strip the option and set is_opt to true
fn get_type(named_field: &syn::Field) -> (syn::Type, bool) {

    if let syn::Type::Path(path) = &named_field.ty {
        let is_opt = path.path.segments[0].ident == syn::Ident::new("Option", Span::call_site());
        let typ = if is_opt {
            strip_opt(path)
        } else {
            syn::Type::Path(path.clone())
        };
        (typ, is_opt)
    } else {
        panic!("can't get type")
    }
}

// create the constructor for the builder (all fields = None)
fn builder_constructor(ident: &syn::Ident, ident_builder: &syn::Ident, builder_fields: &Vec<BuilderField>) -> proc_macro2::TokenStream {
    let field_names: Vec<_> = builder_fields.iter().map(|f| &f.ident).collect();
    quote!(
        impl #ident {
            pub fn builder() -> #ident_builder {
                #ident_builder {
                    #( #field_names: None , )*
                }
            }
        }
    )
}

// setters for the builder
fn builder_setter(builder_field: &BuilderField) -> proc_macro2::TokenStream {
    let field_name = &builder_field.ident;
    let field_typ = &builder_field.typ;

    quote!(
        fn #field_name(&mut self, #field_name: #field_typ) -> &mut Self {
            self.#field_name = Some(#field_name);
            self
        }
    )
}

// the build() function to convert from the builder to the buildee
fn builder_build_func(ident: &syn::Ident, builder_fields: &Vec<BuilderField>) -> proc_macro2::TokenStream {

    let none_checks: Vec<_> = builder_fields.iter().map(|f| none_check(f)).collect();
    let set_fields: Vec<_> = builder_fields.iter().map(|f| set_field(f)).collect();

    quote!(
        pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
            #(#none_checks)*

            Ok( #ident {
                #(#set_fields ,)*
            })
        }
    )
}

// Check if a field is None and early return error if it is
// Fields that are optional in the buildee are not checked
fn none_check(field: &BuilderField) -> proc_macro2::TokenStream {
    let field_name = &field.ident;
    if field.is_opt {
        quote!()    
    } else {
        quote!(
            if self.#field_name.clone() == None {
                return Err(concat!(stringify!(#field_name), " is None !!").into());
            }
        )
    }
}

// set a fields in the buildee with the value in the builder
fn set_field(field: &BuilderField) -> proc_macro2::TokenStream {
    let field_name = &field.ident;
    if field.is_opt {
        quote!(#field_name: self.#field_name.clone())
    } else {
        quote!(#field_name: self.#field_name.clone().unwrap())
    }
}

// Convert from a syn::Field to locally used BuilderField
fn get_builder_field(field: &syn::Field) -> BuilderField {

    if let Some(ident) = &field.ident {
        let(typ, is_opt) = get_type(field);

        BuilderField {
            ident: ident.clone(),
            typ,
            is_opt
        }
    } else {
        panic!("Can't extract BuilderField details")
    }
}

// Return tokens for the builder struct
fn builder_struct(builder_name: &syn::Ident, named_fields: &Vec<BuilderField>) -> proc_macro2::TokenStream {

    let struct_fields = named_fields.iter().map(
        |field: &BuilderField| { 
            let (field_name, field_type) = (&field.ident, &field.typ);
            quote!(#field_name: Option<#field_type>)
        }
    );

    quote!(
        pub struct #builder_name {
            #(#struct_fields , )*
        }
    )
}