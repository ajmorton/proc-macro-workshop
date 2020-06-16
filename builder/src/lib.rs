extern crate proc_macro;
extern crate proc_macro2;

use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::*;

use proc_macro::TokenStream;

#[proc_macro_derive(Builder, attributes(builder))]
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

    let builder_fields: std::result::Result<Vec<_>, syn::Error> =
        named_fields.iter().map(|f| get_builder_field(f)).collect();

    match builder_fields {
        Ok(builder_fields) => {
            let builder_struct = builder_struct(&ident_builder, &builder_fields);
            let builder_constructor = builder_constructor(&ident, &ident_builder, &builder_fields);
            let builder_setters: Vec<_> =
                builder_fields.iter().map(|f| builder_setter(f)).collect();
            let builder_build_func = builder_build_func(&ident, &builder_fields);

            let builder_impl = quote!(
                impl #ident_builder {
                    #(#builder_setters)*
                    #builder_build_func
                }
            );

            proc_macro::TokenStream::from(quote! {
                #builder_struct
                #builder_constructor
                #builder_impl
            })
        }
        Err(err) => err.to_compile_error().into(),
    }
}

// The fields that the builder operates on, with relevant type data
struct BuilderField {
    ident: syn::Ident,
    typ: syn::TypePath,
    is_opt: bool,
    each_arg: Option<syn::Ident>,
}

// Given a type with angled brackets (Option<T> / Vec<T>) strip the type and return T
fn strip_angled(path: &syn::TypePath) -> syn::TypePath {
    if let PathArguments::AngleBracketed(args) = &path.path.segments[0].arguments {
        if let GenericArgument::Type(Type::Path(path)) = &args.args[0] {
            return path.clone();
        }
    }
    panic!(concat!("Can't extract T from type", stringify!(path)));
}

// get the type of a field. If the field is an Option<> then strip the option and set is_opt to true
fn get_type(named_field: &syn::Field) -> (syn::TypePath, bool) {
    if let syn::Type::Path(path) = &named_field.ty {
        let is_opt = path.path.segments[0].ident == syn::Ident::new("Option", Span::call_site());
        (path.clone(), is_opt)
    } else {
        panic!("can't get type")
    }
}

// create the constructor for the builder (all fields = None)
fn builder_constructor(
    ident: &syn::Ident,
    ident_builder: &syn::Ident,
    builder_fields: &Vec<BuilderField>,
) -> proc_macro2::TokenStream {
    let initialisers: Vec<_> = builder_fields
        .iter()
        .map(|field| initialiser(field))
        .collect();

    quote!(
        impl #ident {
            pub fn builder() -> #ident_builder {
                #ident_builder {
                    #( #initialisers , )*
                }
            }
        }
    )
}

fn initialiser(field: &BuilderField) -> proc_macro2::TokenStream {
    let field_name = &field.ident;

    if field.each_arg.is_some() {
        quote!(#field_name: vec!())
    } else {
        quote!(#field_name: std::option::Option::None)
    }
}

// setters for the builder
fn builder_setter(builder_field: &BuilderField) -> proc_macro2::TokenStream {
    let field_name = &builder_field.ident;
    let field_typ = &builder_field.typ;

    if let Some(each_arg) = &builder_field.each_arg {
        let push = quote!(
            fn #each_arg(&mut self, #each_arg: #field_typ) -> &mut Self {
                self.#field_name.push(#each_arg);
                self
            }
        );

        let all_at_once = if each_arg.to_string() != field_name.to_string() {
            quote!(
                fn #field_name(&mut self, #field_name: std::vec::Vec<#field_typ>) -> &mut Self {
                    self.#field_name = #field_name.clone();
                    self
                }
            )
        } else {
            quote!()
        };

        quote!(
            #push
            #all_at_once
        )
    } else {
        quote!(
            fn #field_name(&mut self, #field_name: #field_typ) -> &mut Self {
                self.#field_name = std::option::Option::Some(#field_name);
                self
            }
        )
    }
}

// the build() function to convert from the builder to the buildee
fn builder_build_func(
    ident: &syn::Ident,
    builder_fields: &Vec<BuilderField>,
) -> proc_macro2::TokenStream {
    let none_checks: Vec<_> = builder_fields.iter().map(|f| none_check(f)).collect();
    let set_fields: Vec<_> = builder_fields.iter().map(|f| set_field(f)).collect();

    quote!(
        pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
            #(#none_checks)*

            std::result::Result::Ok( #ident {
                #(#set_fields ,)*
            })
        }
    )
}

// Check if a field is None and early return error if it is
// Fields that are optional in the buildee are not checked
fn none_check(field: &BuilderField) -> proc_macro2::TokenStream {
    let field_name = &field.ident;
    if field.is_opt || field.each_arg.is_some() {
        quote!()
    } else {
        quote!(
            if self.#field_name.clone() == std::option::Option::None {
                return Err(concat!(stringify!(#field_name), " is None !!").into());
            }
        )
    }
}

// set a fields in the buildee with the value in the builder
fn set_field(field: &BuilderField) -> proc_macro2::TokenStream {
    let field_name = &field.ident;
    if field.is_opt || field.each_arg.is_some() {
        quote!(#field_name: self.#field_name.clone())
    } else {
        quote!(#field_name: self.#field_name.clone().unwrap())
    }
}

// get value in the #[each = value] attribute
fn each_name(
    attrs: &Vec<syn::Attribute>,
) -> std::result::Result<Option<proc_macro2::Ident>, syn::Error> {
    for attr in attrs {
        if let Ok(syn::Meta::List(meta)) = attr.parse_meta() {
            for item in &meta.nested {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = &item {
                    if let Some(ident) = nv.path.get_ident() {
                        if ident == &syn::Ident::new("each", Span::call_site()) {
                            if let syn::Lit::Str(each_name) = &nv.lit {
                                return Ok(Some(syn::Ident::new(
                                    &each_name.value(),
                                    Span::call_site(),
                                )));
                            }
                        } else {
                            return Err(syn::Error::new_spanned(
                                &meta,
                                "expected `builder(each = \"...\")`",
                            ));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

// Convert from a syn::Field to internal BuilderField type
fn get_builder_field(field: &syn::Field) -> std::result::Result<BuilderField, syn::Error> {
    let each_arg = each_name(&field.attrs)?;

    if let Some(ident) = field.ident.clone() {
        let (typ, is_opt) = get_type(field);
        let typ = if is_opt || each_arg.is_some() {
            strip_angled(&typ)
        } else {
            typ
        };

        Ok(BuilderField {
            ident,
            typ,
            is_opt,
            each_arg,
        })
    } else {
        Err(syn::Error::new::<&str>(
            Span::call_site(),
            "Can't extract BuilderField details".into(),
        ))
    }
}

// Return tokens for the builder struct
fn builder_struct(
    builder_name: &syn::Ident,
    named_fields: &Vec<BuilderField>,
) -> proc_macro2::TokenStream {
    let struct_fields = named_fields.iter().map(|field: &BuilderField| {
        let (field_name, field_type) = (&field.ident, &field.typ);
        if field.each_arg.is_some() {
            quote!(#field_name: std::vec::Vec<#field_type>)
        } else {
            quote!(#field_name: std::option::Option<#field_type>)
        }
    });

    quote!(
        pub struct #builder_name {
            #(#struct_fields , )*
        }
    )
}
