extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data::Struct, DataStruct, DeriveInput, Fields::Named, FieldsNamed, Ident,
    PathArguments::AngleBracketed, Type::Path,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let tree = parse_macro_input!(input as DeriveInput);
    let ident = &tree.ident;
    let ident_builder = Ident::new(&format!("{}Builder", ident), ident.span());

    // untangling this gnarly path plagiarized directly from jonhoo's stream on this workshop
    let fields = if let Struct(DataStruct {
        fields: Named(FieldsNamed { ref named, .. }),
        ..
    }) = tree.data
    {
        named
    } else {
        unimplemented!()
    }; // TODO: Think about how to enable enums and unions

    fn unwrap_inner_type(t: &syn::Type) -> Option<&syn::Type> {
        if let Path(ref t_path) = t {
            if t_path.path.segments.len() == 1 && t_path.path.segments[0].ident == "Option" {
                if let AngleBracketed(ref inner_type) = t_path.path.segments[0].arguments {
                    if inner_type.args.len() == 1 {
                        let inner = inner_type.args.first().unwrap();
                        if let syn::GenericArgument::Type(ref t) = inner {
                            return Some(t);
                        }
                    }
                }
            }
        }
        None
    }

    // TODO: pick up here, ~ 1:32:37 in stream
    // let unwrap_t = |t: &syn::Type| -> syn::Type {
    //     if let Path(ref t_path) = t {
    //         if let AngleBracketed(inner_type) = t_path.path.segments[0].arguments {
    //         } else {
    //             panic!("Cannot unwrap inner type from Option<inner_type>");
    //         }
    //     }
    //     unreachable!();
    // };

    let opt_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        let inner_type = &f.ty;
        if unwrap_inner_type(&f.ty).is_some() {
            quote! {
                #inner_name: #inner_type
            }
        } else {
            quote! {
                #inner_name: std::option::Option<#inner_type>
            }
        }
    });

    let methods = fields.iter().map(|f| {
        let inner_name = &f.ident;
        let inner_type = &f.ty;
        if let Some(inner_type) = unwrap_inner_type(&f.ty) {
            quote! {
                pub fn #inner_name(&mut self, #inner_name: #inner_type) -> &mut Self {
                    self.#inner_name = Some(#inner_name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #inner_name(&mut self, #inner_name: #inner_type) -> &mut Self {
                    self.#inner_name = Some(#inner_name);
                    self
                }
            }
        }
    });

    let nil_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        quote! {
            #inner_name: None
        }
    });

    let build_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        if unwrap_inner_type(&f.ty).is_some() {
            quote! {
                #inner_name: self.#inner_name.clone()
            }
        } else {
            quote! {
                #inner_name: self.#inner_name.clone().ok_or(concat!(stringify!(#inner_name), " cannot be None"))?
            }
        }
    });

    let quoted = quote! {
        use std::error::Error;

        pub struct #ident_builder {
            #(#opt_fields,)*
        }

        impl #ident {
            pub fn builder() -> #ident_builder {
                 #ident_builder {
                    #(#nil_fields,)*
                 }
            }
        }

        impl #ident_builder {
            pub fn build(&mut self) -> Result<#ident, Box<dyn Error>> {
                Ok(#ident{
                    #(#build_fields,)*
                })
            }

            #(#methods)*

        }
    };

    TokenStream::from(quoted)
}
