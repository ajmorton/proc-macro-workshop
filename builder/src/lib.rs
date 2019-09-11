extern crate proc_macro;
use proc_macro::TokenStream;
use syn::{
    parse_macro_input,
    DeriveInput,
    Ident,
    Data::Struct,
    DataStruct,
    Fields::Named,
    FieldsNamed,
    Type::Path
};
use quote::quote;


#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let tree = parse_macro_input!(input as DeriveInput);
    let ident = &tree.ident;
    let ident_builder = Ident::new(&format!("{}Builder", ident), ident.span());

    // untangling this gnarly path plagiarized directly from jonhoo's stream on this workshop
    let fields = if let Struct(DataStruct{
        fields: Named(FieldsNamed{
            ref named,
            ..
        }),
        ..
    }) = tree.data {
        named
    } else { unimplemented!() }; // TODO: Think about how to enable enums and unions
    
    let is_optional = |t: &syn::Type| {
        if let Path(ref t_path) = t {
            return t_path.path.segments.len() == 1 
                    && t_path.path.segments[0].ident == "Option";
        }
        false
    };
    
    // TODO: pick up here, ~ 1:32:37 in stream
    // let unwrap_t = |t: &syn::Type| -> syn::Type {
    //     if let Path(ref t_path) = t {
    //        assert!(t_path.path.segments.len() == 1 && t_path.path.segments[0].ident == "Option");
    //     }
    //     unreachable!();
    // };

    let opt_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        let inner_type = &f.ty;
        if is_optional(&f.ty) {
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
        if is_optional(&f.ty) {
            quote! {
                pub fn #inner_name(&mut self, #inner_name: #inner_type) -> &mut Self {
                    self.#inner_name = #inner_name;
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
    
    let nil_fields= fields.iter().map(|f| {
        let inner_name = &f.ident;
        quote! {
            #inner_name: None
        }
    });


    let build_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        if is_optional(&f.ty) {
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
