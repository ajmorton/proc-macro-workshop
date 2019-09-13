extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{
    parse_macro_input, Data::Struct, DataStruct, DeriveInput, Fields::Named, FieldsNamed, Ident,
    PathArguments::AngleBracketed, Type::Path,
};

#[proc_macro_derive(Builder, attributes(builder))]
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

    fn unwrap_inner_type<'inner>(
        container: &'static str,
        t: &'inner syn::Type,
    ) -> Option<&'inner syn::Type> {
        if let Path(ref t_path) = t {
            if t_path.path.segments.len() == 1 && t_path.path.segments[0].ident == container {
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

    let opt_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        let inner_type = &f.ty;
        if unwrap_inner_type("Option", &f.ty).is_some() {
            quote! {
                #inner_name: #inner_type
            }
        } else {
            quote! {
                #inner_name: std::option::Option<#inner_type>
            }
        }
    });

    // TODO: 2:16:05, working on bool to control for methods/extend_methods not colliding
    let extend_methods = fields.iter().filter_map(|f| {
        let name = &f.ident;
        for attr in &f.attrs {
            if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
                let mut stream = attr.tokens.clone().into_iter();
                if let Some(proc_macro2::TokenTree::Group(group)) = stream.next() {
                    let mut tokens = group.stream().into_iter();
                    match tokens.next().unwrap() {
                        TokenTree::Ident(ref i) => assert_eq!("each", i.to_string()),
                        _ => panic!("Not an ident or punct"),
                    }
                    match tokens.next().unwrap() {
                        TokenTree::Punct(ref p) => assert_eq!('=', p.as_char()),
                        _ => panic!("Not an ident or punct"),
                    }

                    let arg = match tokens.next().unwrap() {
                        TokenTree::Literal(lit) => lit,
                        _ => panic!("Not a literal"),
                    };

                    match syn::Lit::new(arg) {
                        syn::Lit::Str(s) => {
                            let arg_ident = syn::Ident::new(&s.value(), s.span());
                            let ty = unwrap_inner_type("Vec", &f.ty).unwrap();
                            return Some(quote! {
                                pub fn #arg_ident(&mut self, #arg_ident: #ty) -> &mut Self {
                                    if let Some(ref mut vals) = self.#name {
                                        vals.push(#arg_ident);
                                    } else {
                                        self.#name = Some(vec![#arg_ident]);
                                    }
                                    self
                                }
                            });
                        }
                        _ => panic!("Bad string ident"),
                    }
                }
            }
        }
        None
    });

    let methods = fields.iter().map(|f| {
        let inner_name = &f.ident;
        let inner_type = &f.ty;
        if let Some(inner_type) = unwrap_inner_type("Option", &f.ty) {
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
        if unwrap_inner_type("Option", &f.ty).is_some() {
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

            #(#extend_methods)*
            #(#methods)*

        }
    };

    TokenStream::from(quoted)
}
