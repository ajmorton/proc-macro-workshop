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

    fn builder_of(f: &syn::Field) -> Option<proc_macro2::Group> {
        for attr in &f.attrs {
            if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
                let mut stream = attr.tokens.clone().into_iter();
                if let Some(proc_macro2::TokenTree::Group(group)) = stream.next() {
                    return Some(group);
                }
            }
        }
        None
    }

    fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
        let name = f.ident.as_ref().unwrap();
        let group = builder_of(f)?;
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
                let method = quote! {
                    pub fn #arg_ident(&mut self, #arg_ident: #ty) -> &mut Self {
                        self.#name.push(#arg_ident);
                        self
                    }
                };
                Some((&arg_ident == name, method))
            }
            _ => panic!("Bad string ident"),
        }
    }

    let opt_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        let inner_type = &f.ty;
        if unwrap_inner_type("Option", &f.ty).is_some() || builder_of(&f).is_some() {
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

        let set_method = if let Some(inner_type) = unwrap_inner_type("Option", &f.ty) {
            quote! {
                pub fn #inner_name(&mut self, #inner_name: #inner_type) -> &mut Self {
                    self.#inner_name = Some(#inner_name);
                    self
                }
            }
        } else if builder_of(&f).is_some() {
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
        };

        match extend_method(&f) {
            None => set_method,
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => {
                quote! {
                    #set_method
                    #extend_method
                }
            }
        }
    });

    let nil_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        if builder_of(&f).is_some() {
            quote! {
                #inner_name: Vec::new()
            }
        } else {
            quote! {
                #inner_name: None
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let inner_name = &f.ident;
        if unwrap_inner_type("Option", &f.ty).is_some() || builder_of(f).is_some() {
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
