extern crate proc_macro;
use proc_macro::TokenStream;

use syn::{
    parse_macro_input,
    DeriveInput,
    Ident,
};
use quote::quote;


#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let tree = parse_macro_input!(input as DeriveInput);
    let ident = &tree.ident;
    let ident_builder = Ident::new(&format!("{}Builder", ident), ident.span());

    // starting code gen
    // -----------------
    let quoted = quote! {
        pub struct #ident_builder {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl #ident {
            pub fn builder() -> #ident_builder {
                 #ident_builder {
                     executable: None,
                     args: None,
                     env: None,
                     current_dir: None,
                 }
            }
        }

        impl #ident_builder {
            fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = Some(executable);
                self
            }
            fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args= Some(args);
                self
            }
            fn env(&mut self, env: Vec<String>) -> &mut Self {
                self.env= Some(env);
                self
            }
            fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir= Some(current_dir);
                self
            }
        }
    };
    // ---------------
    // ending code gen

    TokenStream::from(quoted)
}
