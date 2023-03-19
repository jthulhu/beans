use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Data, DeriveInput, Error,
    Fields, GenericParam, Generics, Result,
};

#[proc_macro_derive(Readable, attributes())]
pub fn derive_readable(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let generics = add_trait_bounds(input.generics.clone());
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let result =
        compute_self(&input).unwrap_or_else(|err| err.to_compile_error());
    let expanded = quote! {
    impl #impl_generics ::beans::typed::Readable for #name #ty_generics #where_clause {
        fn read(ast: ::beans::parser::AST) -> Self {
        let mut node = ::beans::node!(ast);
        #result
        }
    }
    };

    proc_macro::TokenStream::from(expanded)
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in generics.params.iter_mut() {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param
                .bounds
                .push(parse_quote!(::beans::typed::Readable));
        }
    }
    generics
}

fn compute_self(node: &DeriveInput) -> Result<TokenStream> {
    Ok(match &node.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! {
                    f.span() =>
                        #name: ::beans::typed::Readable::read(
                        ::beans::get!(node => #name)
                        )
                    }
                });
                quote! {
                    Self {
                    #(#recurse),*
                    }
                }
            }
            Fields::Unnamed(_) => todo!(),
            Fields::Unit => {
                quote! {
                    Self {}
                }
            }
        },
        Data::Enum(data) => {
            let recurse = data.variants.iter().map(|variant| {
                let name = &variant.ident;
                let constructor = match variant.fields {
                    Fields::Named(ref fields) => {
                        let recurse = fields.named.iter().map(|f| {
                            let name = &f.ident;
                            quote_spanned! {
                            f.span() =>
                                #name: ::beans::typed::Readable::read(
                                ::beans::get!(node => #name)
                                )
                            }
                        });
                        quote!({#(#recurse),*})
                    }
                    Fields::Unnamed(_) => todo!(),
                    Fields::Unit => quote!({}),
                };
                quote_spanned! {
                    variant.span() =>
                    Self::#name #constructor
                }
            });
            quote! {
            ::beans::match_variant! {(node) {
                #(#recurse),*
            }}
            }
        }
        Data::Union(_) => {
            return Err(Error::new_spanned(
                node,
                "Union as AST trees are not supported",
            ));
        }
    })
}
