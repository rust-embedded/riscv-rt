extern crate proc_macro;
extern crate quote;
extern crate syn;

use std::collections::HashSet;

use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{
    parse, parse_macro_input, spanned::Spanned, AttrStyle, Attribute, Item, ItemFn, ItemStatic,
    PathArguments, ReturnType, Stmt, Type, Visibility,
};

#[proc_macro_attribute]
pub fn entry(args: TokenStream, input: TokenStream) -> TokenStream {
    let f = parse_macro_input!(input as ItemFn);

    // check the function signature
    let valid_signature = f.constness.is_none()
        && f.vis == Visibility::Inherited
        && f.abi.is_none()
        && f.decl.inputs.is_empty()
        && f.decl.generics.params.is_empty()
        && f.decl.generics.where_clause.is_none()
        && f.decl.variadic.is_none()
        && match f.decl.output {
            ReturnType::Default => false,
            ReturnType::Type(_, ref ty) => match **ty {
                Type::Never(_) => true,
                _ => false,
            },
        };

    if !valid_signature {
        return parse::Error::new(
            f.span(),
            "`#[entry]` function must have signature `[unsafe] fn() -> !`",
        )
        .to_compile_error()
        .into();
    }

    if !args.is_empty() {
        return parse::Error::new(
            Span::call_site().into(),
            "This attribute accepts no arguments",
        )
        .to_compile_error()
        .into();
    }

    // XXX should we blacklist other attributes?
    let attrs = f.attrs;
    let unsafety = f.unsafety;
    let ident = f.ident;
    let (statics, stmts) = match extract_static_muts(f.block.stmts) {
        Err(e) => return e.to_compile_error().into(),
        Ok(x) => x,
    };

    let vars = statics
        .into_iter()
        .map(|var| {
            let (ref cfgs, ref attrs) = extract_cfgs(var.attrs);
            let ident = var.ident;
            let ty = var.ty;
            let expr = var.expr;

            quote!(
                #[allow(non_snake_case)]
                #(#cfgs)*
                let #ident: &'static mut #ty = unsafe {
                    #(#attrs)*
                    #(#cfgs)*
                    static mut #ident: #ty = #expr;

                    &mut #ident
                };
            )
        })
        .collect::<Vec<_>>();

    quote!(
        const #ident: () = {
            #[no_mangle]
            #(#attrs)*
            #unsafety extern "C" fn main() -> ! {
                #(#vars)*

                #(#stmts)*
            }

            main()
        };
    )
    .into()
}

/// Extracts `static mut` vars from the beginning of the given statements
fn extract_static_muts(stmts: Vec<Stmt>) -> Result<(Vec<ItemStatic>, Vec<Stmt>), parse::Error> {
    let mut istmts = stmts.into_iter();

    let mut seen = HashSet::new();
    let mut statics = vec![];
    let mut stmts = vec![];
    while let Some(stmt) = istmts.next() {
        match stmt {
            Stmt::Item(Item::Static(var)) => {
                if var.mutability.is_some() {
                    if seen.contains(&var.ident) {
                        return Err(parse::Error::new(
                            var.ident.span(),
                            format!("the name `{}` is defined multiple times", var.ident),
                        ));
                    }

                    seen.insert(var.ident.clone());
                    statics.push(var);
                } else {
                    stmts.push(Stmt::Item(Item::Static(var)));
                }
            }
            _ => {
                stmts.push(stmt);
                break;
            }
        }
    }

    stmts.extend(istmts);

    Ok((statics, stmts))
}

fn extract_cfgs(attrs: Vec<Attribute>) -> (Vec<Attribute>, Vec<Attribute>) {
    let mut cfgs = vec![];
    let mut not_cfgs = vec![];

    for attr in attrs {
        if eq(&attr, "cfg") {
            cfgs.push(attr);
        } else {
            not_cfgs.push(attr);
        }
    }

    (cfgs, not_cfgs)
}

/// Returns `true` if `attr.path` matches `name`
fn eq(attr: &Attribute, name: &str) -> bool {
    attr.style == AttrStyle::Outer && attr.path.segments.len() == 1 && {
        let pair = attr.path.segments.first().unwrap();
        let segment = pair.value();
        segment.arguments == PathArguments::None && segment.ident.to_string() == name
    }
}
