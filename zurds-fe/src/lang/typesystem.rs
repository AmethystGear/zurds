use std::{collections::HashMap, hash::Hash};

use crate::lang::lexer::Literal;

use super::parser::Expr;

use itertools::Itertools;

#[derive(Clone, PartialEq, Eq, Debug)]
enum Type {
    Variant(Vec<Type>),
    Container(ContainerType, ContainerProperties),
    Int,
    Flt,
    Bool,
    Str,
    Unit,
    Any,
}

impl Type {
    pub fn is_container(&self) -> bool {
        match &self {
            Type::Container(_, _) => true,
            _ => false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum ContainerType {
    List(Box<Type>),
    Maybe(Box<Type>),
    Dict(Dict),
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Dict {
    map: Box<Option<Type>>,
    structure: Option<HashMap<String, Type>>,
    maybe_structure: Option<HashMap<String, Type>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum ContainerProperties {
    // container and whatever it contains may not be changed.
    Immutable,
    // you can't add or remove elements from the container, but elements in the container may be changed.
    Fixed,
    // no restrictions on what can be changed.
    Mutable,
}

struct TypeError(String);

fn consolidate_types<I>(types: I) -> Option<Type>
where
    I: Iterator<Item = Type>,
{
    let mut types_deduped = vec![];
    for t in types {
        if !types_deduped.contains(&t) {
            types_deduped.push(t);
        }
    }
    let mut types = types_deduped;
    if types.len() <= 1 {
        return types.pop();
    }
    // `Any` is basically a poop, only there when we don't know where it ame from, e.g. 'x = []'
    // as soon as we start consolidating wit the poop. 2r3r3fbwith real types, we can ditch `Any`
    if types.iter().all(|x| x == &Type::Any) {
        return Some(Type::Any);
    }
    let types: Vec<_> = types.into_iter().filter(|x| x != &Type::Any).collect();
    // now we try to push down the variant as far as we can, this lets us leverage
    // the usefulness of container types.
    if (!types[0].is_container()) {}
    todo!()
}

fn get_concrete_fn_defs() -> [(&'static str, Vec<Type>, Type); 3] {
    [
        ("print", vec![Type::Str], Type::Unit),
        ("err", vec![Type::Str], Type::Unit),
        ("exit", vec![], Type::Unit),
    ]
}

fn get_concrete_method_defs() -> [(&'static str, Type, Vec<Type>, Type); 9] {
    [
        ("not", Type::Bool, vec![], Type::Bool),
        ("round", Type::Flt, vec![], Type::Flt),
        ("ciel", Type::Flt, vec![], Type::Flt),
        ("floor", Type::Flt, vec![], Type::Flt),
        ("flt", Type::Str, vec![], Type::Flt),
        ("flt?", Type::Str, vec![], Type::Bool),
        ("int", Type::Str, vec![], Type::Int),
        ("int?", Type::Str, vec![], Type::Bool),
        ("contains", Type::Str, vec![Type::Str], Type::Bool),
    ]
}

fn expect_args(args: &[Type], expected: &[Type]) -> Result<(), TypeError> {
    todo!()
}

fn resolve_method_type(
    t: Type,
    method: &str,
    args: Vec<Type>,
) -> Result<(Type, Option<Type>), TypeError> {
    use ContainerProperties::*;
    use ContainerType::*;
    match (&t, method) {
        (_, "str") => {
            expect_args(&args, &[])?;
            Ok((Type::Str, None))
        }
        (Type::Container(List(_), Mutable), "remove") => {
            expect_args(&args, &[Type::Int])?;
            Ok((Type::Unit, None))
        }
        (Type::Container(List(x), Mutable), "add") => {
            expect_args(&args, &[Type::Any])?;
            let new_list_type =
                consolidate_types([x.as_ref().clone(), args[0].clone()].into_iter());
            Ok((Type::Unit, new_list_type))
        }
        (Type::Container(Dict(_, _), _), "contains_key") => {
            expect_args(&args, &[Type::Str])?;
            Ok((Type::Bool, None))
        },
        (Type::Container(Dict(_, _), _), "keys") => {
            expect_args(&args, &[])?;
            Ok((Type::Container(List(Box::new(Type::Str)), Immutable), None))
        }
        (Type::Container(Dict(_, _), _), "remove") => {
            expect_args(&args, &[Type::Str])?;
            Ok((Type::Unit, None))
        }
        _ => {
            for (name, ty, expected_args, out) in get_concrete_method_defs() {
                if ty == t && name == method {
                    expect_args(&args, &expected_args)?;
                    return Ok((out, None));
                }
            }
            Err(TypeError(format!(
                "there is no function named {:?} for type {:?}",
                method, t
            )))
        },
    }
}

fn resolve_function_type(function: &str, args: Vec<Type>) -> Result<Type, TypeError> {
    for (name, expected_args, out) in get_concrete_fn_defs() {
        if name == function {
            expect_args(&args, &expected_args)?;
            return Ok(out)
        }
    }
    Err(TypeError(format!(
        "there is no function named {:?}",
        function,
    )))
}

pub fn get_expr_type(expr: &Expr, ctx: &HashMap<String, Type>) -> Result<Type, TypeError> {
    use ContainerProperties::*;
    use ContainerType::*;
    match expr {
        Expr::Literal(literal) => Ok(match literal {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Flt,
            Literal::Bool(_) => Type::Bool,
            Literal::String(_) => Type::Str,
        }),
        Expr::Call(function, args) => {
            let args: Result<Vec<Type>, _> =
                args.iter().map(|expr| get_expr_type(expr, ctx)).collect();
            resolve_function_type(function, args?)
        }
        Expr::List(vec) => {
            let types: Result<Vec<Type>, _> =
                vec.iter().map(|expr| get_expr_type(expr, ctx)).collect();
            let types = types?;
            Ok(Type::Container(
                List(Box::new(
                    consolidate_types(types.into_iter()).unwrap_or(Type::Any),
                )),
                Mutable,
            ))
        }
        Expr::Dict(vec) => {
            let literal_keys = vec.iter().filter_map(|(k, v)| match k {
                Expr::Literal(Literal::String(s)) => Some((s.clone(), v)),
                _ => None,
            });
            let mut structure_type = HashMap::new();
            for (key, expr) in literal_keys {
                structure_type.insert(key, get_expr_type(expr, ctx)?);
            }
            let expr_keys = vec
                .iter()
                .filter(|(k, _)| match k {
                    Expr::Literal(Literal::String(_)) => false,
                    _ => true,
                })
                .collect_vec();

            expr_keys.iter().map(|(key_expr, _)| {
                let key_type = get_expr_type(key_expr, ctx)?;
                if key_type == Type::Str {
                    Err(TypeError(format!("expected expression to evaluate to string in dictionary key, found {:?}", key_type)))
                } else {
                    Ok(())
                }
            }).collect::<Result<Vec<_>, _>>()?;

            let expr_vals: Result<Vec<_>, _> = expr_keys
                .iter()
                .map(|(_, val_expr)| get_expr_type(val_expr, ctx))
                .collect();

            let expr_vals = expr_vals?;
            let map_type = consolidate_types(expr_vals.into_iter());
            let structure_type = if !structure_type.is_empty() {
                Some(structure_type)
            } else {
                None
            };
            Ok(Type::Container(
                Dict(Box::new(map_type), structure_type),
                Mutable,
            ))
        }
        Expr::BinOp(expr, bin_op, expr1) => todo!(),
        Expr::Var(x) => ctx
            .get(x)
            .ok_or(TypeError("variable does not exist!".into()))
            .cloned(),
    }
}
