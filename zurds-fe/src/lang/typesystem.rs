use std::collections::HashMap;

use crate::lang::{
    lexer::{self, Literal},
    parser::BinOp,
};

use super::parser::Expr;

#[derive(Clone, PartialEq, Eq, Debug)]
enum Type {
    Variant(Vec<Type>),
    Container(Container),
    Int,
    Flt,
    Bool,
    Str,
    Unit,
    Any,
    Range,
}

impl Type {
    pub fn is_container(&self) -> bool {
        match &self {
            Type::Container(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum Container {
    Map(Box<Type>),   // map from string -> type, where we don't know any of the keys
    List(Box<Type>),  // list of type
    Maybe(Box<Type>), // either has a type or doesn't
    Struct {
        known_fields: HashMap<String, Type>, // fields known to be present
        maybe_fields: HashMap<String, Type>, // fields that may or may not be present
    },
    // type of a `{}`, because we don't know if it's a map or a struct.
    // once things get added to it, the type will change.
    EmptyMapOrStruct,
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
    if types.iter().all(|x| x == &Type::Any) {
        return Some(Type::Any);
    }
    let types: Vec<_> = types.into_iter().filter(|x| x != &Type::Any).collect();
    // now we try to push down the variant as far as we can, this lets us leverage
    // the usefulness of container types.
    if (!types[0].is_container()) {}
    todo!()
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
    args_exprs: &Vec<Expr>,
    ctx: &mut HashMap<String, Type>,
) -> Result<(Type, Option<Type>), TypeError> {
    let args: Result<Vec<Type>, _> = args_exprs
        .iter()
        .map(|expr| get_expr_type(expr, ctx))
        .collect();
    let args = args?;

    use Container::*;
    match (&t, method) {
        (_, "str") => {
            expect_args(&args, &[])?;
            Ok((Type::Str, None))
        }
        (Type::Container(List(_)), "remove") => {
            expect_args(&args, &[Type::Int])?;
            Ok((Type::Unit, None))
        }
        (Type::Container(List(x)), "add") => {
            expect_args(&args, &[Type::Any])?;
            let new_list_type =
                consolidate_types([x.as_ref().clone(), args[0].clone()].into_iter());
            Ok((Type::Unit, new_list_type))
        }
        (Type::Container(Map(_)), "contains_key") => {
            expect_args(&args, &[Type::Str])?;
            Ok((Type::Bool, None))
        }
        (Type::Container(Map(_)), "keys") => {
            expect_args(&args, &[])?;
            Ok((Type::Container(List(Box::new(Type::Str))), None))
        }
        (Type::Container(Map(_)), "remove") => {
            expect_args(&args, &[Type::Str])?;
            Ok((Type::Unit, None))
        }
        (Type::Container(Map(t)), "get") => {
            expect_args(&args, &[Type::Str])?;
            Ok((*t.clone(), None))
        }
        (Type::Container(Map(x)), "insert") => {
            expect_args(&args, &[Type::Str, Type::Any])?;
            let new_map_type = consolidate_types([x.as_ref().clone(), args[0].clone()].into_iter());
            Ok((Type::Unit, new_map_type))
        }
        (
            Type::Container(Struct {
                known_fields,
                maybe_fields,
            }),
            "insert",
        ) => {
            expect_args(&args, &[Type::Str, Type::Any])?;
            let field = match &args_exprs[0] {
                Expr::Literal(Literal::String(s)) => Ok(s),
                _ => Err(TypeError(
                    "when inserting fields into a structure, you must use static strings.".into(),
                )),
            }?;

            let mut known_fields = known_fields.clone();
            let mut maybe_fields = maybe_fields.clone();
            if maybe_fields.contains_key(field) {
                maybe_fields.remove(field);
            }
            known_fields.insert(field.clone(), args[1].clone());

            Ok((
                Type::Unit,
                Some(Type::Container(Struct {
                    known_fields,
                    maybe_fields,
                })),
            ))
        }
        (
            Type::Container(Struct {
                known_fields,
                maybe_fields,
            }),
            "remove",
        ) => {
            expect_args(&args, &[Type::Str])?;
            let field = match &args_exprs[0] {
                Expr::Literal(Literal::String(s)) => Ok(s),
                _ => Err(TypeError(
                    "when removing fields from a structure, you must use static strings.".into(),
                )),
            }?;

            let mut known_fields = known_fields.clone();
            let mut maybe_fields = maybe_fields.clone();
            if known_fields.contains_key(field) {
                known_fields.remove(field);
            } else if maybe_fields.contains_key(field) {
                maybe_fields.remove(field);
            } else {
                Err(TypeError(format!(
                    "structure does not have field `{}`",
                    field
                )))?;
            }

            Ok((
                Type::Unit,
                Some(Type::Container(Struct {
                    known_fields,
                    maybe_fields,
                })),
            ))
        }
        (Type::Container(EmptyMapOrStruct), "insert") => {
            expect_args(&args, &[Type::Str, Type::Any])?;
            let field = match &args_exprs[0] {
                Expr::Literal(Literal::String(s)) => Some(s),
                _ => None,
            };

            if let Some(field) = field {
                let mut known_fields = HashMap::new();
                known_fields.insert(field.clone(), args[1].clone());
                Ok((
                    Type::Unit,
                    Some(Type::Container(Struct {
                        known_fields,
                        maybe_fields: HashMap::new(),
                    })),
                ))
            } else {
                Ok((
                    Type::Unit,
                    Some(Type::Container(Map(Box::new(args[1].clone())))),
                ))
            }
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
        }
    }
}

fn get_concrete_fn_defs() -> [(&'static str, Vec<Type>, Type); 3] {
    [
        ("print", vec![Type::Str], Type::Unit),
        ("err", vec![Type::Str], Type::Unit),
        ("exit", vec![], Type::Unit),
    ]
}

fn resolve_function_type(function: &str, args: Vec<Type>) -> Result<Type, TypeError> {
    for (name, expected_args, out) in get_concrete_fn_defs() {
        if name == function {
            expect_args(&args, &expected_args)?;
            return Ok(out);
        }
    }
    Err(TypeError(format!(
        "there is no function named {:?}",
        function,
    )))
}

pub fn get_expr_type(expr: &Expr, ctx: &mut HashMap<String, Type>) -> Result<Type, TypeError> {
    use Container::*;
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
            Ok(Type::Container(List(Box::new(
                consolidate_types(types.into_iter()).unwrap_or(Type::Any),
            ))))
        }
        Expr::Dict(vec) => {
            if vec.is_empty() {
                return Ok(Type::Container(EmptyMapOrStruct));
            }
            let literal_keys = vec.iter().all(|(k, _)| match k {
                Expr::Literal(Literal::String(_)) => true,
                _ => false,
            });
            if literal_keys {
                let literal_keys = vec.iter().filter_map(|(k, v)| match k {
                    Expr::Literal(Literal::String(s)) => Some((s, v)),
                    _ => None,
                });
                let mut known_fields = HashMap::new();
                for (key, expr) in literal_keys {
                    known_fields.insert(key.clone(), get_expr_type(expr, ctx)?);
                }
                Ok(Type::Container(Struct {
                    known_fields,
                    maybe_fields: HashMap::new(),
                }))
            } else {
                let kvs = vec
                    .iter()
                    .map(|(k, v)| (get_expr_type(k, ctx), get_expr_type(v, ctx)));
                let mut values = vec![];
                for (k, v) in kvs {
                    let k = k?;
                    let v = v?;
                    if k != Type::Str {
                        Err(TypeError(format!(
                            "expected string for map key, found {:?}",
                            k
                        )))?;
                    }
                    values.push(v);
                }
                Ok(Type::Container(Map(Box::new(
                    consolidate_types(values.into_iter()).expect("bug: map is empty"),
                ))))
            }
        }
        Expr::BinOp(left, binop, right) => match binop {
            BinOp::Op(lexer::Op::Access) => match right.as_ref() {
                Expr::Call(method, args) => {
                    let left_type = get_expr_type(left, ctx)?;
                    let (out, new_left_type) = resolve_method_type(left_type, method, args, ctx)?;
                    if let Some(new_left_type) = new_left_type {
                        update_expr_type_in_context(left, new_left_type, ctx);
                    }
                    Ok(out)
                }
                Expr::Var(field) => {
                    let left_type = get_expr_type(left, ctx)?;
                    match &left_type {
                        Type::Container(Struct {
                            known_fields,
                            maybe_fields,
                        }) => {
                            if let Some(entry) = known_fields.get(field) {
                                Ok(entry.clone())
                            } else if let Some(entry) = maybe_fields.get(field) {
                                Ok(Type::Container(Maybe(Box::new(entry.clone()))))
                            } else {
                                Err(TypeError(format!(
                                    "can't access field `{}` of {:?}",
                                    field, left_type
                                )))
                            }
                        }
                        _ => Err(TypeError(format!(
                            "can't access field `{}` of {:?}",
                            field, left_type
                        ))),
                    }
                }
                Expr::List(vec) => {
                    let index = if let Some(index) = vec.get(0) {
                        index
                    } else {
                        return Err(TypeError("invalid indexing".into()));
                    };
                    let left_type = get_expr_type(left, ctx)?;
                    let indexing_type = get_expr_type(index, ctx)?;
                    match (&left_type, &indexing_type) {
                        (Type::Container(List(val)), Type::Int) => Ok(val.as_ref().clone()),
                        (Type::Container(Map(val)), Type::Str) => Ok(val.as_ref().clone()),
                        _ => Err(TypeError("invalid indexing".into())),
                    }
                }
                _ => Err(TypeError("invalid property access".into())),
            },
            op => handle_operator(get_expr_type(left, ctx)?, op, get_expr_type(right, ctx)?),
        },
        Expr::Var(x) => ctx
            .get(x)
            .ok_or(TypeError("variable does not exist!".into()))
            .cloned(),
    }
}

fn update_expr_type_in_context(expr: &Expr, expr_type: Type, ctx: &mut HashMap<String, Type>) {
    // updating the expresion's type in the context only matters when we have a direct sequence of acceses
    // from a variable stored in the context.
    // i.e. if we have x.y.remove('field'), then we need to update the type of 'x'.
    // but if we have (x + [1, 2, 3]).add('hello world'), we wouldn't need to update the type of 'x'
    enum Path {
        Field(String),
        Any,
    }

    let mut expr = expr;
    let mut path = vec![];
    let root;
    loop {
        match expr {
            Expr::Var(var) => {
                root = Some(var);
                break;
            }
            Expr::BinOp(left, BinOp::Op(lexer::Op::Access), right) => {
                expr = left;
                match right.as_ref() {
                    Expr::List(_) => path.push(Path::Any),
                    Expr::Var(field) => path.push(Path::Field(field.clone())),
                    _ => unreachable!(),
                }
            }
            _ => {
                root = None;
                break;
            }
        }
    }
    if let Some(root) = root {
        let mut ty = ctx.remove(root).unwrap();
        let mut curr = &mut ty;
        for part in path {
            match (part, curr) {
                (
                    Path::Field(field),
                    Type::Container(Container::Struct {
                        known_fields,
                        maybe_fields,
                    }),
                ) => {
                    if let Some(entry) = known_fields.get_mut(&field) {
                        curr = entry;
                    } else if let Some(entry) = maybe_fields.get_mut(&field) {
                        curr = entry;
                    } else {
                        unreachable!()
                    }
                }
                (Path::Any, Type::Container(Container::Map(val) | Container::List(val))) => {
                    curr = val.as_mut();
                }
                _ => unreachable!(),
            };
        }
        *curr = expr_type;
        ctx.insert(root.clone(), ty);
    }
}

fn handle_operator(left: Type, op: &BinOp, right: Type) -> Result<Type, TypeError> {
    use super::lexer::Op::*;
    use super::parser::BinOp::*;
    Ok(match (left, op, right) {
        (Type::Int, Op(Add), Type::Int) => Type::Int,
        (Type::Int, Op(Sub), Type::Int) => Type::Int,
        (Type::Int, Op(Mul), Type::Int) => Type::Int,
        (Type::Int, Op(Div), Type::Int) => Type::Int,

        (Type::Flt, Op(Add), Type::Flt) => Type::Flt,
        (Type::Flt, Op(Sub), Type::Flt) => Type::Flt,
        (Type::Flt, Op(Mul), Type::Flt) => Type::Flt,
        (Type::Flt, Op(Div), Type::Flt) => Type::Flt,

        (Type::Str, Op(Add), Type::Str) => Type::Str,
        (Type::Container(Container::List(a)), Op(Add), Type::Container(Container::List(b))) => {
            Type::Container(Container::List(Box::new(
                consolidate_types([a, b].map(|x| x.as_ref().clone()).into_iter()).unwrap(),
            )))
        }

        (Type::Bool, And, Type::Bool) => Type::Bool,
        (Type::Bool, Or, Type::Bool) => Type::Bool,

        (Type::Int, Op(Range), Type::Int) => Type::Range,
        (x, Op(Eq), y) if x == y => Type::Bool,
        (a, op, b) => Err(TypeError(format!(
            "can't do operation {:?} between {:?} and {:?}",
            op, a, b
        )))?,
    })
}
