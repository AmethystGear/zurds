use std::collections::HashMap;

use super::{
    lexer::{self, Literal},
    parser::{BinOp, Expr, Statement},
};

enum ValOrRef<'a> {
    Val(Val),
    Ref(&'a mut Val),
}

impl<'a> ValOrRef<'a> {
    fn resolve(&self) -> Val {
        match self {
            ValOrRef::Val(val) => val.clone(),
            ValOrRef::Ref(val) => (*val).clone(),
        }
    }
}

fn eval_expr<'a>(
    expr: &Expr,
    ctx: &'a mut HashMap<String, Val>,
) -> Result<ValOrRef<'a>, EvalError> {
    match expr {
        Expr::Literal(literal) => Ok(ValOrRef::Val(match literal {
            Literal::Int(i) => Val::Int(*i),
            Literal::Float(f) => Val::Float(*f),
            Literal::Bool(b) => Val::Bool(*b),
            Literal::String(s) => Val::String(s.clone()),
        })),
        Expr::Call(ident, vec) => {
            let args: Result<Vec<_>, _> = vec
                .iter()
                .map(|expr| eval_expr(expr, ctx).map(|x| x.resolve()))
                .collect();
            match (&ident[..], &args?[..]) {
                ("print", [Val::String(s)]) => {
                    web_sys::console::log_1(&s.into());
                    Ok(ValOrRef::Val(Val::Unit))
                }
                (fn_name, args) => Err(EvalError(format!(
                    "cannot invoke '{}' with args {:?}",
                    fn_name, args
                ))),
            }
        }
        Expr::List(vec) => {
            let elems: Result<Vec<_>, _> = vec
                .into_iter()
                .map(|expr| eval_expr(expr, ctx).map(|val| val.resolve()))
                .collect();
            Ok(ValOrRef::Val(Val::List(elems?)))
        }
        Expr::Dict(vec) => {
            let elems: Result<HashMap<_, _>, _> = vec
                .into_iter()
                .map(|(key, val)| {
                    let key =
                        if let Val::String(key) = eval_expr(key, ctx).map(|key| key.resolve())? {
                            key
                        } else {
                            return Err(EvalError("dictionary key is not a string".to_string()));
                        };
                    let val = eval_expr(val, ctx).map(|key| key.resolve())?;
                    Ok((key, val))
                })
                .collect();
            Ok(ValOrRef::Val(Val::Dict(elems?)))
        }
        Expr::BinOp(left, bin_op, right) => match bin_op {
            BinOp::Op(lexer::Op::Access) => match right.as_ref() {
                Expr::Call(ident, vec) => {
                    let args: Result<Vec<_>, _> = vec
                        .iter()
                        .map(|expr| eval_expr(expr, ctx).map(|x| x.resolve()))
                        .collect();
                    let left_val = eval_expr(left, ctx)?;
                    Ok(ValOrRef::Val(match (left_val, &ident[..], &args?[..]) {
                        (ValOrRef::Val(Val::Dict(dict)), "has_key", [Val::String(s)]) => {
                            Val::Bool(dict.contains_key(s))
                        }
                        (ValOrRef::Ref(Val::Dict(dict)), "has_key", [Val::String(s)]) => {
                            Val::Bool(dict.contains_key(s))
                        }
                        (ValOrRef::Val(Val::List(mut list)), "push", [val]) => {
                            list.push(val.clone());
                            Val::Unit
                        }
                        (ValOrRef::Ref(Val::List(list)), "push", [val]) => {
                            list.push(val.clone());
                            Val::Unit
                        }
                        (ValOrRef::Val(Val::Dict(mut dict)), "remove", [Val::String(s)]) => {
                            dict.remove(s);
                            Val::Unit
                        }
                        (ValOrRef::Ref(Val::Dict(dict)), "remove", [Val::String(s)]) => {
                            dict.remove(s);
                            Val::Unit
                        },
                        (ValOrRef::Val(Val::Dict(dict)), "keys", []) => {
                            Val::List(dict.keys().into_iter().map(|x| Val::String(x.clone())).collect())
                        }
                        (ValOrRef::Ref(Val::Dict(dict)), "keys", []) => {
                            Val::List(dict.keys().into_iter().map(|x| Val::String(x.clone())).collect())
                        }
                        (ValOrRef::Val(Val::String(s)), "has", [Val::String(sub)]) => {
                            Val::Bool(s.contains(sub))
                        }
                        (ValOrRef::Ref(Val::String(s)), "has", [Val::String(sub)]) => {
                            Val::Bool(s.contains(sub))
                        }
                        (val, "str", []) => Val::String(format!("{}", val.resolve())),
                        (_, fn_name, args) => Err(EvalError(format!(
                            "cannot invoke '{}' with args {:?}",
                            fn_name, args
                        )))?,
                    }))
                }
                Expr::List(vec) => {
                    let index = match (vec.get(0), vec.get(1)) {
                        (Some(expr), None) => Ok(expr),
                        _ => Err(EvalError(format!(
                            "can't index like .[x, y], you must index like .[x]"
                        ))),
                    }?;
                    let index = eval_expr(index, ctx)?.resolve();
                    let left_val = eval_expr(left, ctx)?;
                    let out_of_bounds = || EvalError("list access out of bounds".to_string());
                    match (left_val, index) {
                        (ValOrRef::Val(Val::List(list)), Val::Int(i)) => {
                            let i = usize::try_from(i).map_err(|_| out_of_bounds())?;
                            list.get(i)
                                .ok_or_else(out_of_bounds)
                                .map(|x| ValOrRef::Val(x.clone()))
                        }
                        (ValOrRef::Ref(Val::List(list)), Val::Int(i)) => {
                            let i = usize::try_from(i).map_err(|_| out_of_bounds())?;
                            list.get_mut(i)
                                .ok_or_else(out_of_bounds)
                                .map(|x| ValOrRef::Ref(x))
                        }
                        (ValOrRef::Val(Val::Dict(dict)), Val::String(s)) => dict
                            .get(&s)
                            .map(|x| ValOrRef::Val(x.clone()))
                            .ok_or(EvalError(format!(
                                "key {} does not exist in dict {:?}",
                                s, dict
                            ))),
                        (ValOrRef::Ref(Val::Dict(dict)), Val::String(s)) => {
                            let err =
                                EvalError(format!("key {} does not exist in dict {:?}", s, dict));
                            dict.get_mut(&s).map(|x| ValOrRef::Ref(x)).ok_or(err)
                        }
                        (into, index) => Err(EvalError(format!(
                            "can't index with {:?} into {:?}",
                            index,
                            into.resolve()
                        ))),
                    }
                }
                Expr::Var(ident) => {
                    let left_val = eval_expr(left, ctx)?;
                    match left_val {
                        ValOrRef::Val(Val::Dict(dict)) => dict
                            .get(ident)
                            .map(|x| ValOrRef::Val(x.clone()))
                            .ok_or(EvalError(format!(
                                "key {} does not exist in dict {:?}",
                                ident, dict
                            ))),
                        ValOrRef::Ref(Val::Dict(dict)) => {
                            let err = EvalError(format!(
                                "key {} does not exist in dict {:?}",
                                ident, dict
                            ));
                            dict.get_mut(ident).map(|x| ValOrRef::Ref(x)).ok_or(err)
                        }
                        x => Err(EvalError(format!(
                            "can't access field {} of {:?}",
                            ident,
                            x.resolve()
                        ))),
                    }
                }
                _ => {
                    let left_val = eval_expr(left, ctx)?;
                    Err(EvalError(format!(
                        "can't access property {:?} of {:?}",
                        right,
                        left_val.resolve()
                    )))
                }
            },
            op => handle_operator(
                &eval_expr(left, ctx)?.resolve(),
                &op,
                &eval_expr(right, ctx)?.resolve(),
            )
            .map(|x| ValOrRef::Val(x)),
        },
        Expr::SingleOp(single_op, expr) => match single_op {
            super::parser::SingleOp::Not => {
                let val = eval_expr(expr, ctx)?.resolve();
                if let Val::Bool(b) = val {
                    Ok(ValOrRef::Val(Val::Bool(!b)))
                } else {
                    Err(EvalError(format!("can't `not` a {:?}", expr)))
                }
            }
        },
        Expr::Var(ident) => Ok(ValOrRef::Ref(
            ctx.get_mut(ident)
                .ok_or(EvalError(format!("variable {} does not exist", ident)))?,
        )),
    }
}

#[derive(Debug)]
pub enum ControlFlow {
    Continue,
    Break,
    Error(EvalError),
}

enum Path<'a> {
    Str(String, &'a mut HashMap<String, Val>),
    Int(usize, &'a mut Vec<Val>),
}

impl<'a> Path<'a> {
    fn resolve(&self) -> Option<&Val> {
        match self {
            Path::Str(s, map) => map.get(s).clone(),
            Path::Int(i, vec) => vec.get(*i).clone(),
        }
    }
}

pub fn eval(program: &Vec<Statement>, ctx: &mut HashMap<String, Val>) -> Result<(), ControlFlow> {
    for statement in program {
        match statement {
            Statement::If(expr, true_case, false_case) => {
                let condition = eval_expr(expr, ctx)
                    .map_err(|e| ControlFlow::Error(e))?
                    .resolve();
                if let Val::Bool(condition) = condition {
                    if condition {
                        eval(true_case, ctx)?;
                    } else {
                        eval(false_case, ctx)?;
                    }
                } else {
                    Err(ControlFlow::Error(EvalError(format!(
                        "expected true/false, found {:?}",
                        condition
                    ))))?;
                }
            }
            Statement::For(var, expr, body) => {
                let iterable = eval_expr(expr, ctx)
                    .map_err(|e| ControlFlow::Error(e))?
                    .resolve();
                let values: Box<dyn Iterator<Item = Val>> = match iterable {
                    Val::List(elems) => Box::new(elems.into_iter()),
                    Val::Range(min, max) => Box::new((min..max).into_iter().map(|i| Val::Int(i))),
                    _ => Err(ControlFlow::Error(EvalError(format!(
                        "can't iterate over {:?}",
                        iterable
                    ))))?,
                };
                for val in values {
                    ctx.insert(var.clone(), val);
                    match eval(body, ctx) {
                        Err(ControlFlow::Break) => break,
                        Err(ControlFlow::Error(e)) => return Err(ControlFlow::Error(e)),
                        Err(ControlFlow::Continue) | Ok(()) => {}
                    }
                }
            }
            Statement::Assign(assign, op, expr) => {
                let new = eval_expr(expr, ctx)
                    .map_err(|e| ControlFlow::Error(e))?
                    .resolve();
                let current = match assign {
                    super::parser::Assign::Var(ident) => Path::Str(ident.clone(), ctx),
                    super::parser::Assign::Field(left, field) => {
                        let mut ctx_copy = ctx.clone();
                        let left = eval_expr(left, ctx).map_err(|e| ControlFlow::Error(e))?;
                        match field {
                            super::parser::Field::Var(var) => {
                                if let ValOrRef::Ref(Val::Dict(val)) = left {
                                    Path::Str(var.clone(), val)
                                } else {
                                    return Err(ControlFlow::Error(EvalError(
                                        "invalid assignment".to_string(),
                                    )));
                                }
                            }
                            super::parser::Field::Index(index) => {
                                let index = eval_expr(index, &mut ctx_copy)
                                    .map_err(|e| ControlFlow::Error(e))?
                                    .resolve();
                                match (left, index) {
                                    (ValOrRef::Ref(Val::Dict(val)), Val::String(index)) => {
                                        Path::Str(index.clone(), val)
                                    }
                                    (ValOrRef::Ref(Val::List(val)), Val::Int(index)) => {
                                        let out_of_bounds = ControlFlow::Error(EvalError(
                                            "list access out of bounds".to_string(),
                                        ));
                                        let index =
                                            usize::try_from(index).map_err(|_| out_of_bounds)?;
                                        Path::Int(index, val)
                                    }
                                    _ => {
                                        return Err(ControlFlow::Error(EvalError(
                                            "invalid assignment".to_string(),
                                        )))
                                    }
                                }
                            }
                        }
                    }
                };
                let new = match (current.resolve(), op) {
                    (Some(current), Some(x)) => match x {
                        lexer::AssignOp::Add => handle_operator(current, &BinOp::Op(lexer::Op::Add), &new),
                        lexer::AssignOp::Sub => handle_operator(current, &BinOp::Op(lexer::Op::Sub), &new),
                        lexer::AssignOp::Mul => handle_operator(current, &BinOp::Op(lexer::Op::Mul), &new),
                        lexer::AssignOp::Div => handle_operator(current, &BinOp::Op(lexer::Op::Div), &new),
                    }.map_err(|e| ControlFlow::Error(e))?,
                    (_, None) => new,
                    (None, Some(_)) => return Err(ControlFlow::Error(EvalError("can't do +=,-=,*=,/= because the variable or field wasn't previously defined".to_string()))),
                };
                match current {
                    Path::Str(key, map) => {
                        map.insert(key, new);
                    }
                    Path::Int(key, map) => {
                        if key < map.len() {
                            map[key] = new;
                        } else {
                            return Err(ControlFlow::Error(EvalError(
                                "list access out of bounds".to_string(),
                            )));
                        }
                    }
                }
            }
            Statement::Expr(expr) => {
                eval_expr(expr, ctx).map_err(|e| ControlFlow::Error(e))?;
            }
            Statement::Continue => return Err(ControlFlow::Continue),
            Statement::Break => return Err(ControlFlow::Break),
            Statement::Pass => {}
            Statement::Loop(body) => loop {
                match eval(body, ctx) {
                    Err(ControlFlow::Break) => break,
                    Err(ControlFlow::Error(e)) => return Err(ControlFlow::Error(e)),
                    Err(ControlFlow::Continue) | Ok(()) => {}
                }
            },
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Unit,
    Int(i64),
    Bool(bool),
    Float(f64),
    String(String),
    List(Vec<Val>),
    Range(i64, i64),
    Dict(HashMap<String, Val>),
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Val::Unit => write!(f, "()"),
            Val::Int(value) => write!(f, "{}", value),
            Val::Bool(value) => write!(f, "{}", value),
            Val::Float(value) => write!(f, "{}", value),
            Val::String(value) => write!(f, "\"{}\"", value),
            Val::List(values) => {
                let formatted_values: Vec<String> =
                    values.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", formatted_values.join(", "))
            }
            Val::Range(start, end) => write!(f, "({}..{})", start, end),
            Val::Dict(map) => {
                let formatted_entries: Vec<String> = map
                    .iter()
                    .map(|(key, value)| format!("\"{}\": {}", key, value))
                    .collect();
                write!(f, "{{{}}}", formatted_entries.join(", "))
            }
        }
    }
}

fn handle_operator(a: &Val, op: &super::parser::BinOp, b: &Val) -> Result<Val, EvalError> {
    use super::lexer::Op::*;
    use super::parser::BinOp::*;
    Ok(match (a, op, b) {
        (Val::Int(a), Op(Add), Val::Int(b)) => Val::Int(a + b),
        (Val::Int(a), Op(Sub), Val::Int(b)) => Val::Int(a - b),
        (Val::Int(a), Op(Mul), Val::Int(b)) => Val::Int(a * b),
        (Val::Int(a), Op(Div), Val::Int(b)) => Val::Int(a / b),
        (Val::Float(a), Op(Add), Val::Float(b)) => Val::Float(a + b),
        (Val::Float(a), Op(Sub), Val::Float(b)) => Val::Float(a - b),
        (Val::Float(a), Op(Mul), Val::Float(b)) => Val::Float(a * b),
        (Val::Float(a), Op(Div), Val::Float(b)) => Val::Float(a / b),
        (Val::String(a), Op(Add), Val::String(b)) => Val::String(a.to_owned() + b),
        (Val::Bool(a), And, Val::Bool(b)) => Val::Bool(*a && *b),
        (Val::Bool(a), Or, Val::Bool(b)) => Val::Bool(*a || *b),
        (Val::Int(a), Op(Range), Val::Int(b)) => Val::Range(*a, *b),
        (left, Op(Eq), right) => Val::Bool(left == right),
        (Val::List(a), Op(Add), Val::List(b)) => {
            Val::List(a.clone().into_iter().chain(b.clone().into_iter()).collect())
        }
        (a, op, b) => Err(EvalError(format!(
            "can't do operation {:?} between {:?} and {:?}",
            op, a, b
        )))?,
    })
}

#[derive(Debug)]
struct EvalError(String);
