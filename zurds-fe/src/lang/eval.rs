use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use serde_json::json;

use super::{
    lexer::{self, AssignOp, Literal},
    parser::{BinOp, Expr, Statement},
};

pub fn eval_expr<'a>(
    expr: &Expr,
    ctx: &'a mut HashMap<String, Rc<RefCell<Val>>>,
) -> Result<Rc<RefCell<Val>>, ControlFlow> {
    match expr {
        Expr::Literal(literal) => Ok(Rc::new(RefCell::new(match literal {
            Literal::Int(i) => Val::Int(*i),
            Literal::Float(f) => Val::Float(*f),
            Literal::Bool(b) => Val::Bool(*b),
            Literal::String(s) => Val::String(s.clone()),
        }))),
        Expr::Call(ident, vec) => {
            let args: Result<Vec<_>, _> = vec.iter().map(|expr| eval_expr(expr, ctx)).collect();
            let args: Vec<_> = args?.into_iter().map(|x| x.borrow().clone()).collect();
            Ok(Rc::new(RefCell::new(match (&ident[..], &args[..]) {
                ("print", [Val::String(s)]) => {
                    web_sys::console::log_1(&s.into());
                    Ok(Val::Unit)
                }
                ("exit", []) => Err(ControlFlow::Exit),
                ("err", [Val::String(s)]) => {
                    Err(ControlFlow::Error(EvalError::Custom(s.to_string())))
                }
                ("not", [Val::Bool(b)]) => Ok(Val::Bool(!b)),
                (fn_name, args) => Err(ControlFlow::Error(EvalError::Program(format!(
                    "cannot invoke function '{}' with arguments {:?}",
                    fn_name, args
                )))),
            }?)))
        }
        Expr::List(vec) => {
            let elems: Result<Vec<_>, _> =
                vec.into_iter().map(|expr| eval_expr(expr, ctx)).collect();
            Ok(Rc::new(RefCell::new(Val::List(elems?))))
        }
        Expr::Dict(vec) => {
            let elems: Result<HashMap<_, _>, _> = vec
                .into_iter()
                .map(|(key, val)| {
                    let key = if let Val::String(key) = eval_expr(key, ctx)?.borrow().clone() {
                        key
                    } else {
                        return Err(ControlFlow::Error(EvalError::Program(
                            "dictionary key is not a string".to_string(),
                        )));
                    };
                    let val = eval_expr(val, ctx)?;
                    Ok((key, val))
                })
                .collect();
            Ok(Rc::new(RefCell::new(Val::Dict(elems?))))
        }
        Expr::BinOp(left, bin_op, right) => match bin_op {
            BinOp::Op(lexer::Op::Access) => match right.as_ref() {
                Expr::Call(ident, vec) => {
                    let args: Result<Vec<_>, _> =
                        vec.iter().map(|expr| eval_expr(expr, ctx)).collect();
                    let args = args?;
                    let args: Vec<Ref<'_, Val>> = args.iter().map(|x| x.borrow()).collect();
                    let args: Vec<&Val> = args.iter().map(|x| x.deref()).collect();

                    let left = eval_expr(left, ctx)?;
                    let mut left = left.borrow_mut();
                    let left_val = left.deref_mut();
                    Ok(Rc::new(RefCell::new(
                        match (left_val, &ident[..], &args[..]) {
                            (Val::Dict(dict), "contains_key", [Val::String(s)]) => {
                                Val::Bool(dict.contains_key(s))
                            }
                            (Val::Dict(dict), "remove", [Val::String(s)]) => {
                                dict.remove(s);
                                Val::Unit
                            }
                            (Val::Dict(dict), "keys", []) => Val::List(
                                dict.keys()
                                    .into_iter()
                                    .map(|x| Val::String(x.clone()))
                                    .map(|x| Rc::new(RefCell::new(x)))
                                    .collect(),
                            ),
                            (Val::Dict(dict), "get", [Val::String(s)]) => {
                                Val::Opt(dict.get(s).cloned())
                            }
                            (Val::Opt(opt), "is_some", []) => Val::Bool(opt.is_some()),
                            (Val::Opt(opt), "is_none", []) => Val::Bool(opt.is_none()),
                            (Val::Opt(opt), "unwrap", []) => {
                                if let Some(opt) = opt {
                                    return Ok(opt.clone());
                                } else {
                                    return Err(ControlFlow::Error(EvalError::Program(
                                        "tried to unwrap a maybe value, found nothing".into(),
                                    )));
                                }
                            }
                            (Val::Opt(opt), "unwrap", [Val::String(s)]) => {
                                if let Some(opt) = opt {
                                    return Ok(opt.clone());
                                } else {
                                    return Err(ControlFlow::Error(EvalError::Program(s.into())));
                                }
                            }
                            (Val::Res(res), "is_ok", []) => Val::Bool(res.is_ok()),
                            (Val::Res(res), "is_err", []) => Val::Bool(res.is_err()),
                            (Val::Res(res), "unwrap", []) => match res {
                                Ok(ok) => return Ok(ok.clone()),
                                Err(e) => {
                                    return Err(ControlFlow::Error(EvalError::Program(format!(
                                        "error: {:?}",
                                        e
                                    ))))
                                }
                            },
                            (Val::Res(res), "unwrap_err", []) => match res {
                                Ok(ok) => {
                                    return Err(ControlFlow::Error(EvalError::Program(format!(
                                        "expected error value, but was ok: {:?}",
                                        ok
                                    ))))
                                }
                                Err(e) => return Ok(e.clone()),
                            },
                            (Val::List(list), "add", [val]) => {
                                list.push(Rc::new(RefCell::new((*val).clone())));
                                Val::Unit
                            }
                            (Val::List(list), "len", []) => Val::Int(list.len() as i64),
                            (Val::List(list), "remove", [Val::Int(i)]) => {
                                let out_of_bounds = || {
                                    ControlFlow::Error(EvalError::Program(
                                        "index out of bounds".to_string(),
                                    ))
                                };
                                let i = usize::try_from(*i).map_err(|_| out_of_bounds())?;
                                if i < list.len() {
                                    list.remove(i);
                                } else {
                                    return Err(out_of_bounds());
                                }
                                Val::Unit
                            }
                            (Val::String(s), "contains", [Val::String(sub)]) => {
                                Val::Bool(s.contains(sub))
                            }
                            (val, "str", []) => Val::String(format!("{}", val)),
                            (Val::String(s), "int", []) => Val::Res(
                                s.parse()
                                    .map(|x| Rc::new(RefCell::new(Val::Int(x))))
                                    .map_err(|x| {
                                        Rc::new(RefCell::new(Val::String(format!("{}", x))))
                                    }),
                            ),
                            (Val::String(s), "flt", []) => Val::Res(
                                s.parse()
                                    .map(|x| Rc::new(RefCell::new(Val::Float(x))))
                                    .map_err(|x| {
                                        Rc::new(RefCell::new(Val::String(format!("{}", x))))
                                    }),
                            ),
                            (Val::Int(i), "flt", []) => Val::Float(*i as f64),
                            (Val::Float(f), "round", []) => Val::Int(f.round() as i64),
                            (Val::Float(f), "ciel", []) => Val::Int(f.ceil() as i64),
                            (Val::Float(f), "floor", []) => Val::Int(f.floor() as i64),
                            (_, fn_name, args) => Err(EvalError::Program(format!(
                                "cannot invoke method '{}' with args {:?}",
                                fn_name, args
                            )))
                            .map_err(|e| ControlFlow::Error(e))?,
                        },
                    )))
                }
                Expr::List(vec) => {
                    let index = match (vec.get(0), vec.get(1)) {
                        (Some(expr), None) => Ok(expr),
                        _ => Err(ControlFlow::Error(EvalError::Program(format!(
                            "can't index like .[x, y], you must index like .[x]"
                        )))),
                    }?;
                    let index = eval_expr(index, ctx)?.borrow().clone();
                    let left_val = eval_expr(left, ctx)?;
                    let mut left_val = left_val.borrow_mut();
                    let left_val = left_val.deref_mut();
                    let out_of_bounds = || {
                        ControlFlow::Error(EvalError::Program(
                            "list access out of bounds".to_string(),
                        ))
                    };
                    match (left_val, index) {
                        (Val::List(list), Val::Int(i)) => {
                            let i = usize::try_from(i).map_err(|_| out_of_bounds())?;
                            list.get(i).ok_or_else(out_of_bounds).map(|x| x.clone())
                        }
                        (Val::Dict(dict), Val::String(s)) => dict.get(&s).map(|x| x.clone()).ok_or(
                            ControlFlow::Error(EvalError::Program(format!(
                                "key {} does not exist in dict {}",
                                s,
                                Val::Dict(dict.clone())
                            ))),
                        ),
                        (into, index) => Err(ControlFlow::Error(EvalError::Program(format!(
                            "can't index with {} into {}",
                            index, into
                        )))),
                    }
                }
                Expr::Var(ident) => {
                    let left_val = eval_expr(left, ctx)?;
                    let mut left_val = left_val.borrow_mut();
                    let left_val = left_val.deref_mut();
                    match left_val {
                        Val::Dict(dict) => {
                            dict.get(ident)
                                .map(|x| x.clone())
                                .ok_or(EvalError::Program(format!(
                                    "key {} does not exist in dict {}",
                                    ident,
                                    Val::Dict(dict.clone())
                                )))
                        }
                        x => Err(EvalError::Program(format!(
                            "can't access field {} of {}",
                            ident, x
                        ))),
                    }
                    .map_err(|e| ControlFlow::Error(e))
                }
                _ => {
                    let left_val = eval_expr(left, ctx)?;
                    Err(ControlFlow::Error(EvalError::Program(format!(
                        "can't access property {:?} of {:?}",
                        right, left_val
                    ))))
                }
            },
            op => {
                let a = eval_expr(left, ctx)?;
                let a = a.borrow();
                let b = eval_expr(right, ctx)?;
                let b = b.borrow();
                Ok(Rc::new(RefCell::new(
                    handle_operator(a.deref(), &op, b.deref())
                        .map_err(|e| ControlFlow::Error(e))?,
                )))
            }
        },
        Expr::Var(ident) => {
            Ok(ctx
                .get(ident)
                .cloned()
                .ok_or(ControlFlow::Error(EvalError::Program(format!(
                    "variable {} does not exist",
                    ident
                ))))?)
        }
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Continue,
    Break,
    Exit,
    Error(EvalError),
}

enum Path<'a> {
    Str(String, &'a mut HashMap<String, Rc<RefCell<Val>>>),
    Int(usize, &'a mut Vec<Rc<RefCell<Val>>>),
}

impl<'a> Path<'a> {
    fn resolve(&self) -> Option<&Rc<RefCell<Val>>> {
        match self {
            Path::Str(s, map) => map.get(s).clone(),
            Path::Int(i, vec) => vec.get(*i).clone(),
        }
    }
}

pub fn eval(
    program: &Vec<Statement>,
    ctx: &mut HashMap<String, Rc<RefCell<Val>>>,
) -> Result<(), ControlFlow> {
    for statement in program {
        match statement {
            Statement::If(expr, true_case, false_case) => {
                let condition = eval_expr(expr, ctx)?;
                let condition = condition.borrow();
                if let Val::Bool(condition) = *condition {
                    if condition {
                        eval(true_case, ctx)?;
                    } else {
                        eval(false_case, ctx)?;
                    }
                } else {
                    Err(ControlFlow::Error(EvalError::Program(format!(
                        "expected true/false, found {:?}",
                        condition
                    ))))?;
                }
            }
            Statement::For(var, second_var, expr, body) => {
                let iterable = eval_expr(expr, ctx)?;
                let iterable = iterable.borrow();
                let iterable = iterable.deref().clone();
                if let Some(second_var) = second_var {
                    match iterable {
                        Val::Dict(elems) => {
                            for (k, v) in
                                elems.into_iter().map(|(k, v)| (Val::String(k.clone()), v))
                            {
                                ctx.insert(var.clone(), Rc::new(RefCell::new(k)));
                                ctx.insert(second_var.clone(), v.clone());
                                match eval(body, ctx) {
                                    Err(ControlFlow::Break) => break,
                                    Err(ControlFlow::Continue) | Ok(()) => {}
                                    Err(ControlFlow::Exit) => return Err(ControlFlow::Exit),
                                    Err(ControlFlow::Error(e)) => {
                                        return Err(ControlFlow::Error(e))
                                    }
                                }
                            }
                        }
                        _ => Err(ControlFlow::Error(EvalError::Program(format!(
                            "can't iterate over {:?}",
                            iterable
                        ))))?,
                    };
                } else {
                    let values: Box<dyn Iterator<Item = Rc<RefCell<Val>>>> = match iterable {
                        Val::List(elems) => Box::new(elems.into_iter()),
                        Val::Range(min, max) => Box::new(
                            (min..max)
                                .into_iter()
                                .map(|i| Rc::new(RefCell::new(Val::Int(i)))),
                        ),
                        _ => Err(ControlFlow::Error(EvalError::Program(format!(
                            "can't iterate over {:?}",
                            iterable
                        ))))?,
                    };
                    for val in values {
                        ctx.insert(var.clone(), val);
                        match eval(body, ctx) {
                            Err(ControlFlow::Break) => break,
                            Err(ControlFlow::Continue) | Ok(()) => {}
                            Err(ControlFlow::Exit) => return Err(ControlFlow::Exit),
                            Err(ControlFlow::Error(e)) => return Err(ControlFlow::Error(e)),
                        }
                    }
                }
            }
            Statement::Assign(assign, op, expr) => {
                let op = op.as_ref();
                let new = eval_expr(expr, ctx)?;
                match assign {
                    super::parser::Assign::Var(ident) => {
                        handle_assignment(new, Path::Str(ident.clone(), ctx), op)?
                    }
                    super::parser::Assign::Field(left, field) => {
                        let mut ctx_copy = ctx.clone();
                        let left = eval_expr(left, ctx)?;
                        let mut left = left.borrow_mut();
                        let left = left.deref_mut();
                        match field {
                            super::parser::Field::Var(var) => {
                                let current = if let Val::Dict(val) = left {
                                    Path::Str(var.clone(), val)
                                } else {
                                    return Err(ControlFlow::Error(EvalError::Program(
                                        "expected left side of expression to be a dict".to_string(),
                                    )));
                                };
                                handle_assignment(new, current, op)?;
                            }
                            super::parser::Field::Index(index) => {
                                let index = eval_expr(index, &mut ctx_copy)?;
                                let index = index.borrow();
                                let index = index.deref();
                                let current = match (left, index) {
                                    (Val::Dict(val), Val::String(index)) => {
                                        Path::Str(index.clone(), val)
                                    }
                                    (Val::List(val), Val::Int(index)) => {
                                        let index = usize::try_from(*index).unwrap();
                                        Path::Int(index, val)
                                    }
                                    _ => {
                                        return Err(ControlFlow::Error(EvalError::Program(
                                            "invalid assignment, expected either:\n
                                             <expr that evaluates to a list>.[<expr that evaluates to an int>]\n
                                             <expr that evaluates to a dict>.[<expr that evaluates to a str>]".to_string(),
                                        )))
                                    }
                                };
                                handle_assignment(new, current, op)?;
                            }
                        }
                    }
                };
            }
            Statement::Expr(expr) => {
                eval_expr(expr, ctx)?;
            }
            Statement::Continue => return Err(ControlFlow::Continue),
            Statement::Break => return Err(ControlFlow::Break),
            Statement::Pass => {}
            Statement::Loop(body) => loop {
                match eval(body, ctx) {
                    Err(ControlFlow::Break) => break,
                    Err(ControlFlow::Continue) | Ok(()) => {}
                    Err(ControlFlow::Exit) => return Err(ControlFlow::Exit),
                    Err(ControlFlow::Error(e)) => return Err(ControlFlow::Error(e)),
                }
            },
        }
    }
    Ok(())
}

fn handle_assignment(
    new: Rc<RefCell<Val>>,
    current: Path<'_>,
    op: Option<&AssignOp>,
) -> Result<(), ControlFlow> {
    let new = match (current.resolve(), op) {
        (Some(current), Some(x)) => match x {
            lexer::AssignOp::Add => handle_operator_rc(current, &BinOp::Op(lexer::Op::Add), &new),
            lexer::AssignOp::Sub => handle_operator_rc(current, &BinOp::Op(lexer::Op::Sub), &new),
            lexer::AssignOp::Mul => handle_operator_rc(current, &BinOp::Op(lexer::Op::Mul), &new),
            lexer::AssignOp::Div => handle_operator_rc(current, &BinOp::Op(lexer::Op::Div), &new),
        }
        .map_err(|e| ControlFlow::Error(e))?,
        (_, None) => new,
        (None, Some(_)) => {
            return Err(ControlFlow::Error(EvalError::Program(
                "can't do +=,-=,*=,/= because the variable or field wasn't previously defined"
                    .to_string(),
            )))
        }
    };
    match current {
        Path::Str(key, map) => {
            map.insert(key, new);
        }
        Path::Int(index, list) => {
            if index < list.len() {
                list[index] = new;
            } else {
                return Err(ControlFlow::Error(EvalError::Program(
                    "list access out of bounds".to_string(),
                )));
            }
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    // primitives
    Unit,
    Int(i64),
    Bool(bool),
    Float(f64),
    String(String),
    Range(i64, i64),
    // containers
    List(Vec<Rc<RefCell<Val>>>),
    Dict(HashMap<String, Rc<RefCell<Val>>>),
    Opt(Option<Rc<RefCell<Val>>>),
    Res(Result<Rc<RefCell<Val>>, Rc<RefCell<Val>>>),
}

pub fn to_json(x: &Rc<RefCell<Val>>) -> Result<serde_json::Value, String> {
    let x = x.borrow();
    x.deref().to_json()
}

pub fn from_json(val: serde_json::Value) -> Result<Val, String> {
    Ok(match val {
        serde_json::Value::Null => Val::Opt(None),
        serde_json::Value::Bool(b) => Val::Bool(b),
        serde_json::Value::Number(number) => {
            if number.is_i64() {
                Val::Int(number.as_i64().unwrap())
            } else if number.is_f64() {
                Val::Float(number.as_f64().unwrap())
            } else {
                return Err("number cannot be converted to i64 or f64".into());
            }
        }
        serde_json::Value::String(s) => Val::String(s),
        serde_json::Value::Array(vec) => {
            let vec: Result<Vec<_>, _> = vec.into_iter().map(|x| from_json(x)).collect();
            Val::List(vec?.into_iter().map(|x| Rc::new(RefCell::new(x))).collect())
        }
        serde_json::Value::Object(map) => {
            let mut dict = HashMap::new();
            for (k, v) in map {
                let v = from_json(v)?;
                let v = if k.ends_with('?') {
                    // wrap field in optional if it ends with a '?'
                    match v {
                        Val::Opt(None) => Val::Opt(None),
                        x => Val::Opt(Some(Rc::new(RefCell::new(x)))),
                    }
                } else {
                    // otherwise, assume that the field should be present.
                    match v {
                        Val::Opt(None) => {
                            return Err("field does not end with '?', but is null.".into())
                        }
                        x => x,
                    }
                };
                dict.insert(k, Rc::new(RefCell::new(v)));
            }
            Val::Dict(dict)
        }
    })
}

impl Val {
    pub fn to_json(&self) -> Result<serde_json::Value, String> {
        match self {
            Val::Unit => Err("cannot serialize unit '()' to json".into()),
            Val::Int(i) => Ok(json!(i)),
            Val::Bool(b) => Ok(json!(b)),
            Val::Float(f) => Ok(json!(f)),
            Val::String(s) => Ok(json!(s)),
            Val::Range(x, y) => Err(format!("cannot serialize range '{}..{}' to json", x, y)),
            Val::List(vec) => {
                let array: Result<Vec<_>, _> = vec.iter().map(|x| to_json(x)).collect();
                Ok(serde_json::Value::Array(array?))
            }
            Val::Dict(hash_map) => {
                let mut map = serde_json::Map::new();
                for (k, v) in hash_map {
                    let v = to_json(v)?;
                    if !k.ends_with('?') && v == serde_json::Value::Null {
                        return Err(format!("non-nullable field '{}' was null.", k));
                    }
                    map.insert(k.clone(), v);
                }
                Ok(serde_json::Value::Object(map))
            }
            Val::Opt(ref_cell) => match ref_cell {
                Some(json) => to_json(json),
                None => Ok(serde_json::Value::Null),
            },
            Val::Res(res) => Err(format!("cannot serialize result '{:?}' to json", res)),
        }
    }
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Val::Unit => write!(f, "()"),
            Val::Int(value) => write!(f, "{}", value),
            Val::Bool(value) => write!(f, "{}", value),
            Val::Float(value) => write!(f, "{}", value),
            Val::String(value) => write!(f, "'{}'", value),
            Val::List(values) => {
                let formatted_values: Vec<String> =
                    values.iter().map(|v| format!("{}", v.borrow())).collect();
                write!(f, "[{}]", formatted_values.join(", "))
            }
            Val::Range(start, end) => write!(f, "({}..{})", start, end),
            Val::Dict(map) => {
                let formatted_entries: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("'{}': {}", k, v.borrow()))
                    .collect();
                write!(f, "{{{}}}", formatted_entries.join(", "))
            }
            Val::Opt(v) => {
                if let Some(v) = v {
                    write!(f, "Some({})", v.borrow())
                } else {
                    write!(f, "None")
                }
            }
            Val::Res(v) => match v {
                Ok(ok) => write!(f, "Ok({})", ok.borrow()),
                Err(err) => write!(f, "Err({})", err.borrow()),
            },
        }
    }
}

fn handle_operator_rc(
    a: &Rc<RefCell<Val>>,
    op: &super::parser::BinOp,
    b: &Rc<RefCell<Val>>,
) -> Result<Rc<RefCell<Val>>, EvalError> {
    let a = a.borrow();
    let a = a.deref();
    let b = b.borrow();
    let b = b.deref();
    Ok(Rc::new(RefCell::new(handle_operator(a, op, b)?)))
}

fn handle_operator(a: &Val, op: &super::parser::BinOp, b: &Val) -> Result<Val, EvalError> {
    use super::lexer::Op::*;
    use super::parser::BinOp::*;
    Ok(match (a, op, b) {
        (Val::Int(a), Op(Add), Val::Int(b)) => Val::Int(a + b),
        (Val::Int(a), Op(Sub), Val::Int(b)) => Val::Int(a - b),
        (Val::Int(a), Op(Mul), Val::Int(b)) => Val::Int(a * b),
        (Val::Int(a), Op(Div), Val::Int(b)) => Val::Int(a / b),
        (Val::Int(a), Op(LThan), Val::Int(b)) => Val::Bool(a < b),
        (Val::Int(a), Op(GThan), Val::Int(b)) => Val::Bool(a > b),
        (Val::Int(a), Op(LThanEq), Val::Int(b)) => Val::Bool(a <= b),
        (Val::Int(a), Op(GThanEq), Val::Int(b)) => Val::Bool(a >= b),
        (Val::Float(a), Op(Add), Val::Float(b)) => Val::Float(a + b),
        (Val::Float(a), Op(Sub), Val::Float(b)) => Val::Float(a - b),
        (Val::Float(a), Op(Mul), Val::Float(b)) => Val::Float(a * b),
        (Val::Float(a), Op(Div), Val::Float(b)) => Val::Float(a / b),
        (Val::Float(a), Op(LThan), Val::Float(b)) => Val::Bool(a < b),
        (Val::Float(a), Op(GThan), Val::Float(b)) => Val::Bool(a > b),
        (Val::Float(a), Op(LThanEq), Val::Float(b)) => Val::Bool(a <= b),
        (Val::Float(a), Op(GThanEq), Val::Float(b)) => Val::Bool(a >= b),
        (Val::String(a), Op(Add), Val::String(b)) => Val::String(a.to_owned() + b),
        (Val::Bool(a), And, Val::Bool(b)) => Val::Bool(*a && *b),
        (Val::Bool(a), Or, Val::Bool(b)) => Val::Bool(*a || *b),
        (Val::Int(a), Op(Range), Val::Int(b)) => Val::Range(*a, *b),
        (left, Op(Eq), right) => Val::Bool(left == right),
        (left, Op(Neq), right) => Val::Bool(left != right),
        (Val::List(a), Op(Add), Val::List(b)) => {
            Val::List(a.clone().into_iter().chain(b.clone().into_iter()).collect())
        }
        (Val::Dict(a), Op(Add), Val::Dict(b)) => {
            Val::Dict(a.clone().into_iter().chain(b.clone().into_iter()).collect())
        }
        (a, op, b) => Err(EvalError::Program(format!(
            "can't do operation {:?} between {:?} and {:?}",
            op, a, b
        )))?,
    })
}

#[derive(Debug, Clone)]
pub enum EvalError {
    Program(String), // program did something invalid
    Custom(String),  // program explicitly threw an error
}

#[cfg(test)]
mod tests {
    use std::task::Context;

    use crate::lang::parser::parse;

    use super::*;

    #[test]
    fn test_is_int() {
        let mut empty_ctx = HashMap::new();
        let tokens = lexer::tokenize("not('1'.int?())").unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        match &statements[..] {
            [Statement::Expr(expr)] => {
                let res = eval_expr(expr, &mut empty_ctx).unwrap();
                assert!(empty_ctx.is_empty());
                let res = res.borrow();
                let res = res.deref();
                match res {
                    Val::Bool(false) => {}
                    _ => panic!("expected `false`, found {}", res),
                }
            }
            _ => panic!("expected single expression"),
        }
    }

    #[test]
    fn test_interior_mutability() {
        let mut ctx = HashMap::new();
        let program = include_str!("../../test-spells/interior-mutability.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        eval(&statements, &mut ctx).unwrap();
        let x = ctx.get("x").unwrap().borrow();
        match x.deref() {
            Val::Dict(x) => match x.get("a").unwrap().borrow().deref() {
                Val::String(s) if s == "ab" => {}
                _ => panic!("expected x.a == 'ab'"),
            },
            _ => panic!("expected 'x' to be a dict"),
        }
        let z = ctx.get("z").unwrap().borrow();
        match z.deref() {
            Val::Dict(z) => match z.get("a").unwrap().borrow().deref() {
                Val::String(s) if s == "aa" => {}
                _ => panic!("expected z.a == 'aa'"),
            },
            _ => panic!("expected 'z' to be a dict"),
        }
    }

    #[test]
    fn test_avg() {
        let mut ctx = HashMap::new();
        let program = include_str!("../../test-spells/avg.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        eval(&statements, &mut ctx).unwrap();
        let dataset = to_json(ctx.get("dataset").unwrap()).unwrap();
        let expected_json = json!([
            {"avg":2, "nums":[1,2,3]},
            {"avg":5, "nums":[4,5,6]},
            {"avg":8, "nums":[7,8,9]},
        ]);
        assert_eq!(dataset, expected_json);
    }

    #[test]
    fn test_damage_to_target() {
        for target in ["p0", "p1", "creature0", "bad-id"] {
            let mut ctx = HashMap::new();
            let board = json!({
                "players" : {
                    "p0" : { "health" : 3, "targets" : [target], "mana" : 10},
                    "p1" : { "health" : 3, "targets" : [] }
                },
                "creatures" : {
                    "creature0" : { "health" : 3 }
                }
            });
            ctx.insert(
                "@board".into(),
                Rc::new(RefCell::new(from_json(board).unwrap())),
            );
            ctx.insert(
                "@owner".into(),
                Rc::new(RefCell::new(Val::String("p0".into()))),
            );
            let program = include_str!("../../test-spells/damage-to-target.spell");
            let tokens = lexer::tokenize(program).unwrap();
            let statements = parse(&mut tokens.iter()).unwrap();
            match (target, eval(&statements, &mut ctx)) {
                ("bad-id", Err(ControlFlow::Error(EvalError::Program(x)))) => {
                    assert_eq!(x, "target is not a player or creature");
                }
                (_, x) => {
                    x.unwrap();
                }
            };
            let board = to_json(&ctx["@board"]).unwrap();
            let expected_json = json!({
                "players" : {
                    "p0" : { 
                        "health" : if target == "p0" { 0 } else { 3 }, 
                        "mana" : if target == "bad-id" { 10 } else { 7 },
                        "targets" : [target], 
                    },
                    "p1" : { "health" : if target == "p1" { 0 } else { 3 }, "targets" : [] }
                },
                "creatures" : {
                    "creature0" : { "health" : if target == "creature0" { 0 } else { 3 } }
                }
            });
            assert_eq!(board, expected_json);
        }
    }
}
