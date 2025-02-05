use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use gloo_utils::format::JsValueSerdeExt;
use serde_json::json;
use wasm_bindgen::JsValue;

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
        Expr::Call(fn_name, vec) => {
            let args: Result<Vec<_>, _> = vec.iter().map(|expr| eval_expr(expr, ctx)).collect();
            let args = args?;
            let cannot_invoke = ControlFlow::Error(EvalError::Program(format!(
                "cannot invoke function '{}' with arguments {}",
                fn_name,
                Val::List(args.clone())
            )));

            let builtins = ["print", "exit", "err", "not"];
            if builtins.contains(&&fn_name[..]) {
                let args: Vec<_> = args.iter().map(|x| x.borrow().clone()).collect();
                Ok(Rc::new(RefCell::new(match (&fn_name[..], &args[..]) {
                    ("print", [Val::String(s)]) => {
                        // this if check exists so that we can locally run test code that prints to console.
                        if cfg!(any(target_family = "wasm")) {
                            web_sys::console::log_1(&s.into());
                        } else {
                            println!("{}", s);
                        }
                        Ok(Val::Unit)
                    }
                    ("print", [v]) => {
                        // this if check exists so that we can locally run test code that prints to console.
                        if cfg!(any(target_family = "wasm")) {
                            match v.to_json() {
                                Ok(json) => {
                                    web_sys::console::log_1(&JsValue::from_serde(&json).unwrap());
                                }
                                Err(_) => {
                                    let s = format!("{}", v);
                                    web_sys::console::log_1(&s.into());
                                }
                            }
                        } else {
                            println!("{}", v);
                        }
                        Ok(Val::Unit)
                    }
                    ("exit", []) => Err(ControlFlow::Exit),
                    ("err", [Val::String(s)]) => {
                        Err(ControlFlow::Error(EvalError::Custom(s.to_string())))
                    }
                    ("not", [Val::Bool(b)]) => Ok(Val::Bool(!b)),
                    _ => Err(cannot_invoke),
                }?)))
            } else {
                let function = ctx.get(fn_name).map(|x| x.borrow());
                let res = if let Some(function) = function {
                    let function = function.deref();
                    match function {
                        Val::Function(argument_names, program) => {
                            if argument_names.len() != args.len() {
                                return Err(cannot_invoke);
                            }
                            let mut function_ctx = HashMap::new();
                            for (name, value) in argument_names.iter().zip(args) {
                                function_ctx.insert(name.clone(), value);
                            }
                            match eval(&program, &mut function_ctx) {
                                Ok(()) => Ok(Rc::new(RefCell::new(Val::Unit))),
                                Err(ControlFlow::Return(val)) => Ok(val),
                                Err(ControlFlow::Break | ControlFlow::Continue) => {
                                    Err(ControlFlow::Error(EvalError::Program(
                                        "called break or continue outside of loop".into(),
                                    )))
                                }
                                Err(e) => Err(e),
                            }
                        }
                        x => Err(ControlFlow::Error(EvalError::Program(format!(
                            "{:?} is not a function, but is being called.",
                            x
                        )))),
                    }
                } else {
                    Err(cannot_invoke)
                };
                return res;
            }
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
                    let mut args = args?;
                    let left = eval_expr(left, ctx)?;
                    let ret = {
                        let mut left = left.borrow_mut();
                        let left_val = left.deref_mut();
                        match (left_val, &ident[..]) {
                            (Val::List(list), "add") => {
                                if let (Some(argument), None) = (args.pop(), args.pop()) {
                                    list.push(argument);
                                    Some(Val::Unit)
                                } else {
                                    None
                                }
                            }
                            (Val::Dict(dict), "remove") => {
                                let args: Vec<_> = args.iter().map(|x| x.borrow()).collect();
                                let args: Vec<_> = args.iter().map(|x| x.deref()).collect();
                                match &args[..] {
                                    [Val::String(s)] => {
                                        dict.remove(s);
                                        Some(Val::Unit)
                                    }
                                    _ => None,
                                }
                            }
                            (Val::List(list), "remove") => {
                                let args: Vec<_> = args.iter().map(|x| x.borrow()).collect();
                                let args: Vec<_> = args.iter().map(|x| x.deref()).collect();
                                match &args[..] {
                                    [Val::Int(i)] => {
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
                                        Some(Val::Unit)
                                    }
                                    _ => None,
                                }
                            },
                            _ => None,
                        }
                    };

                    if let Some(ret) = ret {
                        return Ok(Rc::new(RefCell::new(ret)));
                    }
                    let left = left.borrow();
                    let left_val = left.deref();
                    let args: Vec<_> = args.iter().map(|x| x.borrow()).collect();
                    let args: Vec<_> = args.iter().map(|x| x.deref()).collect();
                    Ok(Rc::new(RefCell::new(
                        match (left_val, &ident[..], &args[..]) {
                            (Val::Dict(dict), "contains_key", [Val::String(s)]) => {
                                Val::Bool(dict.contains_key(s))
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
                                        "error: {}",
                                        e.borrow()
                                    ))))
                                }
                            },
                            (Val::Res(res), "unwrap_err", []) => match res {
                                Ok(ok) => {
                                    return Err(ControlFlow::Error(EvalError::Program(format!(
                                        "expected error value, but was Ok({})",
                                        ok.borrow()
                                    ))))
                                }
                                Err(e) => return Ok(e.clone()),
                            },
                            (Val::List(list), "len", []) => Val::Int(list.len() as i64),
                            (Val::List(list), "map", [Val::Function(args, program)]) => {
                                if args.len() > 1 {
                                    return Err(ControlFlow::Error(EvalError::Program(
                                        "map expects a function with one argument".to_string(),
                                    )));
                                }
                                let mut mapped_list = vec![];
                                for val in list {
                                    let mut ctx = ctx.clone();
                                    ctx.insert(args[0].clone(), val.clone());
                                    mapped_list.push(match eval(program, &mut ctx) {
                                        Ok(()) => Rc::new(RefCell::new(Val::Unit)),
                                        Err(ControlFlow::Return(val)) => val,
                                        Err(ControlFlow::Break | ControlFlow::Continue) => {
                                            Err(ControlFlow::Error(EvalError::Program(
                                                "called break or continue outside of loop".into(),
                                            )))?
                                        }
                                        Err(e) => Err(e)?
                                    })
                                }
                                Val::List(mapped_list)
                            }
                            (Val::List(list), "filter", [Val::Function(args, program)]) => {
                                if args.len() > 1 {
                                    return Err(ControlFlow::Error(EvalError::Program(
                                        "filter expects a function with one argument".to_string(),
                                    )));
                                }
                                let mut filtered_list = vec![];
                                for val in list {
                                    let mut ctx = ctx.clone();
                                    ctx.insert(args[0].clone(), val.clone());
                                    let should_filter = match eval(program, &mut ctx) {
                                        Ok(()) => Rc::new(RefCell::new(Val::Unit)),
                                        Err(ControlFlow::Return(val)) => val,
                                        Err(ControlFlow::Break | ControlFlow::Continue) => {
                                            return Err(ControlFlow::Error(EvalError::Program(
                                                "called break or continue outside of loop".into(),
                                            )));
                                        }
                                        Err(e) => Err(e)?
                                    };
                                    let should_filter = should_filter.borrow();
                                    if let Val::Bool(should_filter) = should_filter.deref() {
                                        if *should_filter {
                                            filtered_list.push(val.clone());
                                        }
                                    } else {
                                        return Err(ControlFlow::Error(EvalError::Program(
                                            format!("filter returned {} instead of bool", should_filter.deref())
                                        )));
                                    }
                                }
                                Val::List(filtered_list)
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
                                "cannot invoke method '{}' with args {}",
                                fn_name,
                                args.iter().map(|x| format!("{},", x)).collect::<String>()
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
                    let left_val = {
                        let left_val = eval_expr(left, ctx)?;
                        let left_val = left_val.borrow();
                        left_val.deref().clone()
                    };
                    match &left_val {
                        Val::Dict(dict) => dict.get(ident).map(|x| x.clone()).ok_or_else(|| {
                            EvalError::Program(format!(
                                "key {} does not exist in dict {}",
                                ident,
                                Val::Dict(dict.clone())
                            ))
                        }),
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
                        "can't access property {:?} of {}",
                        right,
                        left_val.borrow()
                    ))))
                }
            },
            BinOp::Chain => {
                eval_expr(left, ctx)?;
                eval_expr(right, ctx)
            }
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
        Expr::Lambda(args, expr) => Ok(Rc::new(RefCell::new(Val::Function(
            args.clone(),
            vec![Statement::Return(Some(*expr.clone()))],
        )))),
        Expr::Assign(assign, op, expr) => {
            let op = op.as_ref();
            let new = eval_expr(expr, ctx)?;
            match *assign.clone() {
                super::parser::Assign::Var(ident) => {
                    let new = get_new_for_assignment(new, &Path::Str(ident.clone(), ctx), op)?;
                    handle_assignment(new, PathMut::Str(ident.clone(), ctx))?;
                }
                super::parser::Assign::Field(left, field) => {
                    let mut ctx_copy = ctx.clone();
                    let left = eval_expr(&left, ctx)?;
                    let (new, path_val) = {   
                        let left = left.borrow();
                        let left = left.deref();
                        match field {
                            super::parser::Field::Var(var) => {
                                let current = if let Val::Dict(val) = left {
                                    Path::Str(var.clone(), val)
                                } else {
                                    return Err(ControlFlow::Error(EvalError::Program(
                                        "expected left side of expression to be a dict".to_string(),
                                    )));
                                };
                                (get_new_for_assignment(new, &current, op)?, current.path_val())
                            }
                            super::parser::Field::Index(index) => {
                                let index = eval_expr(&index, &mut ctx_copy)?;
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
                                (get_new_for_assignment(new, &current, op)?, current.path_val())
                            }
                        }
                    };
                    let mut left = left.borrow_mut();
                    let left = left.deref_mut();
                    match (left, path_val) {
                        (Val::Dict(dict), PathVal::Str(val)) => {
                            handle_assignment(new, PathMut::Str(val, dict))?;
                        },
                        (Val::List(list), PathVal::Int(val)) => {
                            handle_assignment(new, PathMut::Int(val, list))?;
                        },
                        _ => unreachable!()
                    }
                }
            };
            Ok(Rc::new(RefCell::new(Val::Unit)))
        },
    }
}

#[derive(Debug)]
pub enum ControlFlow {
    Continue,
    Break,
    Exit,
    Error(EvalError),
    Return(Rc<RefCell<Val>>),
}

enum PathVal {
    Str(String),
    Int(usize)
}

enum Path<'a> {
    Str(String, &'a HashMap<String, Rc<RefCell<Val>>>),
    Int(usize, &'a Vec<Rc<RefCell<Val>>>),
}

impl<'a> Path<'a> {
    fn resolve(&self) -> Option<&Rc<RefCell<Val>>> {
        match self {
            Path::Str(s, map) => map.get(s).clone(),
            Path::Int(i, vec) => vec.get(*i).clone(),
        }
    }

    fn path_val(&self) -> PathVal {
        match self {
            Path::Str(x, _) => PathVal::Str(x.clone()),
            Path::Int(x, _) => PathVal::Int(*x),
        }
    }
}

enum PathMut<'a> {
    Str(String, &'a mut HashMap<String, Rc<RefCell<Val>>>),
    Int(usize, &'a mut Vec<Rc<RefCell<Val>>>),
}

pub fn eval(
    program: &Vec<Statement>,
    ctx: &mut HashMap<String, Rc<RefCell<Val>>>,
) -> Result<(), ControlFlow> {
    for statement in program {
        match statement {
            Statement::If(expr, true_case, false_case) => {
                let condition = {
                    let condition = eval_expr(expr, ctx)?;
                    let condition = condition.borrow();
                    condition.clone()
                };
                if let Val::Bool(condition) = condition {
                    if condition {
                        eval(true_case, ctx)?;
                    } else {
                        eval(false_case, ctx)?;
                    }
                } else {
                    Err(ControlFlow::Error(EvalError::Program(format!(
                        "expected true/false, found {}",
                        condition
                    ))))?;
                }
            }
            Statement::For(var, second_var, expr, body) => {
                let iterable = {
                    let iterable = eval_expr(expr, ctx)?;
                    let iterable = iterable.borrow();
                    iterable.deref().clone()
                };
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
                                    Err(ControlFlow::Return(x)) => {
                                        return Err(ControlFlow::Return(x))
                                    }
                                }
                            }
                        }
                        _ => Err(ControlFlow::Error(EvalError::Program(format!(
                            "can't iterate over {}",
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
                            "can't iterate over {}",
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
                            Err(ControlFlow::Return(x)) => return Err(ControlFlow::Return(x)),
                        }
                    }
                }
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
                    Err(ControlFlow::Return(x)) => return Err(ControlFlow::Return(x)),
                }
            },
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    return Err(ControlFlow::Return(eval_expr(expr, ctx)?));
                } else {
                    return Err(ControlFlow::Return(Rc::new(RefCell::new(Val::Unit))));
                }
            }
            Statement::Function(name, args, body) => {
                ctx.insert(
                    name.clone(),
                    Rc::new(RefCell::new(Val::Function(args.clone(), body.clone()))),
                );
            }
        }
    }
    Ok(())
}

fn get_new_for_assignment(
    new: Rc<RefCell<Val>>,
    current: &Path<'_>,
    op: Option<&AssignOp>,
) -> Result<Rc<RefCell<Val>>, ControlFlow> {
    Ok(match (current.resolve(), op) {
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
    })
}

fn handle_assignment(
    new: Rc<RefCell<Val>>,
    current: PathMut<'_>,
) -> Result<(), ControlFlow> {
    match current {
        PathMut::Str(key, map) => {
            map.insert(key, new);
        }
        PathMut::Int(index, list) => {
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

#[derive(Clone, PartialEq)]
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
    // code
    Function(Vec<String>, Vec<Statement>),
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
        let is_looping = match &self {
            Val::List(vec) => vec.iter().any(|x| is_looping(x.clone())),
            Val::Dict(map) => map.iter().any(|(_, x)| is_looping(x.clone())),
            Val::Opt(opt) => opt.as_ref().map(|x| is_looping(x.clone())).unwrap_or(false),
            Val::Res(res) => is_looping(
                match res {
                    Ok(x) => x,
                    Err(x) => x,
                }
                .clone(),
            ),
            _ => false,
        };
        if is_looping {
            return Err("structure contains looping references, cannot serialize to json.".into());
        }

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
            Val::Res(_) => Err(format!("cannot serialize result to json")),
            Val::Function(_, _) => Err(format!("cannot serialize function to json")),
        }
    }

    fn str(&self) -> String {
        match &self {
            Val::Unit => "()".into(),
            Val::Int(value) => format!("{}", value),
            Val::Bool(value) => format!("{}", value),
            Val::Float(value) => format!("{:?}f", value),
            Val::String(value) => format!("'{}'", value),
            Val::Range(start, end) => format!("({}..{})", start, end),
            Val::List(values) => {
                let formatted_values: Vec<String> = values
                    .iter()
                    .map(|v| {
                        if is_looping(v.clone()) {
                            "...".into()
                        } else {
                            v.borrow().str()
                        }
                    })
                    .collect();
                format!("[{}]", formatted_values.join(", "))
            }
            Val::Dict(map) => {
                let formatted_entries: Vec<String> = map
                    .iter()
                    .map(|(k, v)| {
                        let v = if is_looping(v.clone()) {
                            "...".into()
                        } else {
                            v.borrow().str()
                        };
                        format!("'{}': {}", k, v)
                    })
                    .collect();
                format!("{{{}}}", formatted_entries.join(", "))
            }
            Val::Opt(v) => {
                if let Some(v) = v {
                    let v = if is_looping(v.clone()) {
                        "...".into()
                    } else {
                        v.borrow().str()
                    };
                    format!("Some({})", v)
                } else {
                    "None".into()
                }
            }
            Val::Res(v) => {
                let either = match &v {
                    Ok(x) => x,
                    Err(x) => x,
                };
                let s = if is_looping(either.clone()) {
                    "...".into()
                } else {
                    either.borrow().str()
                };
                match &v {
                    Ok(_) => format!("Ok({})", s),
                    Err(_) => format!("Err({})", s),
                }
            }
            Val::Function(args, _) => {
                format!("fn ({})", args.join(","))
            }
        }
    }
}

impl Debug for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.str())
    }
}

fn is_looping(start: Rc<RefCell<Val>>) -> bool {
    fn is_looping(start: Rc<RefCell<Val>>, find: Option<Rc<RefCell<Val>>>) -> bool {
        let find = if let Some(find) = find {
            if Rc::ptr_eq(&start, &find) {
                return true;
            }
            find
        } else {
            start.clone()
        };
        let start = start.borrow();
        let start = start.deref();
        match start {
            Val::List(vec) => vec
                .iter()
                .any(|x| is_looping(x.clone(), Some(find.clone()))),
            Val::Dict(map) => map
                .iter()
                .any(|(_, x)| is_looping(x.clone(), Some(find.clone()))),
            Val::Opt(x) => x
                .as_ref()
                .map(|x| is_looping(x.clone(), Some(find)))
                .unwrap_or(false),
            Val::Res(x) => {
                let either = match x {
                    Ok(x) => x,
                    Err(x) => x,
                };
                is_looping(either.clone(), Some(find))
            }
            _ => false,
        }
    }
    is_looping(start, None)
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.str())
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
            Val::List(a.iter().chain(b.iter()).map(|x| x.clone()).collect())
        }
        (Val::Dict(a), Op(Add), Val::Dict(b)) => {
            Val::Dict(a.iter().chain(b.iter()).map(|(k, v)| (k.clone(), v.clone())).collect())
        }
        (a, op, b) => Err(EvalError::Program(format!(
            "can't do operation {:?} between {} and {}",
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
    use crate::lang::parser::parse;

    use super::*;

    fn expr_eval(
        program: &str,
        ctx: &mut HashMap<String, Rc<RefCell<Val>>>,
    ) -> Result<Rc<RefCell<Val>>, ControlFlow> {
        let tokens = lexer::tokenize(program).unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        match &statements[..] {
            [Statement::Expr(expr)] => eval_expr(expr, ctx),
            _ => panic!("expected single expression"),
        }
    }

    #[test]
    fn test_is_int() {
        let mut empty_ctx = HashMap::new();
        let tokens = lexer::tokenize("'1'.int().is_err()").unwrap();
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
    fn test_example() {
        let mut ctx = HashMap::new();
        let program = include_str!("../../test-spells/example.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        eval(&statements, &mut ctx).unwrap();
    }

    #[test]
    fn test_looping_assignment() {
        let mut ctx: HashMap<String, Rc<RefCell<Val>>> = HashMap::new();
        let program = include_str!("../../test-spells/looping-assignment.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        eval(&statements, &mut ctx).unwrap();
        assert_eq!(ctx["x"].borrow().str(), "{'y': ...}");
        assert_eq!(ctx["y"].borrow().str(), "[...]");
        assert_eq!(
            "[...]",
            expr_eval("x.y.[0].y.str()", &mut ctx)
                .unwrap()
                .borrow()
                .to_json()
                .unwrap()
        );
        assert_eq!(
            "{'y': ...}",
            expr_eval("x.y.[0].y.[0].str()", &mut ctx)
                .unwrap()
                .borrow()
                .to_json()
                .unwrap()
        );
        assert_eq!(ctx["a"].borrow().to_json().unwrap(), json!([[[], 1, 2], 1, 2]));
        assert_eq!(ctx["b"].borrow().to_json().unwrap(), json!({"test_1": {"test": 0, "test_1": {}}, "test": 0}));
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
    fn test_run_function() {
        let mut ctx: HashMap<String, Rc<RefCell<Val>>> = HashMap::new();
        let program = include_str!("../../test-spells/fn.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        eval(&statements, &mut ctx).unwrap();
        assert_eq!(ctx["c"].borrow().to_json().unwrap(), json!({'a': 201, 'b': 10000}));
    }

    #[test]
    fn test_map_filter() {
        let mut ctx: HashMap<String, Rc<RefCell<Val>>> = HashMap::new();
        let program = include_str!("../../test-spells/map-filter.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let statements = parse(&mut tokens.iter()).unwrap();
        eval(&statements, &mut ctx).unwrap();
        assert_eq!(ctx["x"].borrow().to_json().unwrap(), json!([{'b': 6, 'a': -1}, {'a': 2, 'b': 1}]));
        assert_eq!(ctx["y"].borrow().to_json().unwrap(), json!([{'a': 2, 'b': 1}]));
    }
}
