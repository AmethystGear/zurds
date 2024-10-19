use std::{collections::HashMap, iter};

use super::{
    lexer::{self, Literal},
    parser::{BinOp, Expr, Statement},
};

#[derive(Clone, Debug)]
enum Val {
    Int(i64),
    Bool(bool),
    Float(f64),
    String(String),
    List(Vec<Val>),
    Range(i64, i64),
    Dict(HashMap<String, Val>),
}

fn eval_expr(expr: &Expr, ctx: &HashMap<String, Val>) -> Val {
    todo!()
}

enum ControlFlow {
    Continue,
    Break,
}

fn eval(program: &Vec<Statement>, mut ctx: HashMap<String, Val>) -> Option<ControlFlow> {
    for statement in program {
        match statement {
            Statement::If(expr, true_case, false_case) => {
                let condition = eval_expr(expr, &ctx);
                if let Val::Bool(condition) = condition {
                    if condition {
                        eval(true_case, ctx.clone())?;
                    } else {
                        eval(false_case, ctx.clone())?;
                    }
                } else {
                    panic!("if condition did not eval to boolean")
                }
            }
            Statement::For(var, expr, body) => {
                let iterable = eval_expr(expr, &ctx);
                match iterable {
                    Val::List(elems) => {
                        for elem in elems {
                            let mut ctx = ctx.clone();
                            ctx.insert(var.clone(), elem);
                            match eval(body, ctx) {
                                Some(ControlFlow::Break) => break,
                                _ => {}
                            }
                        }
                    }
                    Val::Range(min, max) => {
                        for i in min..max {
                            let mut ctx = ctx.clone();
                            ctx.insert(var.clone(), Val::Int(i));
                            match eval(body, ctx) {
                                Some(ControlFlow::Break) => break,
                                _ => {}
                            }
                        }
                    }
                    _ => panic!("expression in for loop is not iterable"),
                }
            }
            Statement::Assign(var, assignment_parts, op, expr) => {
                let eval = eval_expr(expr, &ctx);
                let val = if assignment_parts.len() == 0 {
                    ctx.get_mut(var)
                } else {
                    enum StringOrInt {
                        String(String),
                        Int(usize)
                    }
                    let mut path = vec![];
                    let mut val = ctx.get(var).expect(&format!("variable {} doesn't exist", var));
                    for assignment_part in assignment_parts {
                        match assignment_part {
                            super::parser::Assignment::Ident(ident) => match val {
                                Val::Dict(dict) => {
                                    if dict.contains_key(ident) {
                                        val = dict.get(ident).unwrap();
                                        path.push(StringOrInt::String(ident.clone()));
                                    } else {
                                        panic!("cannot modify property {} of {:?}", ident, dict);
                                    }
                                }
                                x => panic!("cannot modify property {} of {:?}", ident, x),
                            },
                            super::parser::Assignment::Expr(expr) => {
                                match (val, eval_expr(expr, &ctx)) {
                                    (Val::List(list), Val::Int(index)) => {
                                        if index < 0 || index >= list.len() as i64 {
                                            panic!("index out of bounds! {} of {:?}", index, list);
                                        }
                                        val = list.get(index as usize).unwrap();
                                        path.push(StringOrInt::Int(index as usize));
                                    }
                                    (Val::Dict(dict), Val::String(index)) => {
                                        val = dict.get(&index).unwrap();
                                        path.push(StringOrInt::String(index));
                                    }
                                    (val, property) => {
                                        panic!("cannot modify property {:?} of {:?}", property, val)
                                    }
                                }
                            }
                        }
                    }
                    let mut val = ctx.get_mut(var).unwrap();
                    for elem in path {
                        match (val, elem) {
                            (Val::Dict(dict), StringOrInt::String(index)) => val = dict.get_mut(&index).unwrap(),
                            (Val::List(list), StringOrInt::Int(index)) => val = list.get_mut(index).unwrap(),
                            _ => panic!("bug")
                        }
                    }
                    Some(val)
                };
                match op {
                    Some(op) => {
                        if let Some(val) = val {
                            *val = handle_operator(val.clone(), op, eval);
                        } else {
                            panic!("variable {} does not exist, cannot update", var)
                        }
                    }
                    None => {
                        if assignment_parts.len() == 0 {
                            ctx.insert(var.clone(), eval);
                        } else {
                            *(val).expect("bug") = eval;
                        }
                    }
                }
            }
            Statement::Expr(expr) => todo!(),
            Statement::Continue => return Some(ControlFlow::Continue),
            Statement::Break => return Some(ControlFlow::Break),
            Statement::Pass => {}
        }
    }
    None
}


fn handle_operator(a : Val, op : &super::parser::BinOp, b: Val) -> Val {
    use super::parser::BinOp::*;
    use super::lexer::Op::*;
    match (a, op, b) {
        (Val::Int(a), Op(Add), Val::Int(b)) => Val::Int(a + b),
        (Val::Int(a), Op(Sub), Val::Int(b)) => Val::Int(a - b),
        (Val::Int(a), Op(Mul), Val::Int(b)) => Val::Int(a * b),
        (Val::Int(a), Op(Div), Val::Int(b)) => Val::Int(a / b),
        (Val::Float(a), Op(Add), Val::Float(b)) => Val::Float(a + b),
        (Val::Float(a), Op(Sub), Val::Float(b)) => Val::Float(a - b),
        (Val::Float(a), Op(Mul), Val::Float(b)) => Val::Float(a * b),
        (Val::Float(a), Op(Div), Val::Float(b)) => Val::Float(a / b),
        (Val::String(a), Op(Add), Val::String(b)) => Val::String(a + &b),
        (Val::Bool(a), And, Val::Bool(b)) => Val::Bool(a && b),
        (Val::Bool(a), Or, Val::Bool(b)) => Val::Bool(a || b),
        (Val::Int(a), Op(Range), Val::Int(b)) => Val::Range(a, b),
        _ => panic!("invalid operation"),
    }
}