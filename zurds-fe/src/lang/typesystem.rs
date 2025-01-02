use std::{
    cell::RefCell,
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::lang::{
    lexer::{self, Literal},
    parser::BinOp,
};

use super::{lexer::AssignOp, parser::{Assign, Expr, Field, Statement}};

type TypeRef = Rc<RefCell<Type>>;

fn rf<T>(t: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(t))
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum Type {
    Variant(Vec<TypeRef>),
    Container(Container),
    Int,
    Flt,
    Bool,
    Str,
    Range,
    Unit,
    Any,
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
    Map(TypeRef, TypeRef),            // map from type -> type
    List(TypeRef),                    // list of type
    Tuple(Vec<TypeRef>),              // tuple of types
    Opt(TypeRef),                     // either has an object of type or doesn't
    Res(TypeRef, TypeRef),            // either has an object of first type or one of the second.
    Struct(HashMap<String, TypeRef>), // a struct
}

#[derive(Debug)]
struct TypeError(String);

fn get_concrete_method_defs() -> [(&'static str, Type, Vec<Type>, Type); 8] {
    [
        ("round", Type::Flt, vec![], Type::Flt),
        ("ciel", Type::Flt, vec![], Type::Flt),
        ("floor", Type::Flt, vec![], Type::Flt),
        (
            "flt",
            Type::Str,
            vec![],
            Type::Container(Container::Res(rf(Type::Flt), rf(Type::Str))),
        ),
        ("flt", Type::Int, vec![], Type::Flt),
        (
            "int",
            Type::Str,
            vec![],
            Type::Container(Container::Res(rf(Type::Int), rf(Type::Str))),
        ),
        ("int", Type::Flt, vec![], Type::Int),
        ("contains", Type::Str, vec![Type::Str], Type::Bool),
    ]
}

fn expect_args(args: &[TypeRef], expected: &[Type]) -> Result<(), TypeError> {
    Ok(())
}

fn resolve_method_type(
    t: TypeRef,
    method: &str,
    args_exprs: &Vec<Expr>,
    ctx: &mut HashMap<String, TypeRef>,
) -> Result<Type, TypeError> {
    let args: Result<Vec<TypeRef>, _> = args_exprs
        .iter()
        .map(|expr| get_expr_type(expr, ctx))
        .collect();
    let args = args?;
    use Container::*;

    let mut modified_type = None;
    let return_type = {
        let t = t.borrow();
        let t = t.deref();
        match (t, method) {
            (_, "str") => {
                expect_args(&args, &[])?;
                Ok(Type::Str)
            }
            (Type::Container(List(_)), "remove") => {
                expect_args(&args, &[Type::Int])?;
                Ok(Type::Unit)
            }
            (Type::Container(List(x)), "add") => {
                expect_args(&args, &[Type::Any])?;
                modified_type = Some(Type::Container(List(rf(Type::Variant(vec![
                    args[0].clone(),
                    x.clone(),
                ])))));
                Ok(Type::Unit)
            }
            (Type::Container(Map(k, _)), "contains_key") => {
                expect_args(&args, &[k.borrow().clone()])?;
                Ok(Type::Bool)
            }
            (Type::Unit, "insert") => {
                expect_args(&args, &[Type::Any, Type::Any])?;
                modified_type = Some(Type::Container(Map(args[0].clone(), args[1].clone())));
                Ok(Type::Unit)
            }
            (Type::Container(Map(k, _)), "remove") => {
                expect_args(&args, &[k.borrow().clone()])?;
                Ok(Type::Unit)
            }
            (Type::Container(Map(k, v)), "insert") => {
                expect_args(&args, &[Type::Any, Type::Any])?;
                modified_type = Some(Type::Container(Map(
                    rf(Type::Variant(vec![args[0].clone(), k.clone()])),
                    rf(Type::Variant(vec![args[1].clone(), v.clone()])),
                )));
                Ok(Type::Unit)
            }
            (Type::Container(Struct(fields)), "insert") => {
                expect_args(&args, &[Type::Str, Type::Any])?;
                let field = match &args_exprs[0] {
                    Expr::Literal(Literal::String(s)) => Ok(s),
                    _ => Err(TypeError(
                        "when inserting fields into a structure, you must use static strings."
                            .into(),
                    )),
                }?;
                let mut fields = fields.clone();
                fields.insert(field.clone(), args[1].clone());
                modified_type = Some(Type::Container(Struct(fields)));
                Ok(Type::Unit)
            }
            (Type::Container(Struct(fields)), "remove") => {
                expect_args(&args, &[Type::Str])?;
                let field = match &args_exprs[0] {
                    Expr::Literal(Literal::String(s)) => Ok(s),
                    _ => Err(TypeError(
                        "when removing fields from a structure, you must use static strings."
                            .into(),
                    )),
                }?;
                let mut fields = fields.clone();
                if fields.contains_key(field) {
                    fields.remove(field);
                } else {
                    Err(TypeError(format!(
                        "structure does not have field `{}`",
                        field
                    )))?;
                }
                modified_type = Some(Type::Container(Struct(fields)));
                Ok(Type::Unit)
            }
            (t, method) => {
                for (name, ty, expected_args, out) in get_concrete_method_defs() {
                    if &ty == t && name == method {
                        expect_args(&args, &expected_args)?;
                        return Ok(out);
                    }
                }
                Err(TypeError(format!(
                    "there is no function named {:?} for type {:?}",
                    method, t
                )))
            }
        }
    };
    if let Some(modified_type) = modified_type {
        let mut t = t.borrow_mut();
        let t = t.deref_mut();
        *t = modified_type;
    }
    return_type
}

fn get_concrete_fn_defs() -> [(&'static str, Vec<Type>, Type); 4] {
    [
        ("print", vec![Type::Str], Type::Unit),
        ("err", vec![Type::Str], Type::Unit),
        ("not", vec![Type::Bool], Type::Bool),
        ("exit", vec![], Type::Unit),
    ]
}

fn resolve_function_type(function: &str, args: Vec<TypeRef>) -> Result<Type, TypeError> {
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

pub fn get_expr_type(
    expr: &Expr,
    ctx: &mut HashMap<String, TypeRef>,
) -> Result<TypeRef, TypeError> {
    use Container::*;
    match expr {
        Expr::Literal(literal) => Ok(Rc::new(RefCell::new(match literal {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Flt,
            Literal::Bool(_) => Type::Bool,
            Literal::String(_) => Type::Str,
        }))),
        Expr::Call(function, args) => {
            let args: Result<Vec<TypeRef>, _> =
                args.iter().map(|expr| get_expr_type(expr, ctx)).collect();
            resolve_function_type(function, args?).map(rf)
        }
        Expr::List(vec) => {
            let types: Result<Vec<TypeRef>, _> =
                vec.iter().map(|expr| get_expr_type(expr, ctx)).collect();
            let types = types?;
            Ok(rf(Type::Container(List(rf(Type::Variant(types))))))
        }
        Expr::Dict(vec) => {
            if vec.is_empty() {
                return Ok(rf(Type::Unit));
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
                Ok(rf(Type::Container(Struct(known_fields))))
            } else {
                let kvs = vec
                    .iter()
                    .map(|(k, v)| (get_expr_type(k, ctx), get_expr_type(v, ctx)));

                let mut keys = vec![];
                let mut values = vec![];
                for (k, v) in kvs {
                    keys.push(k?);
                    values.push(v?);
                }
                Ok(rf(Type::Container(Map(
                    rf(Type::Variant(keys)),
                    rf(Type::Variant(values)),
                ))))
            }
        }
        Expr::BinOp(left, binop, right) => match binop {
            BinOp::Op(lexer::Op::Access) => match right.as_ref() {
                Expr::Call(method, args_exprs) => {
                    let left_type = get_expr_type(left, ctx)?;
                    resolve_method_type(left_type, method, args_exprs, ctx).map(rf)
                }
                Expr::Var(field) => {
                    let left_type = get_expr_type(left, ctx)?;
                    let left_type = left_type.borrow();
                    let left_type = left_type.deref();
                    match left_type {
                        Type::Container(Struct(fields)) => {
                            if let Some(entry) = fields.get(field) {
                                Ok(entry.clone())
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
                    let index = if let (Some(index), None) = (vec.get(0), vec.get(1)) {
                        index
                    } else {
                        return Err(TypeError("invalid indexing".into()));
                    };
                    let left_type = get_expr_type(left, ctx)?;
                    let left_type = left_type.borrow();
                    let indexing_type = get_expr_type(index, ctx)?;
                    let indexing_type = indexing_type.borrow();
                    match (&*left_type, &*indexing_type) {
                        (Type::Container(List(val)), Type::Int) => Ok(val.clone()),
                        (Type::Container(Struct(fields)), Type::Str) => match index {
                            Expr::Literal(Literal::String(field)) => {
                                if let Some(entry) = fields.get(field) {
                                    Ok(entry.clone())
                                } else {
                                    Err(TypeError(format!(
                                        "can't access field `{}` of {:?}",
                                        field, left_type
                                    )))
                                }
                            }
                            _ => Err(TypeError(format!(
                                "when indexing into a struct you must use string literals."
                            ))),
                        },
                        (Type::Container(Map(k, v)), field) => {
                            if &*k.borrow() == field {
                                Ok(v.clone())
                            } else {
                                Err(TypeError(format!(
                                    "Map type is {:?}, type of key to retrieve is {:?}",
                                    left_type, k
                                )))
                            }
                        }
                        _ => Err(TypeError("invalid indexing".into())),
                    }
                }
                _ => Err(TypeError("invalid property access".into())),
            },
            op => handle_operator(
                &*get_expr_type(left, ctx)?.borrow(),
                op,
                &*get_expr_type(right, ctx)?.borrow(),
            )
            .map(rf),
        },
        Expr::Var(x) => ctx
            .get(x)
            .ok_or(TypeError(format!("variable '{}' does not exist!", x)))
            .cloned(),
    }
}

pub fn type_eval(
    statements: &Vec<Statement>,
    ctx: &mut HashMap<String, TypeRef>,
) -> Result<(), TypeError> {
    use Container::*;
    for statement in statements {
        match statement {
            Statement::Assign(assign, op, expr) => {
                let right_expr_type = get_expr_type(expr, ctx)?;


                match op {
                    Some(op) => {
                        // update assigns can't change types, so we just have to typecheck the operation itself, no type updates required.
                        let binop = match op {
                            lexer::AssignOp::Add => BinOp::Op(lexer::Op::Add),
                            lexer::AssignOp::Sub => BinOp::Op(lexer::Op::Sub),
                            lexer::AssignOp::Mul => BinOp::Op(lexer::Op::Mul),
                            lexer::AssignOp::Div => BinOp::Op(lexer::Op::Div),
                        };
                        match assign {
                            crate::lang::parser::Assign::Var(_) => todo!(),
                            crate::lang::parser::Assign::Field(expr, field) => todo!(),
                        }
                    },
                    None => {

                    },
                }
            }
            Statement::Expr(expr) => {
                get_expr_type(expr, ctx)?;
            }
            Statement::If(expr, vec, vec1) => todo!(),
            Statement::For(_, _, expr, vec) => todo!(),
            Statement::Loop(vec) => todo!(),
            Statement::Continue => todo!(),
            Statement::Break => todo!(),
            Statement::Pass => todo!(),
        }
    }
    Ok(())
}


pub fn resolve_assign_type(assign : &Assign, ctx: &mut HashMap<String, TypeRef>) -> Result<TypeRef, TypeError> {
    match assign {
        Assign::Var(var) => ctx.get(var).cloned().ok_or(TypeError(format!("variable '{}' does not exist", var))),
        Assign::Field(expr, field) => {
            let left_type = get_expr_type(expr, ctx)?;
            let mut left_type = left_type.borrow_mut();
            let left_type = left_type.deref_mut();
            match (left_type, field) {
                Type::Container(Container::List(_)) => todo!(),
                Type::Container(Container::Map(_, _)) => todo!(),
                (Type::Container(Container::Struct(fields)), Field::Index() => {
                    fields.get()
                },
                _ => Err(TypeError(format!("cannot index into or access fields of '{:?}'", left_type)))
            }
        },
    }
}


enum AssignmentAction<'a> {
    UpdateFieldOrContext(String, &'a mut HashMap<String, TypeRef>),
    UpdateList(TypeRef),

}

fn handle_field_or_ctx_update() -> Result<(), TypeError> {
    match (ctx.get(&field).cloned(), op) {
        (Some(current), Some(op)) => {
            handle_assign_op(current, op, new);
        },
        (_, None) => {
            ctx.insert(field, new);
        },
        (None, Some(_)) => return Err(TypeError("can't do +=,-=,*=,/= because the variable or field is not defined".to_string())),
    }
}

fn handle_assignment(new: TypeRef, action: AssignmentAction<'_>, op: Option<&AssignOp>) -> Result<(), TypeError> {
    match action {
        AssignmentAction::Ctx(field, ctx) => {
            match (ctx.get(&field).cloned(), op) {
                (Some(current), Some(op)) => {
                    handle_assign_op(current, op, new);
                },
                (_, None) => {
                    ctx.insert(field, new);
                },
                (None, Some(_)) => return Err(TypeError("can't do +=,-=,*=,/= because the variable or field is not defined".to_string())),
            }
        }
        AssignmentAction::Union(x) => {
            match op {
                Some(_) =>  {
                    handle_assign_op(current, op, new);
                }
                None => {
                    x.append(new);
                }
            }
        }
    }?;
    Ok(())
}

fn handle_assign_op(
    a: TypeRef,
    op: &AssignOp,
    b: TypeRef,
) -> Result<TypeRef, TypeError> {
    let binop = match op {
        lexer::AssignOp::Add => BinOp::Op(lexer::Op::Add),
        lexer::AssignOp::Sub => BinOp::Op(lexer::Op::Sub),
        lexer::AssignOp::Mul => BinOp::Op(lexer::Op::Mul),
        lexer::AssignOp::Div => BinOp::Op(lexer::Op::Div),
    };
    let a = a.borrow();
    let a = a.deref();
    let b = b.borrow();
    let b = b.deref();
    Ok(Rc::new(RefCell::new(handle_operator(a, &binop, b)?)))
}

fn handle_operator(left: &Type, op: &BinOp, right: &Type) -> Result<Type, TypeError> {
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

#[cfg(test)]
mod tests {
    use crate::lang::parser::{self, Statement};

    use super::*;

    #[test]
    fn test_get_expr_type() {
        let tokens = lexer::tokenize("x.value = 10").unwrap();
        let mut statements = parser::parse(&mut tokens.iter()).unwrap();
        let mut ctx = HashMap::new();
        ctx.insert(
            "x".into(),
            rf(Type::Container(Container::Struct(HashMap::new()))),
        );
        match (statements.pop(), statements.pop()) {
            (Some(Statement::Expr(expr)), None) => {
                println!("{:?}", get_expr_type(&expr, &mut ctx).unwrap());
                println!("{:?}", ctx);
            }
            _ => panic!("expected one expression"),
        }
    }
}
