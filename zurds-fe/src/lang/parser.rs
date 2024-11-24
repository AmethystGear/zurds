use super::{
    err::{LangErr, Loc},
    lexer::{self, AssignOp, Token},
};
use std::slice::Iter;

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp {
    Op(lexer::Op),
    And,
    Or,
}

#[derive(Debug)]
pub enum Expr {
    Literal(lexer::Literal),
    Call(lexer::Ident, Vec<Expr>),
    List(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Var(lexer::Ident),
}

#[derive(Debug)]
pub enum Field {
    Var(lexer::Ident),
    Index(Expr),
}

#[derive(Debug)]
pub enum Assign {
    Var(lexer::Ident),
    Field(Expr, Field),
}

#[derive(Debug)]
pub enum Statement {
    If(Expr, Vec<Statement>, Vec<Statement>),
    For(lexer::Ident, Option<lexer::Ident>, Expr, Vec<Statement>),
    Loop(Vec<Statement>),
    Assign(Assign, Option<AssignOp>, Expr),
    Expr(Expr),
    Continue,
    Break,
    Pass,
}

const PRECEDENCE: [BinOp; 15] = [
    BinOp::Op(lexer::Op::Access),
    BinOp::Op(lexer::Op::Range),
    BinOp::Op(lexer::Op::Mul),
    BinOp::Op(lexer::Op::Div),
    BinOp::Op(lexer::Op::Mod),
    BinOp::Op(lexer::Op::Add),
    BinOp::Op(lexer::Op::Sub),
    BinOp::Op(lexer::Op::Eq),
    BinOp::Op(lexer::Op::Neq),
    BinOp::Op(lexer::Op::LThan),
    BinOp::Op(lexer::Op::GThan),
    BinOp::Op(lexer::Op::LThanEq),
    BinOp::Op(lexer::Op::GThanEq),
    BinOp::And,
    BinOp::Or,
];

macro_rules! expect {
    // Case where the pattern has a variable to return
    ($tokens:expr, $pattern:pat => $var:ident, $error_msg:expr) => {
        match $tokens.next() {
            Some(($pattern, _)) => Ok($var),
            Some((_, loc)) => Err(LangErr {
                loc: *loc,
                err: $error_msg.to_string(),
            }),
            None => Err(LangErr {
                loc: Loc {
                    col: 0,
                    line: 0,
                    len: 0,
                },
                err: "unexpected end of file".to_string(),
            }),
        }
    };

    // Case where no variable is bound in the pattern
    ($tokens:expr, $pattern:pat, $error_msg:expr) => {
        match $tokens.next() {
            Some(($pattern, _)) => Ok(()),
            Some((_, loc)) => Err(LangErr {
                loc: *loc,
                err: $error_msg.to_string(),
            }),
            None => Err(LangErr {
                loc: Loc {
                    col: 0,
                    line: 0,
                    len: 0,
                },
                err: "unexpected end of file".to_string(),
            }),
        }
    };
}

fn parse_primary(tokens: &mut Iter<'_, (lexer::Token, Loc)>) -> Result<Expr, LangErr> {
    match tokens.next() {
        Some((token, loc)) => match token {
            lexer::Token::Literal(literal) => Ok(Expr::Literal(literal.clone())),
            lexer::Token::Ident(ident) => match tokens.clone().next().map(|(x, _)| x) {
                Some(lexer::Token::Punc(lexer::Punc::ParenLeft)) => {
                    tokens.next();
                    let mut args = vec![];
                    loop {
                        match tokens.clone().next().map(|(x, _)| x) {
                            Some(lexer::Token::Punc(lexer::Punc::ParenRight)) => {
                                tokens.next();
                                break;
                            }
                            None => Err(LangErr {
                                loc: *loc,
                                err: "unmatched '('".to_string(),
                            })?,
                            _ => {
                                args.push(parse_expr(tokens)?);
                                match tokens.clone().next().map(|(x, _)| x) {
                                    Some(lexer::Token::Punc(lexer::Punc::ParenRight)) => {}
                                    Some(lexer::Token::Punc(lexer::Punc::Comma)) => {
                                        tokens.next();
                                    }
                                    _ => Err(LangErr {
                                        loc: *loc,
                                        err: "expected ')' or ','".to_string(),
                                    })?,
                                }
                            }
                        }
                    }
                    Ok(Expr::Call(ident.clone(), args))
                }
                _ => Ok(Expr::Var(ident.clone())),
            },
            lexer::Token::Punc(lexer::Punc::ParenLeft) => {
                let expr = parse_expr(tokens)?;
                expect!(
                    tokens,
                    lexer::Token::Punc(lexer::Punc::ParenRight),
                    "unmatched '('"
                )?;
                Ok(expr)
            }
            lexer::Token::Punc(lexer::Punc::SqrBracketLeft) => {
                let mut args = vec![];
                loop {
                    match tokens.clone().next().map(|(x, _)| x) {
                        Some(lexer::Token::Punc(lexer::Punc::SqrBracketRight)) => {
                            tokens.next();
                            break;
                        }
                        None => Err(LangErr {
                            loc: *loc,
                            err: "unmatched '['".to_string(),
                        })?,
                        _ => {
                            args.push(parse_expr(tokens)?);
                            match tokens.clone().next().map(|(x, _)| x) {
                                Some(lexer::Token::Punc(lexer::Punc::SqrBracketRight)) => {}
                                Some(lexer::Token::Punc(lexer::Punc::Comma)) => {
                                    tokens.next();
                                }
                                _ => Err(LangErr {
                                    loc: *loc,
                                    err: "expected ']' or ','".to_string(),
                                })?,
                            }
                        }
                    }
                }
                Ok(Expr::List(args))
            }
            lexer::Token::Punc(lexer::Punc::BracketLeft) => {
                let mut args = vec![];
                loop {
                    match tokens.clone().next().map(|(x, _)| x) {
                        Some(lexer::Token::Punc(lexer::Punc::BracketRight)) => {
                            tokens.next();
                            break;
                        }
                        None => Err(LangErr {
                            loc: *loc,
                            err: "unmatched '{'".to_string(),
                        })?,
                        _ => {
                            let key = parse_expr(tokens)?;
                            expect!(
                                tokens,
                                lexer::Token::Punc(lexer::Punc::Colon),
                                "expected ':' between key and value in dict"
                            )?;
                            let value = parse_expr(tokens)?;
                            args.push((key, value));
                            match tokens.clone().next().map(|(x, _)| x) {
                                Some(lexer::Token::Punc(lexer::Punc::BracketRight)) => {}
                                Some(lexer::Token::Punc(lexer::Punc::Comma)) => {
                                    tokens.next();
                                }
                                _ => Err(LangErr {
                                    loc: *loc,
                                    err: "expected '}' or ','".to_string(),
                                })?,
                            }
                        }
                    }
                }
                Ok(Expr::Dict(args))
            }
            x => Err(LangErr {
                loc: *loc,
                err: format!("expected expression here, found {:?}", x),
            })?,
        },
        None => panic!(),
    }
}

enum ExprOrBinOp {
    Expr(Expr),
    BinOp(BinOp),
}

fn parse_primary_operator_list<'a>(
    tokens: &mut Iter<'_, (lexer::Token, Loc)>,
) -> Result<Vec<ExprOrBinOp>, LangErr> {
    let mut mix = vec![];
    loop {
        let primary = parse_primary(tokens)?;
        mix.push(ExprOrBinOp::Expr(primary));
        let binop = match tokens.clone().next().map(|(x, _)| x) {
            Some(x) => match x {
                lexer::Token::Op(op) => Some(BinOp::Op(op.clone())),
                lexer::Token::Kw(lexer::Kw::And) => Some(BinOp::And),
                lexer::Token::Kw(lexer::Kw::Or) => Some(BinOp::Or),
                _ => None,
            },
            None => None,
        };
        if let Some(binop) = binop {
            mix.push(ExprOrBinOp::BinOp(binop));
            tokens.next();
        } else {
            break;
        }
    }
    Ok(mix)
}

fn parse_expr_from_primary_operator_list(mut mix: Vec<ExprOrBinOp>) -> Result<Expr, LangErr> {
    loop {
        if mix.len() == 1 {
            if let ExprOrBinOp::Expr(expr) = mix.pop().unwrap() {
                return Ok(expr);
            } else {
                panic!("BUG! first element in mix should always be an expression");
            }
        }
        let (i, _) = mix
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match x {
                ExprOrBinOp::Expr(_) => None,
                ExprOrBinOp::BinOp(bin_op) => Some((i, bin_op)),
            })
            .map(|(i, x)| {
                let mut j = 0;
                for elem in &PRECEDENCE {
                    if elem == x {
                        return (i, j);
                    }
                    j += 1;
                }
                panic!("BUG! unhandled BinOp")
            })
            .min_by_key(|(_, x)| *x)
            .unwrap();
        if i % 2 == 0 {
            panic!("BUG! binops should always be at odd indices");
        }

        let mut new_mix = vec![];
        let mut collapse = vec![];
        for (j, expr_or_binop) in mix.into_iter().enumerate() {
            if j == i - 1 || j == i || j == i + 1 {
                collapse.push(expr_or_binop);
                if j == i + 1 {
                    let mut c = collapse.into_iter();
                    match (c.next(), c.next(), c.next()) {
                        (
                            Some(ExprOrBinOp::Expr(lhs)),
                            Some(ExprOrBinOp::BinOp(bin_op)),
                            Some(ExprOrBinOp::Expr(rhs)),
                        ) => {
                            new_mix.push(ExprOrBinOp::Expr(Expr::BinOp(
                                Box::new(lhs),
                                bin_op,
                                Box::new(rhs),
                            )));
                        }
                        (Some(ExprOrBinOp::Expr(_)), Some(ExprOrBinOp::BinOp(_)), None) => {
                            panic!("bug")
                        }
                        _ => panic!("BUG! "),
                    }
                    collapse = vec![];
                }
                continue;
            }
            new_mix.push(expr_or_binop);
        }
        mix = new_mix;
    }
}

fn parse_expr<'a>(tokens: &mut Iter<'_, (lexer::Token, Loc)>) -> Result<Expr, LangErr> {
    let mix = parse_primary_operator_list(tokens)?;
    parse_expr_from_primary_operator_list(mix)
}

pub fn parse(tokens: &mut Iter<'_, (lexer::Token, Loc)>) -> Result<Vec<Statement>, LangErr> {
    _parse(0, tokens).map(|(statements, _)| statements)
}

fn _parse(
    expected_indentation: usize,
    tokens: &mut Iter<'_, (lexer::Token, Loc)>,
) -> Result<(Vec<Statement>, usize), LangErr> {
    let mut statements: Vec<Statement> = vec![];
    let mut indentation = 0;
    loop {
        let mut real_indentation = 0;
        loop {
            match tokens.clone().skip(real_indentation).next().map(|(x, _)| x) {
                Some(lexer::Token::Indent) => {
                    real_indentation += 1;
                }
                _ => break,
            }
        }
        if real_indentation < expected_indentation {
            indentation = expected_indentation - real_indentation;
            break;
        } else if real_indentation > expected_indentation {
            Err(LangErr {
                loc: *tokens.next().map(|(_, x)| x).unwrap(),
                err: "invalid indentation".to_string(),
            })?
        } else {
            for _ in 0..real_indentation {
                tokens.next();
            }
        }
        match tokens.clone().next().map(|(x, _)| x) {
            None => break,
            Some(lexer::Token::NewLine) => {
                tokens.next();
                continue;
            }
            Some(lexer::Token::Indent) => panic!("bug"),
            _ => {
                let (statement, statement_indentation) =
                    parse_statement(expected_indentation, tokens)?;
                statements.push(statement);
                if statement_indentation > 0 {
                    indentation = statement_indentation;
                    break;
                }
            }
        };
    }
    Ok((statements, indentation))
}

fn parse_statement(
    indentation: usize,
    mut tokens: &mut Iter<'_, (lexer::Token, Loc)>,
) -> Result<(Statement, usize), LangErr> {
    let mut expect_newline = true;
    let statement = match tokens.clone().next() {
        Some(token_loc) => {
            let mut assignment = false;
            let mut lineloc = Loc {
                col: 0,
                line: 0,
                len: 0,
            };
            for (i, (token, loc)) in std::iter::once(token_loc)
                .chain(&mut tokens.clone())
                .enumerate()
            {
                if i == 0 {
                    lineloc.col = loc.col;
                    lineloc.line = loc.line;
                }
                lineloc.len += loc.len;
                match token {
                    Token::NewLine => break,
                    Token::Assign(op) => {
                        if assignment {
                            Err(LangErr {
                                loc: *loc,
                                err: format!(
                                    "Multiple assignments on a single line. \
                                Did you mean to use '==' (check for equality) instead of '{}='? \
                                If not, split the assignments onto multiple lines.",
                                    match op {
                                        Some(op) => match op {
                                            lexer::AssignOp::Add => "+",
                                            lexer::AssignOp::Sub => "-",
                                            lexer::AssignOp::Mul => "*",
                                            lexer::AssignOp::Div => "/",
                                        },
                                        None => "",
                                    }
                                ),
                            })?
                        } else {
                            assignment = true;
                        }
                    }
                    _ => {}
                }
            }
            if assignment {
                let assignment_expr = parse_expr(&mut tokens)?;
                let err = Err(LangErr {
                    loc: lineloc,
                    err: "expected assignment to be like 'somevar = <...>' or '<...>.somefield = <...>'  or '<...>.[<some expression>] = <...>'".to_string(),
                });
                let assign: Assign = match assignment_expr {
                    Expr::BinOp(left, BinOp::Op(lexer::Op::Access), right) => match *right {
                        Expr::List(mut vec) => match (vec.pop(), vec.pop()) {
                            (Some(expr), None) => Ok(Assign::Field(*left, Field::Index(expr))),
                            _ => err,
                        },
                        Expr::Var(ident) => Ok(Assign::Field(*left, Field::Var(ident))),
                        _ => err,
                    },
                    Expr::Var(ident) => Ok(Assign::Var(ident)),
                    _ => err,
                }?;
                let operator = expect!(tokens, Token::Assign(x) => x,
                    "expected one of '=,+=,-=,*=,/=' here... do you have multiple expressions before the '='?")?;
                let evaluation_expr = parse_expr(&mut tokens)?;
                (
                    Statement::Assign(assign, operator.clone(), evaluation_expr),
                    0,
                )
            } else {
                let (token, loc) = token_loc;
                match token {
                    lexer::Token::Kw(lexer::Kw::And | lexer::Kw::Or) => None,
                    lexer::Token::Kw(_) => tokens.next(),
                    _ => None,
                };
                match token {
                    lexer::Token::Kw(kw) => match kw {
                        lexer::Kw::Pass => (Statement::Pass, 0),
                        lexer::Kw::Break => (Statement::Break, 0),
                        lexer::Kw::Continue => (Statement::Continue, 0),
                        lexer::Kw::If => {
                            expect_newline = false;
                            let condition = parse_expr(tokens)?;
                            expect!(tokens, lexer::Token::Punc(lexer::Punc::Colon), "expected ':' at the end of if condition")?;
                            expect!(tokens, lexer::Token::NewLine, "expected newline/enter after ':'")?;
                            let (true_case, n) = _parse(indentation + 1, tokens)?;
                            let real_indentation = (indentation + 1) - n;
                            match tokens.clone().skip(real_indentation).next() {
                                Some((lexer::Token::Kw(lexer::Kw::Else), loc)) => {
                                    if real_indentation != indentation {
                                        Err(LangErr {
                                            loc : *loc,
                                            err : "`else` is incorrectly indented. make sure it's at the same indentation level as the `if`.".to_string()
                                        })?;
                                    }
                                    for _ in 0..=real_indentation {
                                        tokens.next();
                                    }
                                    expect!(tokens, lexer::Token::Punc(lexer::Punc::Colon), "expected ':' after `else`")?;
                                    expect!(tokens, lexer::Token::NewLine, "expected newline/enter after ':'")?;
                                    let (false_case, n) = _parse(indentation + 1, tokens)?;
                                    (Statement::If(condition, true_case, false_case), n - 1)
                                }
                                _ => (Statement::If(condition, true_case, vec![]), n - 1),
                            }
                        }
                        lexer::Kw::For => {
                            expect_newline = false;
                            let var = expect!(tokens, lexer::Token::Ident(x) => x, "expected variable, e.g. `for x in 0..10:`...")?;
                            let second_var = match tokens.clone().next() {
                                Some((lexer::Token::Punc(lexer::Punc::Comma), _)) => {
                                    Some(expect!(tokens, lexer::Token::Ident(x) => x, "expected variable after comma, e.g. `for x, y in dictionary:`...")?.clone())
                                },
                                _ => None,
                            };
                            expect!(tokens, lexer::Token::Kw(lexer::Kw::In), "expected 'in', e.g. `for x in 0..10:`...")?;
                            let expr = parse_expr(tokens)?;
                            expect!(tokens, lexer::Token::Punc(lexer::Punc::Colon), "expected ':' after loop expression, e.g. `for x in 0..10:`...")?;
                            expect!(tokens, lexer::Token::NewLine, "expected newline/enter after ':'")?;
                            let (block, n) = _parse(indentation + 1, tokens)?;
                            (Statement::For(var.to_string(), second_var, expr, block), n - 1)
                        }
                        lexer::Kw::Loop => {
                            expect_newline = false;
                            expect!(tokens, lexer::Token::Punc(lexer::Punc::Colon), "expected ':' after loop, e.g. `loop:`...")?;
                            expect!(tokens, lexer::Token::NewLine, "expected newline/enter after ':'")?;
                            let (block, n) = _parse(indentation + 1, tokens)?;
                            (Statement::Loop(block), n - 1)
                        },
                        lexer::Kw::Else => {
                            Err(LangErr {
                                loc: *loc,
                                err: "Found an `else`, but it doesn't seem to be attached to an if statement. \
                                Check your indentation (if and else should be indented the same amount), \
                                and make sure there is an `if` before the `else`.".to_string()
                            })?
                        },
                        lexer::Kw::In => {
                            Err(LangErr {
                                loc: *loc,
                                err: "Found an `in` but it doesn't seem to be part of a for loop.".to_string()
                            })?
                        },
                        lexer::Kw::And | lexer::Kw::Or => (Statement::Expr(parse_expr(&mut tokens)?), 0)
                    },
                    _ => (Statement::Expr(parse_expr(&mut tokens)?), 0)
                }
            }
        }
        None => panic!("bug"),
    };
    if expect_newline {
        expect!(
            tokens,
            lexer::Token::NewLine,
            "All statements should end with a newline. Consider moving this code to the next line?"
        )?;
    }
    Ok(statement)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr() {
        let tokens = lexer::tokenize("(is_valid().not() and opponent != \"harry\" + 5)").unwrap();
        println!("{:?}", tokens);
        let parsed = parse_expr(&mut tokens.iter());
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_statement() {
        let program = include_str!("../../test-spells/example.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let parsed = parse(&mut tokens.iter());
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_dictionary_access_and_assignment() {
        let program = "{'hello' : {'blah' : [10, 20]}}.['hello'].blah.[0] *= 3";
        let tokens = lexer::tokenize(program).unwrap();
        let parsed = parse(&mut tokens.iter());
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_assignment() {
        let program = "x = 5";
        let tokens = lexer::tokenize(program).unwrap();
        let parsed = parse(&mut tokens.iter());
        println!("{:?}", parsed);
    }
}
