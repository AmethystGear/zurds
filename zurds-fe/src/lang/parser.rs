use std::collections::VecDeque;

use super::lexer;
#[derive(Debug)]
pub enum SingleOp {
    Not,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp {
    Op(lexer::Op),
    And,
    Or,
}

#[derive(Debug)]
pub enum Op {
    BinOp(BinOp),
    SingleOp(SingleOp),
}

#[derive(Debug)]
pub enum Expr {
    Literal(lexer::Literal),
    Call(lexer::Ident, Vec<Expr>),
    List(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    SingleOp(SingleOp, Box<Expr>),
    Var(lexer::Ident),
}

#[derive(Debug)]
pub enum Assignment {
    Ident(lexer::Ident),
    Expr(Expr),
}


#[derive(Debug)]
pub enum Statement {
    If(Expr, Vec<Statement>, Vec<Statement>),
    For(lexer::Ident, Expr, Vec<Statement>),
    Assign(lexer::Ident, Vec<Assignment>, Option<BinOp>, Expr),
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

pub fn next(
    tokens: &mut impl Iterator<Item = lexer::Token>,
    buf: &mut Vec<lexer::Token>,
) -> Option<lexer::Token> {
    if buf.is_empty() {
        tokens.next()
    } else {
        buf.pop()
    }
}

pub struct Tokens<T: Iterator<Item = lexer::Token>> {
    tokens: T,
    buf: VecDeque<lexer::Token>,
}

impl<T: Iterator<Item = lexer::Token>> Tokens<T> {
    pub fn next(&mut self) -> Option<lexer::Token> {
        if self.buf.is_empty() {
            self.tokens.next()
        } else {
            self.buf.pop_front()
        }
    }

    pub fn peek(&mut self) -> Option<&lexer::Token> {
        if self.buf.is_empty() {
            let token = self.tokens.next();
            if let Some(token) = token {
                self.buf.push_back(token);
            }
        }
        self.buf.get(0)
    }

    pub fn peek_nth(&mut self, n: usize) -> Option<&lexer::Token> {
        while self.buf.len() <= n {
            let token = self.tokens.next();
            if let Some(token) = token {
                self.buf.push_back(token);
            } else {
                return None;
            }
        }
        self.buf.get(n)
    }

    pub fn new(tokens: T) -> Self {
        Self {
            tokens,
            buf: VecDeque::new(),
        }
    }
}

fn parse_primary<T: Iterator<Item = lexer::Token>>(tokens: &mut Tokens<T>) -> Expr {
    match tokens.next() {
        Some(token) => match token {
            lexer::Token::Literal(literal) => Expr::Literal(literal),
            lexer::Token::Ident(ident) => match tokens.peek() {
                Some(lexer::Token::Punc(lexer::Punc::ParenLeft)) => {
                    tokens.next();
                    let mut args = vec![];
                    loop {
                        match tokens.peek() {
                            Some(lexer::Token::Punc(lexer::Punc::ParenRight)) => {
                                tokens.next();
                                break;
                            }
                            None => panic!("err"),
                            _ => {
                                args.push(parse_expr(tokens));
                                match tokens.peek() {
                                    Some(lexer::Token::Punc(lexer::Punc::ParenRight)) => {}
                                    Some(lexer::Token::Punc(lexer::Punc::Comma)) => {
                                        tokens.next();
                                    }
                                    _ => panic!("err"),
                                }
                            }
                        }
                    }
                    Expr::Call(ident, args)
                }
                _ => Expr::Var(ident),
            },
            lexer::Token::Kw(lexer::Kw::Not) => {
                Expr::SingleOp(SingleOp::Not, Box::new(parse_primary(tokens)))
            }
            lexer::Token::Punc(lexer::Punc::ParenLeft) => {
                let expr = parse_expr(tokens);
                match tokens.next() {
                    Some(lexer::Token::Punc(lexer::Punc::ParenRight)) => expr,
                    _ => panic!("err"),
                }
            }
            lexer::Token::Punc(lexer::Punc::SqrBracketLeft) => {
                let mut args = vec![];
                loop {
                    match tokens.peek() {
                        Some(lexer::Token::Punc(lexer::Punc::SqrBracketRight)) => {
                            tokens.next();
                            break;
                        }
                        None => panic!("err"),
                        _ => {
                            args.push(parse_expr(tokens));
                            match tokens.peek() {
                                Some(lexer::Token::Punc(lexer::Punc::SqrBracketRight)) => {}
                                Some(lexer::Token::Punc(lexer::Punc::Comma)) => {
                                    tokens.next();
                                }
                                _ => panic!("err"),
                            }
                        }
                    }
                }
                Expr::List(args)
            }
            lexer::Token::Punc(lexer::Punc::BracketLeft) => {
                let mut args = vec![];
                loop {
                    match tokens.peek() {
                        Some(lexer::Token::Punc(lexer::Punc::BracketRight)) => {
                            tokens.next();
                            break;
                        }
                        None => panic!("err"),
                        _ => {
                            let key = parse_expr(tokens);
                            match tokens.next() {
                                Some(lexer::Token::Punc(lexer::Punc::Colon)) => {}
                                _ => panic!("expected colon ':'"),
                            }
                            let value = parse_expr(tokens);
                            args.push((key, value));
                            match tokens.peek() {
                                Some(lexer::Token::Punc(lexer::Punc::BracketRight)) => {}
                                Some(lexer::Token::Punc(lexer::Punc::Comma)) => {
                                    tokens.next();
                                }
                                _ => panic!("err"),
                            }
                        }
                    }
                }
                Expr::Dict(args)
            }
            x => panic!("{:?}", x),
        },
        None => panic!(),
    }
}

enum ExprOrBinOp {
    Expr(Expr),
    BinOp(BinOp),
}

fn parse_primary_operator_list<T: Iterator<Item = lexer::Token>>(
    tokens: &mut Tokens<T>,
) -> Vec<ExprOrBinOp> {
    let mut mix = vec![];
    loop {
        let primary = parse_primary(tokens);
        mix.push(ExprOrBinOp::Expr(primary));
        let binop = match tokens.peek() {
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
    mix
}

fn parse_expr_from_primary_operator_list(mut mix: Vec<ExprOrBinOp>) -> Expr {
    loop {
        if mix.len() == 1 {
            if let ExprOrBinOp::Expr(expr) = mix.pop().unwrap() {
                return expr;
            } else {
                panic!("bug");
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
                panic!("bug")
            })
            .min_by_key(|(_, x)| *x)
            .unwrap();
        if i % 2 == 0 {
            panic!("bug")
        }

        let mut new_mix = vec![];
        let mut collapse = vec![];
        for (j, expr_or_binop) in mix.into_iter().enumerate() {
            if j == i - 1 || j == i || j == i + 1 {
                collapse.push(expr_or_binop);
                if j == i + 1 {
                    let mut c = collapse.into_iter();
                    match (c.next().unwrap(), c.next().unwrap(), c.next().unwrap()) {
                        (
                            ExprOrBinOp::Expr(lhs),
                            ExprOrBinOp::BinOp(bin_op),
                            ExprOrBinOp::Expr(rhs),
                        ) => {
                            new_mix.push(ExprOrBinOp::Expr(Expr::BinOp(
                                Box::new(lhs),
                                bin_op,
                                Box::new(rhs),
                            )));
                        }
                        _ => panic!("bug"),
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

fn parse_expr<T: Iterator<Item = lexer::Token>>(tokens: &mut Tokens<T>) -> Expr {
    let mix = parse_primary_operator_list(tokens);
    parse_expr_from_primary_operator_list(mix)
}

pub fn parse<T: Iterator<Item = lexer::Token>>(
    expected_indentation: usize,
    tokens: &mut Tokens<T>,
) -> (Vec<Statement>, usize) {
    let mut statements: Vec<Statement> = vec![];
    let mut indentation = 0;
    loop {
        let mut real_indentation = 0;
        loop {
            match tokens.peek_nth(real_indentation) {
                Some(lexer::Token::Indent) => {
                    real_indentation += 1;
                }
                _ => break,
            }
        }
        println!("{:?}", tokens.buf);
        println!(
            "ind: {} {} {:?}",
            real_indentation,
            expected_indentation,
            tokens.peek_nth(real_indentation)
        );
        if real_indentation < expected_indentation {
            indentation = expected_indentation - real_indentation;
            break;
        } else if real_indentation > expected_indentation {
            panic!("invalid indentation")
        } else {
            for _ in 0..real_indentation {
                tokens.next();
            }
        }
        println!(
            "ind: {} {} {:?}",
            real_indentation,
            expected_indentation,
            tokens.peek()
        );
        println!("{:?}", tokens.buf);
        match tokens.peek() {
            None => break,
            Some(lexer::Token::NewLine) => {
                tokens.next();
                continue;
            }
            Some(lexer::Token::Indent) => panic!("bug"),
            _ => {
                let (statement, statement_indentation) =
                    parse_statement(expected_indentation, tokens);
                statements.push(statement);
                if statement_indentation > 0 {
                    indentation = statement_indentation;
                    break;
                }
            }
        };
    }
    (statements, indentation)
}

fn parse_statement<T: Iterator<Item = lexer::Token>>(
    indentation: usize,
    tokens: &mut Tokens<T>,
) -> (Statement, usize) {
    let mut expect_newline = true;

    let statement = match tokens.peek() {
        Some(lexer::Token::Indent | lexer::Token::NewLine) => panic!("bug"),
        Some(lexer::Token::Kw(kw)) => match kw {
            lexer::Kw::Pass => {
                tokens.next();
                (Statement::Pass, 0)
            }
            lexer::Kw::Break => {
                tokens.next();
                (Statement::Break, 0)
            }
            lexer::Kw::Continue => {
                tokens.next();
                (Statement::Continue, 0)
            }
            lexer::Kw::If => {
                expect_newline = false;
                tokens.next();
                let condition = parse_expr(tokens);
                match tokens.next() {
                    Some(lexer::Token::Punc(lexer::Punc::Colon)) => {}
                    _ => panic!("expected colon ':'"),
                }
                match tokens.next() {
                    Some(lexer::Token::NewLine) => {}
                    _ => panic!("expected enter/newline"),
                }
                let (true_case, n) = parse(indentation + 1, tokens);
                let real_indentation = (indentation + 1) - n;
                match tokens.peek_nth(real_indentation) {
                    Some(lexer::Token::Kw(lexer::Kw::Else)) => {
                        if n != 1 {
                            panic!("bad indentation {}", n)
                        }
                        for _ in 0..=real_indentation {
                            tokens.next();
                        }
                        match tokens.next() {
                            Some(lexer::Token::Punc(lexer::Punc::Colon)) => {}
                            _ => panic!("expected colon ':'"),
                        }
                        match tokens.next() {
                            Some(lexer::Token::NewLine) => {}
                            _ => panic!("expected enter/newline"),
                        }
                        let (false_case, n) = parse(indentation + 1, tokens);
                        println!("{}if n", n);
                        (Statement::If(condition, true_case, false_case), n - 1)
                    }
                    _ => (Statement::If(condition, true_case, vec![]), n - 1),
                }
            }
            lexer::Kw::For => {
                expect_newline = false;
                tokens.next();
                let var = if let Some(lexer::Token::Ident(x)) = tokens.next() {
                    x
                } else {
                    panic!("expected variable name")
                };
                match tokens.next() {
                    Some(lexer::Token::Kw(lexer::Kw::In)) => {}
                    _ => panic!("expected 'in'"),
                }
                let expr = parse_expr(tokens);
                match tokens.next() {
                    Some(lexer::Token::Punc(lexer::Punc::Colon)) => {}
                    _ => panic!("expected colon ':'"),
                }
                match tokens.next() {
                    Some(lexer::Token::NewLine) => {}
                    _ => panic!("expected enter/newline"),
                }
                let (block, n) = parse(indentation + 1, tokens);
                println!("{}for n", n);
                (Statement::For(var, expr, block), n - 1)
            }
            x => todo!("{:?}", x),
        },
        Some(_) => {
            let mut mix = parse_primary_operator_list(tokens);
            let mut assignment_operators = mix
                .iter()
                .enumerate()
                .filter_map(|(i, elem)| match elem {
                    ExprOrBinOp::BinOp(BinOp::Op(op)) => match op {
                        lexer::Op::Assign => Some((i, None)),
                        lexer::Op::AddAssign => Some((i, Some(BinOp::Op(lexer::Op::Add)))),
                        lexer::Op::SubAssign => Some((i, Some(BinOp::Op(lexer::Op::Sub)))),
                        lexer::Op::MulAssign => Some((i, Some(BinOp::Op(lexer::Op::Mul)))),
                        lexer::Op::DivAssign => Some((i, Some(BinOp::Op(lexer::Op::Div)))),
                        _ => None,
                    },
                    _ => None,
                })
                .collect::<Vec<_>>();
            if assignment_operators.len() > 1 {
                panic!("cannot have more than one assignment per line!")
            } else if assignment_operators.len() == 1 {
                let mut expr = mix.split_off(assignment_operators[0].0);
                expr.pop();
                let (ident, assigment_parts) = parse_assignment(mix);
                (
                    Statement::Assign(
                        ident,
                        assigment_parts,
                        assignment_operators.pop().unwrap().1,
                        parse_expr_from_primary_operator_list(expr),
                    ),
                    0,
                )
            } else {
                (Statement::Expr(parse_expr(tokens)), 0)
            }
        }
        None => panic!("bug"),
    };
    if expect_newline {
        match tokens.next() {
            Some(lexer::Token::NewLine) => {}
            _ => panic!("expected enter/newline {:?}", statement),
        }
    }
    statement
}

fn parse_assignment(mix: Vec<ExprOrBinOp>) -> (lexer::Ident, Vec<Assignment>) {
    let mut exprs = mix.into_iter().filter_map(|elem| match elem {
        ExprOrBinOp::BinOp(BinOp::Op(lexer::Op::Access)) => None,
        ExprOrBinOp::BinOp(_) => {
            panic!("the only operator allowed in the left side of an assignment is .")
        }
        ExprOrBinOp::Expr(expr) => Some(expr),
    });
    let ident = exprs.next();
    let ident = match ident {
        Some(Expr::Var(ident)) => ident,
        _ => panic!("left side of assignment should start with a variable."),
    };
    let mut assignment_parts = vec![];
    for expr in exprs {
        assignment_parts.push(match expr {
            Expr::Var(ident) => Assignment::Ident(ident),
            Expr::List(mut exprs) => {
                if exprs.len() > 1 {
                    panic!("indexing expression can only have one argument")
                }
                Assignment::Expr(exprs.pop().unwrap())
            }
            _ => panic!(),
        });
    }
    (ident, assignment_parts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr() {
        let tokens = lexer::tokenize("(not is_valid() and opponent != \"harry\" + 5)").unwrap();
        let parsed = parse_expr(&mut Tokens::new(tokens.into_iter()));
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_statement() {
        let program = include_str!("../../example.spell");
        let tokens = lexer::tokenize(program).unwrap();
        let parsed = parse(0, &mut Tokens::new(tokens.into_iter()));
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_dictionary_access() {
        let program = "{'hello' : 'world'}.['hello']";
        let tokens = lexer::tokenize(program).unwrap();
        let parsed = parse(0, &mut Tokens::new(tokens.into_iter()));
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_assignment() {
        let program = "x = 5";
        let tokens = lexer::tokenize(program).unwrap();
        let parsed = parse(0, &mut Tokens::new(tokens.into_iter()));
        println!("{:?}", parsed);
    }
}
