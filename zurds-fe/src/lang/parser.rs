use super::lexer;
#[derive(Debug)]
enum SingleOp {
    Not,
}

#[derive(Debug, PartialEq, Eq)]
enum BinOp {
    Op(lexer::Op),
    And,
    Or,
}

#[derive(Debug)]
enum Op {
    BinOp(BinOp),
    SingleOp(SingleOp),
}

#[derive(Debug)]
enum Expr {
    Literal(lexer::Literal),
    Call(lexer::Ident, Vec<Expr>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    SingleOp(SingleOp, Box<Expr>),
    Var(lexer::Ident),
}

enum Statement {
    If(Expr, Vec<Statement>, Vec<Statement>),
    For(lexer::Ident, Vec<Statement>, Vec<Statement>),
    Assign(lexer::Ident, Expr),
    Expr(Expr),
    Continue,
    Break,
    Pass,
}

const PRECEDENCE: [BinOp; 14] = [
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

fn next(
    tokens: &mut impl Iterator<Item = lexer::Token>,
    buf: &mut Vec<lexer::Token>,
) -> Option<lexer::Token> {
    if buf.is_empty() {
        tokens.next()
    } else {
        buf.pop()
    }
}

struct Tokens<T: Iterator<Item = lexer::Token>> {
    tokens: T,
    buf: Option<lexer::Token>,
}

impl<T: Iterator<Item = lexer::Token>> Tokens<T> {
    pub fn next(&mut self) -> Option<lexer::Token> {
        if self.buf.is_none() {
            self.tokens.next()
        } else {
            self.buf.take()
        }
    }

    pub fn peek(&mut self) -> Option<&lexer::Token> {
        if self.buf.is_none() {
            let token = self.tokens.next();
            self.buf = token;
        }
        self.buf.as_ref()
    }

    pub fn new(tokens: T) -> Self {
        Self { tokens, buf: None }
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
            _ => panic!()
        },
        None => panic!(),
    }
}

fn parse_expr<T: Iterator<Item = lexer::Token>>(tokens: &mut Tokens<T>) -> Expr {
    enum ExprOrBinop {
        Expr(Expr),
        BinOp(BinOp)
    }
    let mut mix = vec![];
    loop {
        let primary = parse_primary(tokens);
        mix.push(ExprOrBinop::Expr(primary));
        let binop = match tokens.peek() {
            Some(x) => match x {
                lexer::Token::Op(op) => Some(BinOp::Op(op.clone())),
                lexer::Token::Kw(lexer::Kw::And) => Some(BinOp::And),
                lexer::Token::Kw(lexer::Kw::Or) => Some(BinOp::Or),
                _ => None
            },
            None => None,
        };
        if let Some(binop) = binop {
            mix.push(ExprOrBinop::BinOp(binop));
            tokens.next();
        } else {
            break;
        }
    }

    loop {
        if mix.len() == 1 {
            if let ExprOrBinop::Expr(expr) = mix.pop().unwrap() {
                return expr;
            } else {
                panic!("bug");
            }
        }
        let (i, _) = mix.iter().enumerate().filter_map(|(i, x)| match x {
            ExprOrBinop::Expr(_) => None,
            ExprOrBinop::BinOp(bin_op) => Some((i, bin_op)),
        }).map(|(i, x)| {
            let mut j = 0;
            for elem in &PRECEDENCE {
                if elem == x {
                    return (i, j);
                }
                j += 1;
            }
            panic!("bug")
        }).min_by_key(|(_, x)| *x).unwrap();
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
                        (ExprOrBinop::Expr(lhs), ExprOrBinop::BinOp(bin_op), ExprOrBinop::Expr(rhs)) => {
                            new_mix.push(ExprOrBinop::Expr(Expr::BinOp(Box::new(lhs), bin_op, Box::new(rhs))));
                        },
                        _ => panic!("bug")
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr() {
        let tokens = lexer::tokenize("(not is_valid() and opponent != \"harry\" + 5").unwrap();
        let parsed = parse_expr(&mut Tokens::new(tokens.into_iter()));
        println!("{:?}", parsed);
    }
}
