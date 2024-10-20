use std::{
    char,
    convert::identity,
    iter::repeat,
    str::{CharIndices, Chars},
};

use escape8259::unescape;
use phf::{phf_map, Map};

use super::characters::Characters;

#[derive(Clone, Copy, Debug)]
pub enum Punc {
    Colon,
    SqrBracketLeft,
    SqrBracketRight,
    BracketLeft,
    BracketRight,
    ParenLeft,
    ParenRight,
    Comma,
}

const PUNC: Map<&str, Punc> = phf_map!(
    ":" => Punc::Colon,
    "[" => Punc::SqrBracketLeft,
    "]" => Punc::SqrBracketRight,
    "{" => Punc::BracketLeft,
    "}" => Punc::BracketRight,
    "(" => Punc::ParenLeft,
    ")" => Punc::ParenRight,
    "," => Punc::Comma,
);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Eq,
    Neq,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    Assign,
    LThanEq,
    GThanEq,
    LThan,
    GThan,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Range,
    Access,
    Pow,
}

const OP: Map<&str, Op> = phf_map!(
    "." => Op::Access,
    "==" => Op::Eq,
    "!=" => Op::Neq,
    "=" => Op::Assign,
    "+=" => Op::AddAssign,
    "-=" => Op::SubAssign,
    "*=" => Op::MulAssign,
    "/=" => Op::DivAssign,
    "<=" => Op::LThanEq,
    ">=" => Op::GThanEq,
    "<" => Op::LThan,
    ">" => Op::GThan,
    "+" => Op::Add,
    "-" => Op::Sub,
    "*" => Op::Mul,
    "/" => Op::Div,
    "%" => Op::Mod,
    ".." => Op::Range,
    "**" => Op::Pow
);

#[derive(Clone, Copy, Debug)]
pub enum Kw {
    If,
    Elif,
    Else,
    Pass,
    Continue,
    Break,
    For,
    While,
    Not,
    And,
    Or,
    In,
}

const KW: Map<&str, Kw> = phf_map!(
    "if" => Kw::If,
    "elif" => Kw::Elif,
    "else" => Kw::Else,
    "pass" => Kw::Pass,
    "break" => Kw::Break,
    "continue" => Kw::Continue,
    "for" => Kw::For,
    "while" => Kw::While,
    "not" => Kw::Not,
    "and" => Kw::And,
    "or" => Kw::Or,
    "in" => Kw::In,
);

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug)]
pub enum Token {
    Indent,
    NewLine,
    Punc(Punc),
    Op(Op),
    Kw(Kw),
    Literal(Literal),
    Ident(Ident),
}

pub type Ident = String;

fn match_exact<T: Copy + std::fmt::Debug>(
    map: Map<&str, T>,
    chars: &mut Characters<'_>,
) -> Option<T> {
    let mut longest = 0;
    let mut longest_match = None;
    for (k, v) in map.entries() {
        if let Some(skip) = chars.match_exact(&k) {
            if k.len() > longest {
                longest_match = Some((*v, skip));
                longest = k.len();
            }
        }
    }
    if let Some((val, skip)) = longest_match {
        chars.skip(skip);
        Some(val)
    } else {
        None
    }
}

fn match_int(chars: &mut Characters<'_>) -> Result<Option<i64>, String> {
    let mut c = chars.next();
    if c == Some('-') {
        c = chars.next();
    }
    let mut has_number = false;
    while c.map_or(false, |c| c.is_ascii_digit()) {
        has_number = true;
        c = chars.next();
    }
    if has_number {
        if c.is_some() {
            chars.undo();
        }
        Ok(Some(
            chars.consumed().parse().map_err(|x| format!("{}", x))?,
        ))
    } else {
        Ok(None)
    }
}

fn match_flt(chars: &mut Characters<'_>) -> Result<Option<f64>, String> {
    let mut numbers = [false, false];
    let mut c = chars.next();
    if c == Some('-') {
        c = chars.next();
    }
    while c.map_or(false, |c| c.is_ascii_digit()) {
        numbers[0] = true;
        c = chars.next();
    }
    if c != Some('.') {
        return Ok(None);
    } else {
        c = chars.next();
    }
    while c.map_or(false, |c| c.is_ascii_digit()) {
        numbers[1] = true;
        c = chars.next();
    }
    if numbers[0] && numbers[1] {
        if c.is_some() {
            chars.undo();
        }
        Ok(Some(
            chars.consumed().parse().map_err(|x| format!("{}", x))?,
        ))
    } else {
        Ok(None)
    }
}

fn match_bool(chars: &mut Characters<'_>) -> Option<bool> {
    let bool = match_exact(phf_map!("true" => true, "false" => false), chars)?;
    if brk(chars) {
        Some(bool)
    } else {
        None
    }
}

fn match_str(chars: &mut Characters<'_>) -> Result<Option<String>, String> {
    let mut escp = false;
    let mut first = true;
    let mut opened = false;
    let mut s = "".to_string();
    while let Some(c) = chars.next() {
        if (c != '"' && c != '\'') && first {
            return Ok(None);
        } else if c == '\\' && !escp {
            escp = true;
        } else if (c == '"' || c == '\'') && !escp && !first {
            return Ok(Some(unescape(s).map_err(|x| format!("{}", x))?));
        } else if !c.is_whitespace() {
            escp = false;
        }
        if !first {
            if c == '\n' {
                s += "\\n";
            } else {
                s += &format!("{}", c);
            }
        }
        first = false;
        opened = true;
    }
    if opened {
        Err("unclosed string".into())
    } else {
        Ok(None)
    }
}

fn match_kw(chars: &mut Characters<'_>) -> Option<Kw> {
    if let Some(kw) = match_exact(KW, chars) {
        if brk(chars) {
            return Some(kw);
        }
    }
    None
}

/// is there a break ahead?
fn brk(chars: &Characters<'_>) -> bool {
    let mut chars = chars.clone();
    match_exact(OP, &mut chars).is_some() || match_exact(PUNC, &mut chars).is_some() || {
        let c = chars.next();
        c == None || c == Some(' ') || c == Some('\n')
    }
}

fn match_ident(chars: &mut Characters<'_>) -> Option<String> {
    let mut first = true;
    while let Some(c) = chars.next() {
        if !(c == '_' || (c == '@' && first) || c.is_ascii_alphanumeric()) {
            break;
        }
        first = false;
    }
    if chars.consumed().len() > 0 {
        Some(chars.consumed().to_string())
    } else {
        None
    }
}

#[derive(Debug)]
pub struct TokenizationError {
    col: usize,
    line: usize,
    err: String,
}

const INDENT_SIZE: usize = 4;
pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenizationError> {
    let mut col = 0;
    let mut line = 1;
    let mut tokens = vec![];
    let mut chars = s.chars();
    loop {
        let c = if let Some((c)) = chars.clone().next() {
            c
        } else {
            break;
        };
        // comment, skip chars until newline
        if c == '#' {
            col = 0;
            line += 1;
            loop {
                if let Some(c) = chars.next() {
                    if c == '\n' {
                        break;
                    }
                } else {
                    break;
                };
            }
            tokens.push(Token::NewLine);
            continue;
        }

        // indentation
        if col == 0 && c == ' ' {
            let mut spaces = 0;
            {
                let mut chars = chars.clone();
                while let Some(' ') = chars.next() {
                    spaces += 1;
                    col += 1;
                }
            }
            let indents = spaces / INDENT_SIZE;
            // each ' ' is 1 byte so indexing indents * 4 onwards is okay to do
            chars = chars.as_str()[indents * 4..].chars();
            tokens.extend((0..indents).map(|_| Token::Indent));
            continue;
        }

        // whitespace
        if c == ' ' {
            col += 1;
            chars.next();
            continue;
        }
        if c == '\n' {
            col = 0;
            line += 1;
            tokens.push(Token::NewLine);
            chars.next();
            continue;
        }

        let mut characters = None;
        #[rustfmt::skip]
        let matchers = [
            |mut chars| (match_flt(&mut chars).map(|op| op.map(|flt| Token::Literal(Literal::Float(flt)))), chars),
            |mut chars| (match_int(&mut chars).map(|op| op.map(|int| Token::Literal(Literal::Int(int)))), chars),
            |mut chars| (match_str(&mut chars).map(|op| op.map(|s| Token::Literal(Literal::String(s)))), chars),
            |mut chars| (Ok(match_exact(OP, &mut chars).map(|op| Token::Op(op))), chars),
            |mut chars| (Ok(match_exact(PUNC, &mut chars).map(|punc: Punc| Token::Punc(punc))), chars),
            |mut chars| (Ok(match_kw(&mut chars).map(|kw| Token::Kw(kw))), chars),
            |mut chars| (Ok(match_bool(&mut chars).map(|bool| Token::Literal(Literal::Bool(bool)))), chars),
            |mut chars| (Ok(match_ident(&mut chars).map(|ident| Token::Ident(ident))), chars)
        ];
        for matcher in matchers {
            let (res, c) = matcher(Characters::new(&chars));
            let res = res.map_err(|err| TokenizationError { col, line, err })?;
            if let Some(token) = res {
                characters = Some(c);
                tokens.push(token);
                break;
            }
        }

        if let Some(characters) = characters {
            let token = characters.consumed();
            chars = chars.as_str()[token.len()..].chars();
            col += token.len();
        } else {
            return Err(TokenizationError {
                col,
                line,
                err: "could not parse".into(),
            });
        }
    }

    // remove any completely blank lines
    let mut tokens_without_blank_lines = vec![];
    let mut blank = true;
    let mut indents = 0;
    for token in tokens {
        match (blank, token) {
            (true, Token::Indent) => indents += 1,
            (_, Token::NewLine) => {
                if !blank {
                    tokens_without_blank_lines.push(Token::NewLine);
                }
                indents = 0;
                blank = true;
            }
            (_, token) => {
                for _ in 0..indents {
                    tokens_without_blank_lines.push(Token::Indent);
                }
                tokens_without_blank_lines.push(token);
                blank = false;
                indents = 0;
            }
        }
    }

    // tack on a newline at the end if there isn't one
    match tokens_without_blank_lines.last() {
        Some(Token::NewLine) => {}
        _ => {
            tokens_without_blank_lines.push(Token::NewLine);
        }
    }
    Ok(tokens_without_blank_lines)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_float() {
        for scenario in ["0.5", "000.5", " 0.50000", "   0.5      "] {
            match tokenize(scenario).as_deref() {
                Ok([Token::Literal(Literal::Float(0.5)), Token::NewLine]) => {}
                x => panic!("doesn't match {:?}", x),
            }
        }
    }

    #[test]
    fn test_tokenize_int() {
        for scenario in ["5", " 5", "5 ", "   5    "] {
            match tokenize(scenario).as_deref() {
                Ok([Token::Literal(Literal::Int(5)), Token::NewLine]) => {}
                x => panic!("doesn't match {:?}", x),
            }
        }
    }

    #[test]
    fn test_math() {
        match tokenize("5+2*10").as_deref() {
            Ok([Token::Literal(Literal::Int(5)), Token::Op(Op::Add), Token::Literal(Literal::Int(2)), Token::Op(Op::Mul), Token::Literal(Literal::Int(10)), Token::NewLine]) => {}
            x => panic!("doesn't match {:?}", x),
        }
    }

    #[test]
    fn test_tokenize_range() {
        match tokenize("0..5").as_deref() {
            Ok(
                [Token::Literal(Literal::Int(0)), Token::Op(Op::Range), Token::Literal(Literal::Int(5)), Token::NewLine],
            ) => {}
            x => panic!("doesn't match {:?}", x),
        }
    }

    #[test]
    fn test_tokenize_dict() {
        match tokenize("{'hello' : 'world'}").as_deref() {
            Ok(
                [Token::Punc(Punc::BracketLeft), Token::Literal(Literal::String(a)), Token::Punc(Punc::Colon), Token::Literal(Literal::String(b)), Token::Punc(Punc::BracketRight), Token::NewLine],
            ) => {
                if a != "hello" || b != "world" {
                    panic!(
                        "expected map to contain 'hello' : 'world', contains '{}' : '{}'",
                        a, b
                    )
                }
            }
            x => panic!("doesn't match {:?}", x),
        }
    }
}
