
use escape8259::unescape;
use phf::{phf_map, Map};
use super::{characters::Characters, err::{LangErr, Loc}};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div
}

const OP: Map<&str, Token> = phf_map!(
    "." => Token::Op(Op::Access),
    "==" => Token::Op(Op::Eq),
    "!=" => Token::Op(Op::Neq),
    "<=" => Token::Op(Op::LThanEq),
    ">=" => Token::Op(Op::GThanEq),
    "<" => Token::Op(Op::LThan),
    ">" => Token::Op(Op::GThan),
    "+" => Token::Op(Op::Add),
    "-" => Token::Op(Op::Sub),
    "*" => Token::Op(Op::Mul),
    "/" => Token::Op(Op::Div),
    "%" => Token::Op(Op::Mod),
    ".." => Token::Op(Op::Range),
    "**" => Token::Op(Op::Pow),
    "=" => Token::Assign(None),
    "+=" => Token::Assign(Some(AssignOp::Add)),
    "-=" => Token::Assign(Some(AssignOp::Sub)),
    "*=" => Token::Assign(Some(AssignOp::Mul)),
    "/=" => Token::Assign(Some(AssignOp::Div)),
    ";" => Token::Chain
);

#[derive(Clone, Copy, Debug)]
pub enum Kw {
    If,
    Else,
    Pass,
    Continue,
    Break,
    For,
    Loop,
    And,
    Or,
    In,
    Fn,
    Return
}

const KW: Map<&str, Kw> = phf_map!(
    "if" => Kw::If,
    "else" => Kw::Else,
    "pass" => Kw::Pass,
    "break" => Kw::Break,
    "continue" => Kw::Continue,
    "for" => Kw::For,
    "loop" => Kw::Loop,
    "and" => Kw::And,
    "or" => Kw::Or,
    "in" => Kw::In,
    "fn" => Kw::Fn,
    "return" => Kw::Return
);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Token {
    Indent,
    NewLine,
    Punc(Punc),
    Op(Op),
    Kw(Kw),
    Literal(Literal),
    Ident(Ident),
    Assign(Option<AssignOp>),
    Chain
}

pub type Ident = String;

fn match_exact<T: Clone + std::fmt::Debug>(
    map: Map<&str, T>,
    chars: &mut Characters<'_>,
) -> Option<T> {
    let mut longest = 0;
    let mut longest_match = None;
    for (k, v) in map.entries() {
        if let Some(skip) = chars.match_exact(&k) {
            if k.len() > longest {
                longest_match = Some((v.clone(), skip));
                longest = k.len();
            }
        }
    }
    if let Some((val, skip)) = longest_match {
        chars.skip(skip, None);
        Some(val)
    } else {
        None
    }
}

fn match_int(characters: &mut Characters<'_>) -> Result<Option<i64>, (String, usize)> {
    let mut chars = characters.clone();
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
        characters.skip(chars.get_skip(), c);
        Ok(Some(
            characters
                .consumed()
                .parse()
                .map_err(|x| (format!("{}", x), characters.consumed().chars().count()))?,
        ))
    } else {
        Ok(None)
    }
}

fn match_flt(characters: &mut Characters<'_>) -> Result<Option<f64>, (String, usize)> {
    let mut chars = characters.clone();
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
        characters.skip(chars.get_skip(), c);
        Ok(Some(
            characters
                .consumed()
                .parse()
                .map_err(|x| (format!("{}", x), characters.consumed().chars().count()))?,
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

fn match_str(chars: &mut Characters<'_>) -> Result<Option<String>, (String, usize)> {
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
            let count = s.chars().count();
            return Ok(Some(unescape(s).map_err(|x| (format!("{}", x), count))?));
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
        Err(("unclosed string".into(), s.chars().count()))
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
    while let Some(c) = chars.clone().next() {
        if !(c == '_' || (c == '@' && first) || c.is_ascii_alphanumeric() || c == '?') {
            break;
        }
        first = false;
        chars.next();
    }
    if chars.consumed().len() > 0 {
        Some(chars.consumed().to_string())
    } else {
        None
    }
}

const INDENT_SIZE: usize = 4;
pub fn tokenize(s: &str) -> Result<Vec<(Token, Loc)>, LangErr> {
    let mut col = 0;
    let mut line = 1;
    let mut tokens = vec![];
    let mut chars = s.chars();
    loop {
        let c = if let Some(c) = chars.clone().next() {
            c
        } else {
            break;
        };
        // comment, skip chars until newline
        if c == '#' {
            loop {
                if let Some(c) = chars.next() {
                    if c == '\n' {
                        break;
                    }
                } else {
                    break;
                };
            }
            tokens.push((Token::NewLine, Loc {col, line, len: 1}));
            col = 0;
            line += 1;
            continue;
        }

        // indentation
        if col == 0 && c == ' ' {
            let mut spaces = 0;
            {
                let mut chars = chars.clone();
                while let Some(' ') = chars.next() {
                    spaces += 1;
                }
            }
            // if there is no real indentation, just skip the whitespace and continue.
            if spaces < INDENT_SIZE {
                col += spaces;
                for _ in 0..spaces {
                    chars.next();
                }
                continue;
            }
            let indents = spaces / INDENT_SIZE;
            // each ' ' is 1 byte so indexing indents * INDENT_SIZE onwards is okay to do
            chars = chars.as_str()[indents * INDENT_SIZE..].chars();
            tokens.extend((0..indents).map(|i| (Token::Indent, Loc {col : i * INDENT_SIZE, line, len: INDENT_SIZE})));
            col += indents * INDENT_SIZE;
            continue;
        }

        // whitespace
        if c == ' ' {
            col += 1;
            chars.next();
            continue;
        }
        if c == '\n' {
            tokens.push((Token::NewLine, Loc {col, line, len: 1}));
            chars.next();
            col = 0;
            line += 1;
            continue;
        }

        #[rustfmt::skip]
        const MATCHERS : [fn(Characters<'_>) -> (Result<Option<Token>, (String, usize)>, Characters<'_>); 8] = [
            |mut chars| (match_flt(&mut chars).map(|op| op.map(|flt| Token::Literal(Literal::Float(flt)))), chars),
            |mut chars| (match_int(&mut chars).map(|op| op.map(|int| Token::Literal(Literal::Int(int)))), chars),
            |mut chars| (match_str(&mut chars).map(|op| op.map(|s| Token::Literal(Literal::String(s)))), chars),
            |mut chars| (Ok(match_exact(OP, &mut chars).map(|op| op)), chars),
            |mut chars| (Ok(match_exact(PUNC, &mut chars).map(|punc: Punc| Token::Punc(punc))), chars),
            |mut chars| (Ok(match_kw(&mut chars).map(|kw| Token::Kw(kw))), chars),
            |mut chars| (Ok(match_bool(&mut chars).map(|bool| Token::Literal(Literal::Bool(bool)))), chars),
            |mut chars| (Ok(match_ident(&mut chars).map(|ident| Token::Ident(ident))), chars)
        ];

        let mut characters = None;
        for matcher in MATCHERS {
            let (res, c) = matcher(Characters::new(&chars));
            let res = res.map_err(|(err, len)| LangErr { loc : Loc {col, line, len}, err})?;
            if let Some(token) = res {
                tokens.push((token, Loc {col, line, len: c.consumed().len()}));
                characters = Some(c);
                break;
            }
        }

        if let Some(characters) = characters {
            let token = characters.consumed();
            chars = chars.as_str()[token.len()..].chars();
            col += token.len();
        } else {
            return Err(LangErr {
                loc : Loc {col, line, len: 1},
                err: "could not parse".into(),
            });
        }
    }

    // remove any completely blank lines
    let mut tokens_without_blank_lines = vec![];
    let mut blank = true;
    let mut indents = 0;
    for (token, loc) in tokens {
        match (blank, token) {
            (true, Token::Indent) => indents += 1,
            (_, Token::NewLine) => {
                if !blank {
                    tokens_without_blank_lines.push((Token::NewLine, loc));
                }
                indents = 0;
                blank = true;
            }
            (_, token) => {
                for i in 0..indents {
                    tokens_without_blank_lines.push((Token::Indent, Loc {col : i * 4, line : loc.line, len: 4}));
                }
                tokens_without_blank_lines.push((token, loc));
                blank = false;
                indents = 0;
            }
        }
    }

    // tack on a newline at the end if there isn't one
    match tokens_without_blank_lines.last() {
        Some((Token::NewLine, _)) => {}
        _ => {
            tokens_without_blank_lines.push((Token::NewLine, Loc {col: col + 1, line, len: 1}));
        }
    }
    Ok(tokens_without_blank_lines)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tok(s: &str) -> Vec<Token> {
        tokenize(s)
            .map(|x| x.into_iter().map(|(x, _)| x))
            .unwrap()
            .collect()
    }

    #[test]
    fn test_tokenize_float() {
        for scenario in ["0.5", "000.5", " 0.50000", "   0.5      "] {
            match &tok(scenario)[..] {
                [Token::Literal(Literal::Float(0.5)), Token::NewLine] => {}
                x => panic!("doesn't match {:?}", x),
            }
        }
    }

    #[test]
    fn test_tokenize_int() {
        for scenario in ["5", " 5", "5 ", "   5    "] {
            match &tok(scenario)[..] {
                [Token::Literal(Literal::Int(5)), Token::NewLine] => {}
                x => panic!("doesn't match {:?}", x),
            }
        }
    }

    #[test]
    fn test_math() {
        match &tok("5+2*10")[..] {
            #[rustfmt::skip]
            [
                Token::Literal(Literal::Int(5)), 
                Token::Op(Op::Add), 
                Token::Literal(Literal::Int(2)), 
                Token::Op(Op::Mul), 
                Token::Literal(Literal::Int(10)), 
                Token::NewLine
            ] => {}
            x => panic!("doesn't match {:?}", x),
        }
    }

    #[test]
    fn test_tokenize_range() {
        match &tok("0..5")[..] {
            [Token::Literal(Literal::Int(0)), Token::Op(Op::Range), Token::Literal(Literal::Int(5)), Token::NewLine] =>
                {}
            x => panic!("doesn't match {:?}", x),
        }
    }

    #[test]
    fn test_tokenize_dict() {
        match &tok("{'hello' : 'world'}")[..] {
            #[rustfmt::skip]
            [
                Token::Punc(Punc::BracketLeft),
                Token::Literal(Literal::String(a)), 
                Token::Punc(Punc::Colon), 
                Token::Literal(Literal::String(b)), 
                Token::Punc(Punc::BracketRight), 
                Token::NewLine
            ] => {
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
