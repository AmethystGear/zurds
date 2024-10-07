use escape8259::unescape;
use phf::{phf_map, Map};

#[derive(Clone, Copy, Debug)]
pub enum Punc {
    Colon,
    SqrBracketLeft,
    SqrBracketRight,
    BracketLeft,
    BracketRight,
    ParenLeft,
    ParenRight,
    Dot,
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
    "." => Punc::Dot,
    "," => Punc::Comma,
);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Eq,
    Neq,
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
}

const OP: Map<&str, Op> = phf_map!(
    "==" => Op::Eq,
    "!=" => Op::Neq,
    "=" => Op::Assign,
    "<=" => Op::LThanEq,
    ">=" => Op::GThanEq,
    "<" => Op::LThan,
    ">" => Op::GThan,
    "+" => Op::Add,
    "-" => Op::Sub,
    "*" => Op::Mul,
    "/" => Op::Div,
    "%" => Op::Mod,
    ".." => Op::Range
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
    In
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
    Punc(Punc),
    Op(Op),
    Kw(Kw),
    Literal(Literal),
    Ident(Ident),
}

pub type Ident = String;

fn match_exact<T: Copy>(map: Map<&str, T>, s: &str) -> Option<(usize, T)> {
    let mut longest_str = "";
    let mut longest_match = None;
    for (k, v) in map.entries() {
        if s.starts_with(k) && s.len() > longest_str.len() {
            longest_match = Some(*v);
            longest_str = k;
        }
    }
    if let Some(longest_match) = longest_match {
        Some((longest_str.len(), longest_match))
    } else {
        None
    }
}

fn match_int(chars: &[char], s: &str) -> Result<Option<(usize, i64)>, String> {
    let mut i = 0;
    if chars[0] == '-' {
        i += 1;
    }
    let mut has_number = false;
    while i < chars.len() && chars[i].is_ascii_digit() {
        has_number = true;
        i += 1;
    }
    if !has_number {
        return Ok(None);
    }
    Ok(Some((i, s[0..i].parse().map_err(|x| format!("{}", x))?)))
}

fn match_flt(chars: &[char], s: &str) -> Result<Option<(usize, f64)>, String> {
    let mut i = 0;
    if chars[0] == '-' {
        i += 1;
    }
    let mut has_number = false;
    while i < chars.len() && chars[i].is_ascii_digit() {
        has_number = true;
        i += 1;
    }
    if i < chars.len() &&  chars[i] == '.' {
        i += 1;
    } else {
        return Ok(None);
    }
    while i < chars.len() && chars[i].is_ascii_digit() {
        has_number = true;
        i += 1;
    }
    if !has_number {
        return Ok(None);
    }
    Ok(Some((i, s[0..i].parse().map_err(|x| format!("{}", x))?)))
}

fn match_bool(pos: usize, s: &str, chars: &[char]) -> Option<(usize, bool)> {
    if (s[pos..].starts_with("true") || s[pos..].starts_with("True")) && brk(pos + 4, s, chars) {
        Some((4, true))
    } else if s[pos..].starts_with("false")
        || s[pos..].starts_with("False") && brk(pos + 5, s, chars)
    {
        Some((5, false))
    } else {
        None
    }
}

fn match_str(chars: &[char]) -> Result<Option<(usize, String)>, String> {
    let mut escp = false;
    let mut first = true;
    let mut s = "".to_string();
    for (i, c) in chars.iter().enumerate() {
        if c != &'"' && first {
            return Ok(None);
        } else if c == &'\\' && !escp {
            escp = true;
        } else if c == &'"' && !escp && !first {
            return Ok(Some((i + 1, unescape(s).map_err(|x| format!("{}", x))?)));
        } else if !c.is_whitespace() {
            escp = false;
        }
        if !first {
            if c == &'\n' {
                s += "\\n";
            } else {
                s += &format!("{}", c);
            }
        }
        first = false;
    }
    return Err("unclosed string".into());
}

fn match_kw(pos: usize, s: &str, chars: &[char]) -> Option<(usize, Kw)> {
    if let Some((i, kw)) = match_exact(KW, &s[pos..]) {
        if brk(pos + i, s, chars) {
            return Some((i, kw));
        }
    }
    None
}

fn brk(i: usize, s: &str, chars: &[char]) -> bool {
    chars[i] == ' '
        || chars[i] == '\n'
        || match_exact(OP, &s[i..]).is_some()
        || match_exact(PUNC, &s[i..]).is_some()
}

#[derive(Debug)]
pub struct TokenizationError {
    col: usize,
    line: usize,
    err: String,
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenizationError> {
    let mut col = 0;
    let mut line = 1;
    let mut pos = 0;

    if !s.is_ascii() {
        return Err(TokenizationError {
            col,
            line,
            err: "program should only contain ascii characters".into(),
        });
    }

    let chars: Vec<_> = s.chars().collect();
    let mut tokens = vec![];
    while pos < chars.len() {
        // comment, skip chars until newline
        if chars[pos] == '#' {
            while pos < chars.len() && chars[pos] != '\n' {
                pos += 1;
            }
            col = 0;
            line += 1;
            continue;
        }

        // indentation
        if col == 0 && chars[pos] == ' ' {
            let mut i = 0;
            while chars[pos] == ' ' {
                col += 1;
                pos += 1;
                i += 1;
            }
            let indents = i / 4;
            for _ in 0..indents {
                tokens.push(Token::Indent);
            }
            continue;
        }

        // whitespace
        if chars[pos] == ' ' {
            col += 1;
            pos += 1;
            continue;
        }
        if chars[pos] == '\n' {
            col = 0;
            line += 1;
            pos += 1;
            continue;
        }

        // tokens
        let mut match_len = None;
        if let Some((i, f)) =
            match_flt(&chars[pos..], &s[pos..]).map_err(|err| TokenizationError { col, line, err })?
        {
            tokens.push(Token::Literal(Literal::Float(f)));
            match_len = Some(i);
        } else if let Some((i, int)) =
            match_int(&chars[pos..], &s[pos..]).map_err(|err| TokenizationError { col, line, err })?
        {
            tokens.push(Token::Literal(Literal::Int(int)));
            match_len = Some(i);
        } else if let Some((i, s)) =
            match_str(&chars[pos..]).map_err(|err| TokenizationError { col, line, err })?
        {
            tokens.push(Token::Literal(Literal::String(s)));
            match_len = Some(i);
        } else if let Some((s, op)) = match_exact(OP, &s[pos..]) {
            tokens.push(Token::Op(op));
            match_len = Some(s);
        } else if let Some((s, punc)) = match_exact(PUNC, &s[pos..]) {
            tokens.push(Token::Punc(punc));
            match_len = Some(s);
        } else if let Some((i, kw)) = match_kw(pos, s, &chars) {
            tokens.push(Token::Kw(kw));
            match_len = Some(i);
        } else if let Some((i, b)) = match_bool(pos, s, &chars) {
            tokens.push(Token::Literal(Literal::Bool(b)));
            match_len = Some(i);
        } else {
            let mut i = pos;
            while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            if i != pos {
                tokens.push(Token::Ident(s[pos..i].into()));
                match_len = Some(i - pos);
            }
        }

        if let Some(match_len) = match_len {
            col += match_len;
            pos += match_len;
        } else {
            return Err(TokenizationError {
                col,
                line,
                err: "could not parse".into(),
            });
        }
    }
    Ok(tokens)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_float() {
        for scenario in ["0.5", ".5", "000.5", " 0.50000", "   0.5      "] {
            match tokenize(scenario).as_deref() {
                Ok([Token::Literal(Literal::Float(0.5))]) => {}
                _ => panic!("doesn't match")
            }
        }
    }

    #[test]
    fn test_tokenize_int() {
        match tokenize("5").as_deref() {
            Ok([Token::Literal(Literal::Int(5))]) => {}
            _ => panic!("doesn't match")
        }
    }
}