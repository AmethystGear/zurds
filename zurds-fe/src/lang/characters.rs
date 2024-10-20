use std::str::Chars;

#[derive(Clone)]
pub struct Characters<'a> {
    pub view: &'a str,
    pub chars: Chars<'a>,
    pub consumed: usize,
    pub prev: Option<char>,
    pub buf : Option<char>,
}

impl<'a> Characters<'a> {
    pub fn new(chars: &Chars<'a>) -> Self {
        Self {
            view: chars.as_str(),
            chars: chars.clone(),
            consumed: 0,
            prev: None,
            buf: None
        }
    }

    pub fn next(&mut self) -> Option<char> {
        if let Some(c) = self.buf.take() {
            self.consumed += c.len_utf8();
            self.prev = Some(c);
            Some(c)
        } else if let Some(c) = self.chars.next() {
            self.consumed += c.len_utf8();
            self.prev = Some(c);
            Some(c)
        } else {
            None
        }
    }

    pub fn consumed(&self) -> &'a str {
        &self.view[0..self.consumed]
    }

    pub fn match_exact(&self, s: &str) -> Option<Skip> {
        if self.view[self.consumed..].starts_with(s) {
            Some(Skip{consumed: self.consumed, skip: s.len()})
        } else {
            None
        }
    }

    pub fn skip(&mut self, s : Skip) {
        if self.consumed == s.consumed {
            self.consumed += s.skip;
            self.chars = self.view[self.consumed..].chars();
        }
    }

    pub fn undo(&mut self) {
        self.buf = self.prev;
        self.prev = None;
        if let Some(buf) = self.buf {
            self.consumed -= buf.len_utf8();
        }
    }
}

pub struct Skip {
    consumed : usize,
    skip : usize
}