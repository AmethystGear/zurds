use std::str::Chars;

#[derive(Clone)]
pub struct Characters<'a> {
    pub view: &'a str,
    pub chars: Chars<'a>,
    pub consumed: usize,
}

impl<'a> Characters<'a> {
    pub fn new(chars: &Chars<'a>) -> Self {
        Self {
            view: chars.as_str(),
            chars: chars.clone(),
            consumed: 0,
        }
    }

    pub fn next(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.consumed += c.len_utf8();
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
        } else {
            panic!("invalid skip");
        }
    }

    pub fn get_skip(&self) -> Skip {
        Skip { consumed: 0, skip : self.consumed }
    }
}

pub struct Skip {
    pub consumed : usize,
    pub skip : usize
}