#[derive(Debug)]
pub struct LangErr {
    pub loc: Loc,
    pub err: String,
}

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub col: usize,
    pub line: usize,
    pub len: usize
}