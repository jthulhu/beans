pub trait Stream<'a> {
    type Output: 'a;
    fn get_at(&'a self, pos: usize) -> Self::Output;
    fn pos(&self) -> usize;
    fn set_pos(&mut self, pos: usize);
    fn pos_pp(&mut self) {
        self.set_pos(self.pos() + 1);
    }
    fn pos_inc(&mut self, off: usize) {
        self.set_pos(self.pos() + off);
    }
    fn get(&'a self) -> Self::Output {
        self.get_at(self.pos())
    }
}

pub enum Char {
    Char(char),
    EOF,
}

pub struct StringStream {
    stream: String,
    pos: usize,
}

impl StringStream {
    pub fn new(stream: String) -> Self {
        Self { stream, pos: 0 }
    }
    pub fn continues(&self, keyword: &str) -> bool {
        self.stream[self.pos..].starts_with(keyword)
    }
    pub fn borrow(&self) -> &str {
        &self.stream[..]
    }
    pub fn len(&self) -> usize {
        self.stream.len()
    }
}

impl Stream<'_> for StringStream {
    type Output = Char;
    fn get_at(&self, pos: usize) -> Self::Output {
	if let Some(chr) = self.stream.chars().nth(pos) {
	    Char::Char(chr)
	} else {
	    Char::EOF
	}   
    }
    fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }
    fn pos(&self) -> usize {
        self.pos
    }
}
