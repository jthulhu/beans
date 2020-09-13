use std::str::Chars;

pub trait Stream {
    type Single;
    type Slice;
    fn get_at(&self, pos: usize) -> Option<Self::Single>;
    fn pos(&self) -> usize;
    fn set_pos(&mut self, pos: usize);
    fn slice(&self, slice: std::ops::Range<usize>) -> Self::Slice;
    fn pos_pp(&mut self) {
        self.set_pos(self.pos() + 1);
    }
    fn get(&self) -> Option<Self::Single> {
        self.get_at(self.pos())
    }
}

pub struct StringStream {
    stream: String,
    pos: usize
}

impl StringStream {
    pub fn new(stream: String) -> Self {
	Self {
	    stream,
	    pos: 0
	}
    }
}

impl<'a> Stream for StringStream {
    type Single = char;
    type Slice = &'a str;
    fn slice(&self, slice: std::ops::Range<usize>) -> Self::Slice {
	self.stream[slice]
    }
    fn get_at(&self, pos: usize) -> Option<Self::Single> {
	self.stream.chars().nth(pos)
    }
    fn set_pos(&mut self, pos: usize) {
	self.pos = pos;
    }
    fn pos(&self) -> usize {
	self.pos
    }
}
