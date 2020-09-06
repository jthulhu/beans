pub trait Stream {
    type Output;
    fn get_at(&self, pos: u32) -> Self::Output;
    fn pos(&self) -> u32;
    fn set_pos(&mut self, pos: u32);
    fn pos_pp(&mut self) {
	self.set_pos(self.pos()+1);
    }
    fn get(&self) -> Self::Output {
	self.get_at(self.pos())
    }
}
