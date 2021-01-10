use super::parsing::Regex;
use fixedbitset::FixedBitSet;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn groups() {
        use super::super::parsing::compile;
        // Test the regexp (a+)(b+)
        let (program, nb_groups) = compile("(a+)(b+)", 0).unwrap();
        let (end, idx, results) = find(&program, "aabbb", nb_groups).unwrap();
        assert_eq!(idx, 0);
        assert_eq!(end, 5);
        assert_eq!(results, vec![Some(0), Some(2), Some(2), Some(5)]);
    }

    #[test]
    fn greedy() {
        use super::super::parsing::compile;
        let (program, nb_groups) = compile("(a+)(a+)", 0).unwrap();
        let (end, idx, results) = find(&program, "aaaa", nb_groups).unwrap();
        assert_eq!(end, 4);
        assert_eq!(idx, 0);
        assert_eq!(results, vec![Some(0), Some(3), Some(3), Some(4)]);
    }

    #[test]
    fn partial() {
        use super::super::parsing::compile;
        let (program, nb_groups) = compile("a+", 0).unwrap();
        let (end, idx, results) = find(&program, "aaabcd", nb_groups).unwrap();
        assert_eq!(end, 3);
        assert_eq!(idx, 0);
        assert_eq!(results, Vec::new());
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Save(usize),
    Split(usize, usize),
    Char(char),
    Jump(usize),
    Match(usize),
}

pub type Match = (usize, usize, Vec<Option<usize>>);
pub type Program = Vec<Instruction>;

struct ThreadList {
    done: FixedBitSet,
    threads: Vec<Thread>,
}

impl ThreadList {
    fn new(size: usize) -> Self {
        Self {
            done: FixedBitSet::with_capacity(size),
            threads: Vec::new(),
        }
    }

    fn add(&mut self, thread: Thread) {
        let pos = thread.instruction();
        if !self.done.contains(pos) {
            self.done.insert(pos);
            self.threads.push(thread);
        }
    }

    fn get(&mut self) -> Option<Thread> {
        self.threads.pop()
    }

    fn from(threads: Vec<Thread>, size: usize) -> Self {
        let mut thread_list = Self::new(size);
        for thread in threads.into_iter() {
            thread_list.add(thread);
        }
        thread_list
    }
}

#[derive(Clone, Debug)]
struct Thread {
    instruction: usize,
    groups: Vec<Option<usize>>,
}

impl Thread {
    pub fn new(instruction: usize, size: usize) -> Self {
        Self {
            instruction,
            groups: vec![None; 2 * size],
        }
    }
    fn instruction(&self) -> usize {
        self.instruction
    }
    fn jump(&mut self, pos: usize) {
        self.instruction = pos;
    }
    fn save(&mut self, idx: usize, pos: usize) {
        self.groups[idx] = Some(pos);
    }
}

pub fn find(prog: &Program, input: &str, size: usize) -> Option<Match> {
    let mut current = ThreadList::from(vec![Thread::new(0, size)], prog.len());
    let mut best_match = None;
    for (pos, chr) in input.chars().enumerate() {
        let mut next = ThreadList::new(prog.len());
        while let Some(mut thread) = current.get() {
            match prog[thread.instruction()] {
                Instruction::Char(expected) => {
                    if expected == chr {
                        thread.jump(thread.instruction() + 1);
                        next.add(thread);
                    }
                }
                Instruction::Jump(pos) => {
                    thread.jump(pos);
                    current.add(thread);
                }
                Instruction::Save(idx) => {
                    thread.save(idx, pos);
                    thread.jump(thread.instruction() + 1);
                    current.add(thread);
                }
                Instruction::Split(pos1, pos2) => {
                    let mut other = thread.clone();
                    other.jump(pos2);
                    thread.jump(pos1);
                    current.add(other);
                    current.add(thread);
                }
                Instruction::Match(id) => {
                    if let Some((p, prior, _)) = best_match {
                        if pos > p || prior > id {
                            best_match = Some((pos, id, thread.groups));
                        }
                    } else {
                        best_match = Some((pos, id, thread.groups));
                    }
                }
            }
        }
        current = next;
    }
    let pos = input.len();
    while let Some(mut thread) = current.get() {
        match prog[thread.instruction()] {
            Instruction::Char(expected) => (),
            Instruction::Jump(pos) => {
                thread.jump(pos);
                current.add(thread);
            }
            Instruction::Save(idx) => {
                thread.save(idx, pos);
                thread.jump(thread.instruction() + 1);
                current.add(thread);
            }
            Instruction::Split(pos1, pos2) => {
                let mut other = thread.clone();
                other.jump(pos2);
                thread.jump(pos1);
                current.add(other);
                current.add(thread);
            }
            Instruction::Match(id) => {
                if let Some((p, prior, _)) = best_match {
                    if pos > p || prior > id {
                        best_match = Some((pos, id, thread.groups));
                    }
                } else {
                    best_match = Some((pos, id, thread.groups));
                }
            }
        }
    }

    best_match
}
