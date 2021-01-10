use fixedbitset::FixedBitSet;

// #[cfg(test)]
// mod tests {
//     use super::*;
//     #[test]
//     fn groups_test() {
//         use Instruction::*;
//         // Test the regexp (a+)(b+)
//         let program = vec![
//             Save(2),
//             Char('a'),
//             Split(1, 3),
//             Save(3),
//             Save(4),
//             Char('b'),
//             Split(5, 7),
//             Save(5),
//             Match,
//         ];
//         let mut results = find(&program, String::from("aabbb")).into_iter();
//         let mut expected_1 = [None; 20];
//         expected_1[2] = Some(0);
//         expected_1[3] = Some(2);
//         expected_1[4] = Some(2);
//         expected_1[5] = Some(3);
//         let mut expected_2 = [None; 20];
//         expected_2[2] = Some(0);
//         expected_2[3] = Some(2);
//         expected_2[4] = Some(2);
//         expected_2[5] = Some(4);
//         let mut expected_3 = [None; 20];
//         expected_3[2] = Some(0);
//         expected_3[3] = Some(2);
//         expected_3[4] = Some(2);
//         expected_3[5] = Some(5);

//         assert_eq!(results.next(), Some(expected_1));
//         assert_eq!(results.next(), Some(expected_2));
//         assert_eq!(results.next(), Some(expected_3));
//         assert_eq!(results.next(), None);
//     }
// }

#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Save(usize),
    Split(usize, usize),
    Char(char),
    Jump(usize),
    Match,
}

pub type Program = Vec<Instruction>;
// pub type Groups = [Option<usize>; 20];
// pub type Match = Groups;
// pub type Matches = Vec<Match>;

// struct ThreadList {
//     done: FixedBitSet,
//     threads: Vec<Thread>,
// }

// impl ThreadList {
//     fn new(size: usize) -> Self {
//         Self {
//             done: FixedBitSet::with_capacity(size),
//             threads: Vec::new(),
//         }
//     }

//     fn add(&mut self, thread: Thread) {
//         let pos = thread.instruction();
//         if !self.done.contains(pos) {
//             self.done.insert(pos);
//             self.threads.push(thread);
//         }
//     }

//     fn get(&mut self) -> Option<Thread> {
//         self.threads.pop()
//     }

//     fn from(threads: Vec<Thread>) -> Self {
//         let mut thread_list = Self::new();
//         for thread in threads.into_iter() {
//             thread_list.add(thread);
//         }
//         thread_list
//     }
// }

// #[derive(Clone, Debug)]
// struct Thread {
//     instruction: usize,
//     groups: Groups,
// }

// impl Thread {
//     pub fn new(instruction: usize) -> Self {
//         Self {
//             instruction,
//             groups: [None; 20],
//         }
//     }
//     fn instruction(&self) -> usize {
//         self.instruction
//     }
//     fn jump(&mut self, pos: usize) {
//         self.instruction = pos;
//     }
//     fn save(&mut self, idx: usize, pos: usize) {
//         self.groups[idx] = Some(pos);
//     }
// }

// pub fn find(prog: &Program, input: String) -> Matches {
//     let mut current = ThreadList::from(vec![Thread::new(0)]);
//     let mut matches = Vec::new();
//     for (pos, chr) in input.chars().enumerate() {
//         println!("Reading charcter {}: {}", pos, chr);
//         let mut next = ThreadList::new(prog.len());
//         while let Some(mut thread) = current.get() {
//             println!("Executing thread {}", thread.instruction());
//             match prog[thread.instruction()] {
//                 Instruction::Char(expected) => {
//                     if chr == expected {
//                         thread.jump(thread.instruction() + 1);
//                         next.add(thread);
//                     }
//                 }
//                 Instruction::Jump(pos) => {
//                     thread.jump(pos);
//                     current.add(thread);
//                 }
//                 Instruction::Save(idx) => {
//                     thread.save(idx, pos);
//                     thread.jump(thread.instruction() + 1);
//                     current.add(thread);
//                 }
//                 Instruction::Split(pos1, pos2) => {
//                     let mut other = thread.clone();
//                     other.jump(pos1);
//                     thread.jump(pos2);
//                     current.add(other);
//                     current.add(thread);
//                 }
//                 Instruction::Match => {
//                     matches.push(thread.groups);
//                 }
//             }
//         }
//         current = next;
//     }
//     let pos = input.len();
//     println!("Reading character {}: EOF", pos);
//     while let Some(mut thread) = current.get() {
//         println!("Executing thread {}", thread.instruction());
//         match prog[thread.instruction()] {
//             Instruction::Char(expected) => (),
//             Instruction::Jump(pos) => {
//                 thread.jump(pos);
//                 current.add(thread);
//             }
//             Instruction::Save(idx) => {
//                 thread.save(idx, pos);
//                 thread.jump(thread.instruction() + 1);
//                 current.add(thread);
//             }
//             Instruction::Split(pos1, pos2) => {
//                 let mut other = thread.clone();
//                 other.jump(pos1);
//                 thread.jump(pos2);
//                 current.add(other);
//                 current.add(thread);
//             }
//             Instruction::Match => {
//                 matches.push(thread.groups);
//             }
//         }
//     }

//     matches
// }
