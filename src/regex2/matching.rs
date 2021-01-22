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
    fn chars() {
	use super::super::parsing::compile;
	let (program, nb_groups) = compile("ab", 0).unwrap();
	let (end, idx, results) = find(&program, "abb", nb_groups).unwrap();
	assert_eq!(idx, 0);
	assert_eq!(end, 2);
	assert_eq!(results, vec![]);
    }

    #[test]
    fn escaped() {
	use super::super::parsing::compile;
	let escaped = vec![
	    (
		r"\w",
		vec![
		    ("a", true),
		    ("A", true),
		    ("0", true),
		    ("_", true),
		    ("%", false),
		    ("'", false)
		]
	    )
	];
	for (regex, tests) in escaped {
	    let (program, nb_groups) = compile(regex, 0).unwrap();
	    for (string, result) in tests {
		assert_eq!(find(&program, string, 0).is_some(), result);
	    }
	}
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

/// # Summary
///
/// `Instruction` represents an instruction of the VM.
///
/// # Variants
///
/// `Save(reg: usize)`: save the current location in register `reg`
/// `Split(ip1: usize, ip2: usize)`: fork the current thread, and set the instruction pointer
///                                of both thread respectivly to `ip1` and `ip2`
/// `Char(chr: char)`: match `chr` at the current location, or stop the thread if it doesn't match
/// `Jump(ip: usize)`: set the instruction pointer of the current thread to `ip`
/// `Match(id: usize)`: stop the thread and record it as a successful match of the regex `id`
/// `Any`: match any character at the current location
#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Save(usize),
    Split(usize, usize),
    Char(char),
    Jump(usize),
    Match(usize),
    WordChar,
    Any,
}

/// # Summary
///
/// Represents a `Match`, as returned by a parse of an input by a regex.
///
/// # Definition
/// 
/// `Match` is `pos: usize, id: usize, groups: Vec<Option<usize>>`, where
/// `pos` is the end position (exclusive) of the match (the start position is 0, since all matches are anchored matches).
/// `id` is the id of the regex that led to a match. If many had, one has been selected according to the priority rules.
/// `groups` is a vector of all the groups defined by the regex that led to the match. The groups `i` owns items `2*i`
///   and `2*i+1`, respectivly the start position (inclusive) and the end position (exclusive) of the match.
pub type Match = (usize, usize, Vec<Option<usize>>);

/// # Summary
///
/// Represents the program that is read and executed by the VM. Currently, this is a `Vec` of `Instruction`.
pub type Program = Vec<Instruction>;

/// # Summary
///
/// A way to referencing a `Program` without have an explicit `&Vec<_>` (which clippy doesn't like), but instead a slice referece.
pub type ProgramRef<'a> = &'a [Instruction];

/// # Summary
///
/// `ThreadList` is a `Thread` *priority queue* that doesn't accept twice the same `Thread`.
/// Currently, the priority is the one of a LIFO queue (also called a stack).
/// This may change in the future.
///
/// # Methods
///
/// `new`: create a new `ThreadList`
/// `add`: insert a new `Thread`
/// `get`: pop a thread
/// `from`: create a new `ThreadList` from an existing `Vec<Thread>`
struct ThreadList {
    done: FixedBitSet,
    threads: Vec<Thread>,
}

impl ThreadList {
    /// Create a new `ThreadList` with a given capacity.
    fn new(size: usize) -> Self {
        Self {
            done: FixedBitSet::with_capacity(size),
            threads: Vec::new(),
        }
    }

    /// Insert a new `Thread` in the `ThreadList`. Doesn't do anything if it has already been added once.
    fn add(&mut self, thread: Thread) {
        let pos = thread.instruction();
        if !self.done.contains(pos) {
            self.done.insert(pos);
            self.threads.push(thread);
        }
    }

    /// Pop a `Thread` from the `ThreadList`. This will **not** make the `ThreadList` accept the same `Thread` again.
    fn get(&mut self) -> Option<Thread> {
        self.threads.pop()
    }

    /// Create a new `ThreadList` with given capacity from a `Vec<Thread>`.
    fn from(threads: Vec<Thread>, size: usize) -> Self {
        let mut thread_list = Self::new(size);
        for thread in threads.into_iter() {
            thread_list.add(thread);
        }
        thread_list
    }
}

/// # Summary
///
/// `Thread` corresponds to a given execution of the program up to a certain point.
/// During its lifetime, it only moves forward during the executing of the program,
/// meaning it never backtracks (it might execute code already executed, but never
/// without having gone forward in the text matching). If the thread has to make a choice,
/// a copy of the thread is created, and then each copy makes a different choice.
/// This is why a `Thread` never backtracks (in the worst case scenario, it just stops).
///
/// Fundamentally, a `Thread` is set a registers: the special *instruction pointer* `ip` register,
/// and a bunch of registers dedicated to group matching.
///
/// # Methods
/// 
/// `new`: create a new `Thread`
/// `instruction`: return the `ip` value
/// `jump`: set the `ip` to a new value
/// `save`: set the value for a register dedicated to group matching
#[derive(Clone, Debug)]
struct Thread {
    instruction: usize,
    groups: Vec<Option<usize>>,
}

impl Thread {
    /// Create a new `Thread`. This method is public only to be accessible from documentation,
    /// but this doesn't mean it is meant to be accessed by the end-user.
    pub fn new(instruction: usize, size: usize) -> Self {
        Self {
            instruction,
            groups: vec![None; 2 * size],
        }
    }

    /// Return the value stored in the *instruction pointer* register, `ip`.
    fn instruction(&self) -> usize {
        self.instruction
    }

    /// Set the *instruction pointer* register value to `pos`.
    fn jump(&mut self, pos: usize) {
        self.instruction = pos;
    }

    /// Set the *group* register `idx` value to `pos`.
    fn save(&mut self, idx: usize, pos: usize) {
        self.groups[idx] = Some(pos);
    }
}

fn match_next(chr: char, pos: usize, mut thread: Thread, current: &mut ThreadList, next: Option<&mut ThreadList>, prog: ProgramRef, best_match: &mut Option<Match>) {
    match prog[thread.instruction()] {
        Instruction::Char(expected) => {
            if expected == chr {
                thread.jump(thread.instruction() + 1);
                if let Some(next) = next {
		    next.add(thread);
		}
            }
        }
        Instruction::Any => {
            thread.jump(thread.instruction() + 1);
	    if let Some(next) = next {
		next.add(thread);
	    }
        }
	Instruction::WordChar => {
	    if chr.is_alphanumeric() || chr == '_' {
		thread.jump(thread.instruction() + 1);
		if let Some(next) = next {
		    next.add(thread);
		}
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
                if pos > *p || *prior > id {
                    *best_match = Some((pos, id, thread.groups));
                }
            } else {
                *best_match = Some((pos, id, thread.groups));
            }
        }
    }
}

/// Simulate a VM with program `prog` on `input`. There should be `size` groups.
pub fn find(prog: ProgramRef, input: &str, size: usize) -> Option<Match> {
    let mut current = ThreadList::from(vec![Thread::new(0, size)], prog.len());
    let mut best_match = None;
    for (pos, chr) in input.chars().enumerate() {
        let mut next = ThreadList::new(prog.len());
        while let Some(mut thread) = current.get() {
	    match_next(chr, pos, thread, &mut current, Some(&mut next), &prog[..], &mut best_match);
            
        }
        current = next;
    }
    let pos = input.len();
    while let Some(mut thread) = current.get() {
        match_next('#', pos, thread, &mut current, None, &prog[..], &mut best_match);
    }

    best_match
}
