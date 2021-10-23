use crate::lexer::TerminalId;
use crate::newtype;
use fixedbitset::FixedBitSet;
use unbounded_interval_tree::IntervalTree;

#[cfg(test)]
mod tests {
    use super::super::parsing::tests::compile;
    use super::*;
    #[test]
    fn groups() {
        // Test the regexp (a+)(b+)
        let (program, nb_groups) = compile("(a+)(b+)", TerminalId(0)).unwrap();
        let Match {
            pos: end,
            id: idx,
            groups: results,
        } = find(&program, "aabbb", nb_groups, &Allowed::All).unwrap();
        assert_eq!(idx, TerminalId(0));
        assert_eq!(end, 5);
        assert_eq!(results, vec![Some(0), Some(2), Some(2), Some(5)]);
    }

    #[test]
    fn chars() {
        let (program, nb_groups) = compile("ab", TerminalId(0)).unwrap();
        let Match {
            pos: end,
            id: idx,
            groups: results,
        } = find(&program, "abb", nb_groups, &Allowed::All).unwrap();
        assert_eq!(idx, TerminalId(0));
        assert_eq!(end, 2);
        assert_eq!(results, vec![]);
    }

    #[test]
    fn escaped() {
        let escaped = vec![
            (
                r"\w",
                vec![
                    ("a", true),
                    ("A", true),
                    ("0", true),
                    ("_", true),
                    ("%", false),
                    ("'", false),
                ],
            ),
            (r"a\b", vec![("a", true), ("ab", false)]),
            (
                r".\b.",
                vec![("a ", true), (" a", true), ("  ", false), ("aa", false)],
            ),
        ];
        for (regex, tests) in escaped {
            let (program, _) = compile(regex, TerminalId(0)).unwrap();
            for (string, result) in tests {
                assert_eq!(find(&program, string, 0, &Allowed::All).is_some(), result);
            }
        }
    }

    #[test]
    fn greedy() {
        let (program, nb_groups) = compile("(a+)(a+)", TerminalId(0)).unwrap();
        let Match { pos: end, id: idx, groups: results } = find(&program, "aaaa", nb_groups, &Allowed::All).unwrap();
        assert_eq!(end, 4);
        assert_eq!(idx, TerminalId(0));
        assert_eq!(results, vec![Some(0), Some(3), Some(3), Some(4)]);
    }

    #[test]
    fn partial() {
        let (program, nb_groups) = compile("a+", TerminalId(0)).unwrap();
        let Match { pos: end, id: idx, groups: results } = find(&program, "aaabcd", nb_groups, &Allowed::All).unwrap();
        assert_eq!(end, 3);
        assert_eq!(idx, TerminalId(0));
        assert_eq!(results, Vec::new());
    }
}

newtype! {
    pub id InstructionPointer
    impl {
	pub fn incr(&self) -> Self {
	    Self(self.0+1)
	}
    }
}

/// # Summary
///
/// `Instruction` represents an instruction of the VM.
///
/// # Variants
///
/// `Switch(ips: Vec<(usize, usize)>)`: fork the current thread once for each `(id, ip)` in `ips`,
///                                         and set the instruction pointer of each new thread to `ip`, but only
///                                         if the regex `id` is allowed, or if `ignored`.
/// `Save(reg: usize)`: save the current location in register `reg`
/// `Split(ip1: usize, ip2: usize)`: fork the current thread, and set the instruction pointer
///                                of both thread respectivly to `ip1` and `ip2`
/// `Char(chr: char)`: match `chr` at the current location, or stop the thread if it doesn't match
/// `Jump(ip: usize)`: set the instruction pointer of the current thread to `ip`
/// `Match(id: usize)`: stop the thread and record it as a successful match of the regex `id`
/// `WordChar`: match /[A-Za-z0-9_]/ at the current location, or stop the thread if it doesn't match
/// `WordBoundary`: match a word boundary (meaning, the end of the beginning of a word)
/// `CharacterClass(
///      class: IntervalTree<char>,
///      negated: bool
///  )`: match any character inside `class`, or outside if `negated`.
///     **Warning**: this instruction is not constant time complexity,
///              since it is actually a shorthand for many instructions at once.
///              It is however (much) more efficient than if those instructions
///              were executed indipendently.
/// `Any`: match any character at the current location
#[derive(PartialEq, Debug)]
pub enum Instruction {
    Switch(Vec<(TerminalId, InstructionPointer)>),
    Save(usize),
    Split(InstructionPointer, InstructionPointer),
    Char(char),
    Jump(InstructionPointer),
    Match(TerminalId),
    WordChar,
    Digit,
    WordBoundary,
    Whitespace,
    CharacterClass(IntervalTree<char>, bool),
    EOF,
    Any,
}

/// # Summary
/// Set the allowed rules for the regex engine. It can either be `Allowed::All`, to allow
/// all rules, or `Allowed::Some(rules: FixedBitSet)`, in which case only `rules` are allowed.
#[derive(Debug)]
pub enum Allowed {
    All,
    Some(AllowedTerminals),
}

impl Allowed {
    pub fn contains(&self, i: TerminalId) -> bool {
        match self {
            Allowed::All => true,
            Allowed::Some(allowed) => allowed.contains(i),
        }
    }
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
pub struct Match {
    pub pos: usize,
    pub id: TerminalId,
    pub groups: Vec<Option<usize>>,
}

// /// # Summary
// ///
// /// A way to referencing a `Program` without have an explicit `&Vec<_>` (which clippy doesn't like), but instead a slice referece.
// pub type ProgramRef<'a> = &'a [Instruction];

newtype! {
    #[derive(PartialEq)]
    pub vec Program (Instruction) [InstructionPointer]
    impl {
	pub fn len_ip(&self) -> InstructionPointer {
	    InstructionPointer(self.len())
	}
    }
}

newtype! {
    pub slice ProgramSlice (Instruction) [InstructionPointer]
    of Program
}

newtype! {
    set DoneThreads [InstructionPointer]
}

newtype! {
    pub set AllowedTerminals [TerminalId]
}

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
    done: DoneThreads,
    threads: Vec<Thread>,
}

impl ThreadList {
    /// Create a new `ThreadList` with a given capacity.
    fn new(size: usize) -> Self {
        Self {
            done: DoneThreads::with_capacity(size),
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
    instruction: InstructionPointer,
    groups: Vec<Option<usize>>,
}

impl Thread {
    /// Create a new `Thread`. This method is public only to be accessible from documentation,
    /// but this doesn't mean it is meant to be accessed by the end-user.
    pub fn new(instruction: InstructionPointer, size: usize) -> Self {
        Self {
            instruction,
            groups: vec![None; 2 * size],
        }
    }

    /// Return the value stored in the *instruction pointer* register, `ip`.
    fn instruction(&self) -> InstructionPointer {
        self.instruction
    }

    /// Set the *instruction pointer* register value to `pos`.
    fn jump(&mut self, pos: InstructionPointer) {
        self.instruction = pos;
    }

    /// Set the *group* register `idx` value to `pos`.
    fn save(&mut self, idx: usize, pos: usize) {
        self.groups[idx] = Some(pos);
    }
}

/// Execute a single instruction for `thread`, in a given context.
#[allow(clippy::too_many_arguments)]
fn match_next(
    chr: char,
    pos: usize,
    mut thread: Thread,
    current: &mut ThreadList,
    next: Option<&mut ThreadList>,
    prog: &ProgramSlice,
    best_match: &mut Option<Match>,
    last: Option<char>,
    allowed: &Allowed,
) {
    /// Return whether `chr` is a word char,
    /// matched by /[a-zA-Z0-9_]/.
    fn is_word_char(chr: char) -> bool {
        chr.is_alphanumeric() || chr == '_'
    }

    /// Return whether `chr` is a digit,
    /// matched by /[0-9]/.
    fn is_digit(chr: char) -> bool {
        chr.is_digit(10)
    }

    /// Return whether `chr` is a whitespace,
    /// matched by /[ \t]/.
    fn is_whitespace(chr: char) -> bool {
        chr == ' ' || chr == '\t'
    }

    /// Advance `thread` to the next instruction and queue it
    /// in the `thread_list`.
    fn advance(mut thread: Thread, thread_list: Option<&mut ThreadList>) {
        thread.jump(thread.instruction().incr());
        if let Some(next) = thread_list {
            next.add(thread);
        }
    }

    match &prog[thread.instruction()] {
        Instruction::Char(expected) => {
            if *expected == chr {
                advance(thread, next);
            }
        }
        Instruction::Any => advance(thread, next),
        Instruction::WordChar => {
            if is_word_char(chr) {
                advance(thread, next);
            }
        }
        Instruction::Digit => {
            if is_digit(chr) {
                advance(thread, next);
            }
        }
        Instruction::Whitespace => {
            if is_whitespace(chr) {
                advance(thread, next);
            }
        }
        Instruction::Jump(pos) => {
            thread.jump(*pos);
            current.add(thread);
        }
        Instruction::Save(idx) => {
            thread.save(*idx, pos);
            advance(thread, Some(current));
        }
        Instruction::Switch(instructions) => {
            instructions
                .iter()
                .rev()
                .filter(|(id, _)| allowed.contains(*id))
                .for_each(|(_, ip)| {
                    let mut new = thread.clone();
                    new.jump(*ip);
                    current.add(new);
                });
        }
        Instruction::Split(pos1, pos2) => {
            let mut other = thread.clone();
            other.jump(*pos2);
            thread.jump(*pos1);
            current.add(other);
            current.add(thread);
        }
        Instruction::Match(id) => {
            if let Some(Match {
                pos: p, id: prior, ..
            }) = best_match
            {
                if pos > *p || *prior > *id {
                    *best_match = Some(Match {
                        pos,
                        id: *id,
                        groups: thread.groups,
                    });
                }
            } else {
                *best_match = Some(Match { pos, id: *id, groups: thread.groups });
            }
        }
        Instruction::CharacterClass(class, negated) => {
            if negated ^ class.contains_point(chr) {
                advance(thread, next);
            }
        }
        Instruction::WordBoundary => {
            if let Some(last) = last {
                if is_word_char(last) ^ is_word_char(chr) {
                    advance(thread, Some(current));
                }
            } else {
                advance(thread, Some(current));
            }
        }
        Instruction::EOF => {
            if next.is_none() {
                advance(thread, Some(current));
            }
        }
    }
}

/// Simulate a VM with program `prog` on `input`. There should be `size` groups.
pub fn find(prog: &ProgramSlice, input: &str, size: usize, allowed: &Allowed) -> Option<Match> {
    let mut current = ThreadList::from(vec![Thread::new(InstructionPointer(0), size)], prog.len());
    let mut best_match = None;
    let mut last = None;
    for (pos, chr) in input.chars().enumerate() {
        let mut next = ThreadList::new(prog.len());
        while let Some(thread) = current.get() {
            match_next(
                chr,
                pos,
                thread,
                &mut current,
                Some(&mut next),
                prog,
                &mut best_match,
                last,
                allowed,
            );
        }
        current = next;
        last = Some(chr);
    }
    let pos = input.len();
    while let Some(thread) = current.get() {
        match_next(
            '#',
            pos,
            thread,
            &mut current,
            None,
            prog,
            &mut best_match,
            last,
            allowed,
        );
    }

    best_match
}
