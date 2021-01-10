use super::matching::{find, Instruction, Program, Match};
use super::parsing::{build, read, Regex, RegexError};

pub struct CompiledRegex {
    names: Vec<String>,
    program: Program,
    groups: Vec<(usize, usize)>,
    size: usize,
}

impl CompiledRegex {
    fn new(program: Program, names: Vec<String>, groups: Vec<(usize, usize)>, size: usize) -> Self {
        Self {
            program,
            names,
            groups,
            size,
        }
    }

    fn find(&self, input: &str) -> Option<Match> {
	find(&self.program, input, self.size)
    }
}

pub struct RegexBuilder {
    names: Vec<String>,
    regexes: Vec<Regex>,
    groups: Vec<(usize, usize)>,
    current: usize,
}

impl RegexBuilder {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            regexes: Vec::new(),
            groups: Vec::new(),
            current: 0,
        }
    }

    pub fn with_named_regex(mut self, regex: &str, name: String) -> Result<Self, RegexError> {
        self.names.push(name);
        let (regex, groups) = read(regex, self.current)?;
        self.groups.push((self.current, groups));
        self.current += groups;
        self.regexes.push(regex);
        Ok(self)
    }

    pub fn build(mut self) -> CompiledRegex {
        let offset = self.regexes.len();
        let mut program = Vec::new();
        for i in 0..offset {
            program.push(Instruction::Split(0, 0));
        }
        for (i, regex) in self.regexes.into_iter().enumerate() {
            program[i] = Instruction::Split(program.len(), i + 1);
            build(regex, &mut program, offset);
        }
        CompiledRegex::new(program, self.names, self.groups, self.current)
    }
}
