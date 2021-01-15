use super::matching::{find, Instruction, Program};
use super::parsing::{build, read, Regex, RegexError};

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn builder() {
	use Instruction::*;
	let regex = RegexBuilder::new()
	    .build();
	assert!(regex.names.is_empty());
	assert!(regex.program.is_empty());
	assert!(regex.groups.is_empty());
	assert_eq!(regex.size, 0);

	let regex = RegexBuilder::new()
	    .with_named_regex("a+", String::from("As"))
	    .unwrap()
	    .with_named_regex("b", String::from("B"))
	    .unwrap()
	    .build();
	assert_eq!(regex.names, vec![String::from("As"), String::from("B")]);
	assert_eq!(
	    regex.program,
	    vec![
		Split(1, 4),
		Char('a'),
		Split(1, 3),
		Match(0),
		Char('b'),
		Match(1)
	    ]
	);
	assert_eq!(
	    regex.groups,
	    vec![
		(0, 0),
		(0, 0)
	    ]
	);
	assert_eq!(regex.size, 0);

	let regex = RegexBuilder::new()
	    .with_named_regex("(a+)", String::from("As"))
	    .unwrap()
	    .with_named_regex("(b)", String::from("B"))
	    .unwrap()
	    .build();
	assert_eq!(
	    regex.program,
	    vec![
		Split(1, 6),
		Save(0),
		Char('a'),
		Split(2, 4),
		Save(1),
		Match(0),
		Save(2),
		Char('b'),
		Save(3),
		Match(1)
	    ]
	);
    }

    #[test]
    fn find() {
	let regex = RegexBuilder::new()
	    .with_named_regex("(a+)", String::from("As"))
	    .unwrap()
	    .with_named_regex("(b)", String::from("B"))
	    .unwrap()
	    .build();
	
	let match1 = regex.find("aaacd").unwrap();
	assert_eq!(
	    match1.length,
	    3
	);
	assert_eq!(
	    match1.id,
	    "As"
	);
	assert_eq!(
	    match1.groups.len(),
	    1
	);
	let handle = match1.groups[0].as_ref().unwrap();
	assert_eq!(
	    handle.start,
	    0
	);
	assert_eq!(
	    handle.end,
	    3
	);
	assert_eq!(
	    handle.text,
	    "aaa"
	);
	assert_eq!(
	    match1.text,
	    "aaa"
	);
    }
}

pub struct Handle<'a> {
    pub start: usize,
    pub end: usize,
    pub text: &'a str
}

pub struct Match<'a> {
    pub length: usize,
    pub id: &'a str,
    pub groups: Vec<Option<Handle<'a>>>,
    pub text: &'a str
}

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

    pub fn find<'a>(&'a self, input: &'a str) -> Option<Match<'a>> {
	if let Some((length, id, groups)) = find(&self.program, input, self.size) {
	    let (begin_groups, nb_groups) = self.groups[id];
	    let mut grps = Vec::new();
	    for i in begin_groups..begin_groups+nb_groups {
		if let Some(start) = groups[2*i] {
		    let end = groups[2*i+1].unwrap();
		    let handle = Handle {
			start,
			end,
			text: &input[start..end]
		    };
		    grps.push(Some(handle));
		} else {
		    grps.push(None);
		}
	    }
	    Some(Match {
		length,
		id: &self.names[id][..],
		groups: grps,
		text: &input[..length]
	    })
	} else {
	    None
	}
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
	if self.regexes.len() == 0 {
	    return CompiledRegex::new(Vec::new(), self.names, self.groups, self.current);
	}
        let offset = self.regexes.len()-1;
        let mut program = Vec::new();
        for i in 0..offset {
            program.push(Instruction::Split(0, 0));
        }
        for (i, regex) in self.regexes.into_iter().enumerate() {
	    if i < offset {
		program[i] = Instruction::Split(program.len(), i + 1);
	    } else if offset != 0 {
		if let Instruction::Split(next, _) = program[i-1] {
		    program[i-1] = Instruction::Split(next, program.len());
		}
	    }
            build(regex, &mut program);
	    program.push(Instruction::Match(i));
        }
        CompiledRegex::new(program, self.names, self.groups, self.current)
    }
}
