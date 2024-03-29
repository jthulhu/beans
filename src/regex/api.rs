use super::matching::InstructionPointer;
use super::matching::{self, AllowedTerminals, Instruction, Program};
use super::parsing::{build, read, Regex, RegexError};
use crate::lexer::TerminalId;
use newty::newty;
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn builder() {
        use Instruction::*;
        let regex = RegexBuilder::new().build();
        assert!(regex.names.is_empty());
        assert!(regex.program.is_empty());
        assert!(regex.groups.is_empty());
        assert_eq!(regex.size, 0);

        let regex = RegexBuilder::new()
            .with_named_regex("a+", String::from("As"), false)
            .unwrap()
            .with_named_regex("b", String::from("B"), false)
            .unwrap()
            .build();
        assert_eq!(
            regex.names,
            GroupNames::from(vec![String::from("As"), String::from("B")])
        );
        assert_eq!(
            regex.program,
            Program::from(vec![
                Switch(vec![
                    (TerminalId(0), InstructionPointer(1)),
                    (TerminalId(1), InstructionPointer(4))
                ]),
                Char('a'),
                Split(InstructionPointer(1), InstructionPointer(3)),
                Match(TerminalId(0)),
                Char('b'),
                Match(TerminalId(1))
            ])
        );
        assert_eq!(regex.groups, Groups::from(vec![(0, 0), (0, 0)]));
        assert_eq!(regex.size, 0);

        let regex = RegexBuilder::new()
            .with_named_regex("(a+)", String::from("As"), false)
            .unwrap()
            .with_named_regex("(b)", String::from("B"), false)
            .unwrap()
            .build();
        assert_eq!(
            regex.program,
            Program::from(vec![
                Switch(vec![
                    (TerminalId(0), InstructionPointer(1)),
                    (TerminalId(1), InstructionPointer(6))
                ]),
                Save(0),
                Char('a'),
                Split(InstructionPointer(2), InstructionPointer(4)),
                Save(1),
                Match(TerminalId(0)),
                Save(2),
                Char('b'),
                Save(3),
                Match(TerminalId(1))
            ])
        );

        // let regex = RegexBuilder::new()
        //     .with_named_regex(r"'(([^'\\]|(\\[^\\])|(\\\\))*)'", String::from("STRING"))
        //     .unwrap()
        //     .build();
        // assert_eq!(
        //     regex.program,
        //     Vec::new()
        // );
    }

    #[test]
    fn find() {
        let regex = RegexBuilder::new()
            .with_named_regex("(a+)", String::from("As"), false)
            .unwrap()
            .with_named_regex("(b)(c)", String::from("BC"), false)
            .unwrap()
            .build();
        let text = "aaacd";
        let match1 = regex.find(text, &Allowed::All).unwrap();
        assert_eq!(match1.chars_length, 3);
        assert_eq!(match1.name, "As");
        assert_eq!(match1.groups.len(), 1);
        let handle = match1.groups[0].as_ref().unwrap();
        assert_eq!(handle.bytes_start, 0);
        assert_eq!(handle.bytes_end, 3);
        assert_eq!(handle.text(text), "aaa");

        let text = "bc";
        let match2 = regex.find(text, &Allowed::All).unwrap();
        assert_eq!(match2.chars_length, 2);
        assert_eq!(match2.name, "BC");
        assert_eq!(match2.groups.len(), 2);
        let handle = match2.groups[0].as_ref().unwrap();
        assert_eq!(handle.bytes_start, 0);
        assert_eq!(handle.bytes_end, 1);
        assert_eq!(handle.text(text), "b");
        let handle = match2.groups[1].as_ref().unwrap();
        assert_eq!(handle.bytes_start, 1);
        assert_eq!(handle.bytes_end, 2);
        assert_eq!(handle.text(text), "c");

        let text = "cde";
        let match3 = regex.find(text, &Allowed::All);
        assert!(match3.is_none());
    }

    #[test]
    fn groups() {
        let regex = RegexBuilder::new()
            .with_named_regex("'(.*)'", String::from("STRING"), false)
            .unwrap()
            .with_named_regex("\"(.*)\"", String::from("STRING"), false)
            .unwrap()
            .build();
        let text = "'blabla'";
        let match1 = regex.find(text, &Allowed::All).unwrap();
        assert_eq!(match1.chars_length, 8);
        assert_eq!(match1.name, "STRING");
        assert_eq!(match1.groups.len(), 1);
        let handle = match1.groups[0].as_ref().unwrap();
        assert_eq!(handle.bytes_start, 1);
        assert_eq!(handle.bytes_end, 7);
        assert_eq!(handle.text(text), "blabla");
    }

    #[test]
    fn any() {
        let regex = RegexBuilder::new()
            .with_named_regex(".*", String::from("Default"), false)
            .unwrap()
            .build();
        assert_eq!(
            regex.find("0123456", &Allowed::All).unwrap().chars_length,
            7
        );
        assert_eq!(regex.find("012", &Allowed::All).unwrap().chars_length, 3);
        assert_eq!(regex.find("", &Allowed::All).unwrap().chars_length, 0);
    }
}

newty! {
    #[derive(PartialEq, Eq, Serialize, Deserialize)]
    pub vec Groups((usize, usize))[TerminalId]
}

newty! {
    #[derive(PartialEq, Eq, Serialize, Deserialize)]
    vec GroupNames(String)[TerminalId]
}

/// The allowed named regex in a single match.
/// This is useful is you want to prevent the engine from matching certain regex by not allowing them.
/// It is very efficient in the sense that the complexity of a match depends only on the number of allowed regex,
/// not on the number of compiled regex, which means it is a good idea to compile all regex at once, into a single
/// engine, and then filter the one used for a certain match.
#[derive(Debug)]
pub enum Allowed {
    /// Allow all regex.
    All,
    /// Allow only regex whose id is the one in the vector.
    Some(Vec<TerminalId>),
}

impl Allowed {
    fn convert(&self, size: usize) -> matching::Allowed {
        match self {
            Allowed::All => matching::Allowed::All,
            Allowed::Some(rules) => {
                let mut allowed = AllowedTerminals::with_raw_capacity(size);
                for i in rules {
                    allowed.insert(*i);
                }
                matching::Allowed::Some(allowed)
            }
        }
    }
}

/// # Summary
///
/// `Handle` represents a region of the input captured by a group.
///
/// # Methods
///
/// `start`: return the start position of the region (inclusive)
/// `end`: return the end position of the region (exclusive)
/// `text`: return the captured region as a slice of the input
/// `length`: return the length of the captured region
#[derive(Debug)]
pub struct Handle {
    bytes_start: usize,
    bytes_end: usize,
}

impl Handle {
    /// Return the start position of the region (inclusive).
    pub fn bytes_start(&self) -> usize {
        self.bytes_start
    }

    /// Return the end position of the region (exclusive).
    pub fn bytes_end(&self) -> usize {
        self.bytes_end
    }

    /// Return the captured region as a slice of the input.
    pub fn text<'a>(&self, origin_text: &'a str) -> &'a str {
        &origin_text[self.bytes_start..self.bytes_end]
    }
}

/// # Summary
///
/// `Match` represents the successful report of the parsing of an input text by a regex engine.
/// It only stands for matches that are anchored at the very beggining the the input.
/// It contains informations about: the length of the match, which regex led to that match,
/// the position of every groups that have been caught, and the slice of the text that has been
/// matched.
///
/// # Methods
///
/// `length`: return the length of the match
/// `name`: return the name of the regex which led to the match
/// `id`: return the identifier of the regex which led to the match
/// `groups`: return the groups of the regex
/// `text`: return the substring of the input that corresponds to the match
#[derive(Debug)]
pub struct Match<'pattern> {
    chars_length: usize,
    name: &'pattern str,
    id: TerminalId,
    groups: Vec<Option<Handle>>,
}

impl Match<'_> {
    /// Return the length of the match in characters.
    pub fn chars_length(&self) -> usize {
        self.chars_length
    }

    /// Return the identifier of the regex which led to the match.
    pub fn id(&self) -> TerminalId {
        self.id
    }

    /// Return the name of the regex which led to the match.
    pub fn name(&self) -> &str {
        self.name
    }

    /// Return the groups of the regex. The position of each group is
    /// enforced by its position in the regex. Each group may or may not
    /// have been caught.
    pub fn groups(&self) -> &[Option<Handle>] {
        &self.groups
    }
}

/// # Summary
///
/// `CompiledRegex` represents a regex that has already been processed. Since it is not supposed to be built
/// directly, there is no way to do so. Instead, use the `RegexBuilder`.
/// Keep in mind that the compilation time might be more expensive than matching time, especially if
/// you match short inputs against a lot of regex, so you should always reuse a compiled regex when possible.
/// To match a regex against a given input, do `regex.find(input)`.
///
/// # Method
///
/// `find`: match against a given input
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Serialize, Deserialize)]
pub struct CompiledRegex {
    names: GroupNames,
    program: Program,
    groups: Groups,
    size: usize,
}

impl CompiledRegex {
    fn new(program: Program, names: GroupNames, groups: Groups, size: usize) -> Self {
        Self {
            names,
            program,
            groups,
            size,
        }
    }

    /// Match against a given input. Will return only one match, if many were possibles,
    /// according to the priority rules.
    pub fn find<'pattern>(
        &'pattern self,
        input: &str,
        allowed: &Allowed,
    ) -> Option<Match<'pattern>> {
        if let Some(matching::Match {
            char_pos: length,
            id,
            groups,
            ..
        }) = matching::find(
            &self.program,
            input,
            self.size,
            &allowed.convert(self.names.len()),
        ) {
            let (begin_groups, end_groups) = self.groups[id];
            let mut grps = Vec::new();
            for i in begin_groups..end_groups {
                if let Some(start) = groups[2 * i] {
                    let end = groups[2 * i + 1].unwrap();
                    let handle = Handle {
                        bytes_start: start,
                        bytes_end: end,
                    };
                    grps.push(Some(handle));
                } else {
                    grps.push(None);
                }
            }
            Some(Match {
                chars_length: length,
                id,
                name: &self.names[id],
                groups: grps,
            })
        } else {
            None
        }
    }
}

/// # Summary
///
/// `RegexBuilder` is a helper to build a regular expression engine.
/// A single regex engine may process many regex at once, so it is encouraged
/// to (as it is very cheap) load every regex that should be matched against the
/// same input within a single regex engine.
/// `RegexBuilder` only provides a multi-regex feature, that is to say, it is meant
/// to contain a lot of regular expressions. For that reason, every one has to be named.
/// When matching, the engine will tell in its report which of the many regex has led to a match.
///
/// # Methods
///
/// `new`: create a new `RegexBuilder`
/// `with_named_regex`: add a regex, and bind it to the given name
/// `build`: consume the `RegexBuilder` and return the `CompiledRegex`
#[derive(Debug)]
pub struct RegexBuilder {
    names: Vec<String>,
    regexes: Vec<Regex>,
    groups: Vec<(usize, usize)>,
    current: usize,
}

impl RegexBuilder {
    /// Create a new `RegexBuilder`.
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            regexes: Vec::new(),
            groups: Vec::new(),
            current: 0,
        }
    }

    /// Add a regex, and bind it to the given name.
    /// The regex is read when it is added, so `with_named_regex`
    /// might fail, if the provided regex is malformed.
    pub fn with_named_regex(
        mut self,
        regex: &str,
        name: String,
        keyword: bool,
    ) -> Result<Self, RegexError> {
        self.names.push(name);
        let (regex, groups) = read(regex, self.current)?;
        let regex = if keyword {
            Regex::Concat(Box::new(regex), Box::new(Regex::WordBoundary))
        } else {
            regex
        };
        self.groups.push((self.current, groups));
        self.current = groups;
        self.regexes.push(regex);
        Ok(self)
    }

    /// Return the `CompiledRegex`. This consumes the `RegexBuilder`.
    pub fn build(self) -> CompiledRegex {
        if self.regexes.is_empty() {
            return CompiledRegex::new(
                Program::new(),
                self.names.into(),
                self.groups.into(),
                self.current,
            );
        }
        let mut program = Program::new();
        let mut switch = Vec::new();
        program.push(Instruction::Split(0.into(), 0.into())); // Fake instruction that is going to be replaced later with a `Switch`.
        for (id, regex) in self
            .regexes
            .into_iter()
            .enumerate()
            .map(|(id, regex)| (TerminalId(id), regex))
        {
            let ip = InstructionPointer(program.len());
            switch.push((id, ip));
            build(regex, &mut program);
            program.push(Instruction::Match(id));
        }

        program[InstructionPointer(0)] = Instruction::Switch(switch);
        CompiledRegex::new(program, self.names.into(), self.groups.into(), self.current)
    }
}

impl Default for RegexBuilder {
    fn default() -> Self {
        Self::new()
    }
}
