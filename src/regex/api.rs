use super::matching::{self, find, Instruction, Program};
use super::parsing::{build, read, Regex, RegexError};
use fixedbitset::FixedBitSet;

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
        assert_eq!(regex.groups, vec![(0, 0), (0, 0)]);
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
            .with_named_regex("(a+)", String::from("As"))
            .unwrap()
            .with_named_regex("(b)(c)", String::from("BC"))
            .unwrap()
            .build();

        let match1 = regex.find("aaacd", &Allowed::All).unwrap();
        assert_eq!(match1.length, 3);
        assert_eq!(match1.name, "As");
        assert_eq!(match1.groups.len(), 1);
        let handle = match1.groups[0].as_ref().unwrap();
        assert_eq!(handle.start, 0);
        assert_eq!(handle.end, 3);
        assert_eq!(handle.text, "aaa");
        assert_eq!(match1.text, "aaa");

        let match2 = regex.find("bc", &Allowed::All).unwrap();
        assert_eq!(match2.length, 2);
        assert_eq!(match2.name, "BC");
        assert_eq!(match2.groups.len(), 2);
        assert_eq!(match2.text, "bc");
        let handle = match2.groups[0].as_ref().unwrap();
        assert_eq!(handle.start, 0);
        assert_eq!(handle.end, 1);
        assert_eq!(handle.text, "b");
        let handle = match2.groups[1].as_ref().unwrap();
        assert_eq!(handle.start, 1);
        assert_eq!(handle.end, 2);
        assert_eq!(handle.text, "c");

        let match3 = regex.find("cde", &Allowed::All);
        assert!(match3.is_none());
    }

    #[test]
    fn groups() {
        let regex = RegexBuilder::new()
            .with_named_regex("'(.*)'", String::from("STRING"))
            .unwrap()
            .with_named_regex("\"(.*)\"", String::from("STRING"))
            .unwrap()
            .build();
        let match1 = regex.find("'blabla'", &Allowed::All).unwrap();
        assert_eq!(match1.length, 8);
        assert_eq!(match1.name, "STRING");
        assert_eq!(match1.groups.len(), 1);
        assert_eq!(match1.text, "'blabla'");
        let handle = match1.groups[0].as_ref().unwrap();
        assert_eq!(handle.start, 1);
        assert_eq!(handle.end, 7);
        assert_eq!(handle.text, "blabla");
    }

    #[test]
    fn any() {
        let regex = RegexBuilder::new()
            .with_named_regex(".*", String::from("Default"))
            .unwrap()
            .build();
        assert_eq!(regex.find("0123456", &Allowed::All).unwrap().length, 7);
        assert_eq!(regex.find("012", &Allowed::All).unwrap().length, 3);
        assert_eq!(regex.find("", &Allowed::All).unwrap().length, 0);
    }
}

pub enum Allowed {
    All,
    Some(Vec<usize>)
}

impl Allowed {
    pub fn convert(&self, size: usize) -> matching::Allowed {
	match self {
	    Allowed::All => matching::Allowed::All,
	    Allowed::Some(rules) => {
		let mut allowed = FixedBitSet::with_capacity(size);
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
pub struct Handle<'a> {
    start: usize,
    end: usize,
    text: &'a str,
}

impl<'a> Handle<'a> {
    /// Create a new `Handle`
    pub fn new(start: usize, end: usize, text: &'a str) -> Self {
        Self { start, end, text }
    }
    /// Return the start position of the region (inclusive).
    pub fn start(&self) -> usize {
        self.start
    }

    /// Return the end position of the region (exclusive).
    pub fn end(&self) -> usize {
        self.end
    }

    /// Return the captured region as a slice of the input.
    pub fn text(&self) -> &str {
        self.text
    }

    /// Return the length of the captured region.
    pub fn length(&self) -> usize {
        self.end - self.start
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
pub struct Match<'a> {
    length: usize,
    name: &'a str,
    id: usize,
    groups: Vec<Option<Handle<'a>>>,
    text: &'a str,
}

impl Match<'_> {
    /// Return the length of the match.
    pub fn length(&self) -> usize {
        self.length
    }

    /// Return the identifier of the regex which led to the match.
    pub fn id(&self) -> usize {
        self.id
    }

    /// Return the name of the regex which led to the match.
    pub fn name(&self) -> &str {
        self.name
    }

    /// Return the groups of the regex. The position of each group is
    /// enforced by its position in the regex. Each group may or may not
    /// have been caught.
    pub fn groups(&self) -> &[Option<Handle<'_>>] {
        &self.groups[..]
    }

    /// Return the substring of the input that corresponds to the match.
    pub fn text(&self) -> &str {
        self.text
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
#[derive(Debug, PartialEq)]
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

    /// Match against a given input. Will return only one match, if many were possibles,
    /// according to the priority rules.
    pub fn find<'a>(&'a self, input: &'a str, allowed: &Allowed) -> Option<Match<'a>> {
        if let Some((length, id, groups)) = find(&self.program, input, self.size, &allowed.convert(self.names.len())) {
            let (begin_groups, end_groups) = self.groups[id];
            let mut grps = Vec::new();
            for i in begin_groups..end_groups {
                if let Some(start) = groups[2 * i] {
                    let end = groups[2 * i + 1].unwrap();
                    let handle = Handle {
                        start,
                        end,
                        text: &input[start..end],
                    };
                    grps.push(Some(handle));
                } else {
                    grps.push(None);
                }
            }
            Some(Match {
                length,
                id,
                name: &self.names[id],
                groups: grps,
                text: &input[..length],
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
///
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
    pub fn with_named_regex(mut self, regex: &str, name: String) -> Result<Self, RegexError> {
        self.names.push(name);
        let (regex, groups) = read(regex, self.current)?;
        self.groups.push((self.current, groups));
        self.current = groups;
        self.regexes.push(regex);
        Ok(self)
    }

    /// Return the `CompiledRegex`. This consumes the `RegexBuilder`.
    pub fn build(self) -> CompiledRegex {
        if self.regexes.is_empty() {
            return CompiledRegex::new(Vec::new(), self.names, self.groups, self.current);
        }
        let offset = self.regexes.len() - 1;
        let mut program = Vec::new();
        for _ in 0..offset {
            program.push(Instruction::Split(0, 0));
        }
        for (i, regex) in self.regexes.into_iter().enumerate() {
            if i < offset {
                program[i] = Instruction::Split(program.len(), i + 1);
            } else if offset != 0 {
                if let Instruction::Split(next, _) = program[i - 1] {
                    program[i - 1] = Instruction::Split(next, program.len());
                }
            }
            build(regex, &mut program);
            program.push(Instruction::Match(i));
        }
        CompiledRegex::new(program, self.names, self.groups, self.current)
    }
}

impl Default for RegexBuilder {
    fn default() -> Self {
        Self::new()
    }
}
