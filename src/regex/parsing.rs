use super::matching::{Instruction, Program};
use unbounded_interval_tree::IntervalTree;

#[cfg(test)]
pub mod tests {
    use super::*;

    /// Compile a regex into a program executable on the VM.
    /// **This is a private function, please use the API instead.**
    pub fn compile(regex: &str, id: usize) -> Result<(Program, usize), RegexError> {
        let mut program = Vec::new();
        let (regex, nb_groups) = read(regex, 0)?;
        build(regex, &mut program);
        program.push(Instruction::Match(id));
        Ok((program, nb_groups))
    }

    #[test]
    fn read_char() {
        use Regex::*;
        assert_eq!(read("a", 0).unwrap(), (Char('a'), 0));
    }

    #[test]
    fn read_word_boundary() {
        use Regex::*;
        assert_eq!(read(r"\b", 0).unwrap(), (WordBoundary, 0));
    }

    #[test]
    fn read_eof() {
        use Regex::*;
        assert_eq!(read(r"\Z", 0).unwrap(), (EOF, 0));
        assert_eq!(read(r"\z", 0).unwrap(), (EOF, 0));
    }

    #[test]
    fn read_char_class() {
        use std::ops::Bound::Included;
        use Regex::*;
        let mut tree = IntervalTree::default();
        tree.insert((Included('a'), Included('a')));
        assert_eq!(read("[a]", 0).unwrap(), (CharacterClass(tree, false), 0));

        let mut tree = IntervalTree::default();
        tree.insert((Included('a'), Included('z')));
        tree.insert((Included('A'), Included('Z')));
        tree.insert((Included('0'), Included('9')));
        tree.insert((Included('_'), Included('_')));
        assert_eq!(
            read("[a-zA-Z0-9_]", 0).unwrap(),
            (CharacterClass(tree, false), 0)
        );

        let mut tree = IntervalTree::default();
        tree.insert((Included('a'), Included('z')));
        tree.insert((Included('A'), Included('Z')));
        tree.insert((Included('0'), Included('9')));
        tree.insert((Included('_'), Included('_')));
        assert_eq!(
            read("[^a-zA-Z0-9_]", 0).unwrap(),
            (CharacterClass(tree, true), 0)
        );

        assert_eq!(
            read("[a-zb-z]", 0).unwrap_err(),
            (
                8,
                String::from(
                    "Found 1 overlaps in character class from position 0 to position 8: (a-z,b-z)"
                )
            )
        );
    }

    #[test]
    fn read_escaped() {
        use Regex::*;
        assert_eq!(read(r"\w", 0).unwrap(), (WordChar, 0));
    }

    #[test]
    #[should_panic]
    fn read_wrong_escaped() {
        read(r"\l", 0).unwrap();
    }

    #[test]
    fn read_group() {
        use Regex::*;
        assert_eq!(read("(a)", 0).unwrap(), (Group(Box::new(Char('a')), 0), 1));
    }

    #[test]
    fn read_repetition() {
        use Regex::*;
        assert_eq!(
            read("a+b+", 0).unwrap(),
            (
                Concat(
                    Box::new(Repetition(Box::new(Char('a')))),
                    Box::new(Repetition(Box::new(Char('b'))))
                ),
                0
            )
        );

        assert_eq!(
            read("(a+)(b+)", 0).unwrap(),
            (
                Concat(
                    Box::new(Group(Box::new(Repetition(Box::new(Char('a')))), 0)),
                    Box::new(Group(Box::new(Repetition(Box::new(Char('b')))), 1))
                ),
                2
            )
        );
    }

    #[test]
    fn read_kleene_star() {
        use Regex::*;

        assert_eq!(
            read("a*b*", 0).unwrap(),
            (
                Concat(
                    Box::new(KleeneStar(Box::new(Char('a')))),
                    Box::new(KleeneStar(Box::new(Char('b'))))
                ),
                0
            )
        );

        assert_eq!(
            read("(a*)(b*)", 0).unwrap(),
            (
                Concat(
                    Box::new(Group(Box::new(KleeneStar(Box::new(Char('a')))), 0)),
                    Box::new(Group(Box::new(KleeneStar(Box::new(Char('b')))), 1))
                ),
                2
            )
        );
    }

    #[test]
    fn read_option() {
        use Regex::*;

        assert_eq!(
            read("abc|bcd", 0).unwrap(),
            (
                Option(
                    Box::new(Concat(
                        Box::new(Concat(Box::new(Char('a')), Box::new(Char('b')),)),
                        Box::new(Char('c'))
                    )),
                    Box::new(Concat(
                        Box::new(Concat(Box::new(Char('b')), Box::new(Char('c')),)),
                        Box::new(Char('d'))
                    )),
                ),
                0
            )
        );

        assert_eq!(
            read("ab|cd|ef", 0).unwrap(),
            (
                Option(
                    Box::new(Option(
                        Box::new(Concat(Box::new(Char('a')), Box::new(Char('b')))),
                        Box::new(Concat(Box::new(Char('c')), Box::new(Char('d'))))
                    )),
                    Box::new(Concat(Box::new(Char('e')), Box::new(Char('f'))))
                ),
                0
            )
        );
    }

    #[test]
    fn read_any() {
        use Regex::*;
        assert_eq!(read(".", 0).unwrap(), (Any, 0));
        assert_eq!(read(".*", 0).unwrap(), (KleeneStar(Box::new(Any)), 0));
    }

    #[test]
    fn read_malformed() {
        let wrong_regex = [
            ("a**", 2),
            ("a*?", 2),
            ("a?*", 2),
            ("a*+", 2),
            ("a+*", 2),
            ("a??", 2),
            ("a?+", 2),
            ("a+?", 2),
            ("a++", 2),
            ("*", 0),
            ("?", 0),
            ("+", 0),
        ];
        for &(regex, p) in wrong_regex.iter() {
            if let Err((pos, _)) = read(regex, 0) {
                assert_eq!(pos, p);
            } else {
                panic!("(/{}/ shouldn't succeed.", regex);
            }
        }
    }

    #[test]
    fn build_basic() {
        use Instruction::*;
        let program = compile("a+b+", 0).unwrap();
        assert_eq!(
            program,
            (
                vec![Char('a'), Split(0, 2), Char('b'), Split(2, 4), Match(0)],
                0
            )
        );
    }

    #[test]
    fn build_char() {
        use Instruction::*;
        let program = compile("a", 0).unwrap();
        assert_eq!(program, (vec![Char('a'), Match(0)], 0));
    }

    #[test]
    fn build_char_class() {
        use std::ops::Bound::Included;
        use Instruction::*;
        let mut tree = IntervalTree::default();
        tree.insert((Included('a'), Included('z')));
        tree.insert((Included('A'), Included('Z')));
        tree.insert((Included('0'), Included('9')));
        tree.insert((Included('_'), Included('_')));
        let program = compile("[a-zA-Z0-9_]", 0).unwrap();
        assert_eq!(program, (vec![CharacterClass(tree, false), Match(0)], 0));
    }

    #[test]
    fn build_word_boundary() {
        use Instruction::*;
        let program = compile(r"\b", 0).unwrap();
        assert_eq!(program, (vec![WordBoundary, Match(0)], 0));
    }

    #[test]
    fn build_eof() {
        use Instruction::*;
        let program = compile(r"\z", 0).unwrap();
        assert_eq!(program, (vec![EOF, Match(0)], 0));
        let program = compile(r"\Z", 0).unwrap();
        assert_eq!(program, (vec![EOF, Match(0)], 0));
    }

    #[test]
    fn build_escaped() {
        use Instruction::*;
        let program = compile(r"\w", 0).unwrap();
        assert_eq!(program, (vec![WordChar, Match(0)], 0));
    }

    #[test]
    fn build_concat() {
        use Instruction::*;
        let program = compile("ab", 0).unwrap();
        assert_eq!(program, (vec![Char('a'), Char('b'), Match(0)], 0));
    }

    #[test]
    fn build_option() {
        use Instruction::*;
        let program = compile("a|b", 0).unwrap();
        assert_eq!(
            program,
            (
                vec![Split(1, 3), Char('a'), Jump(4), Char('b'), Match(0)],
                0
            )
        );
    }

    #[test]
    fn build_optional() {
        use Instruction::*;
        let program = compile("a?", 0).unwrap();
        assert_eq!(program, (vec![Split(1, 2), Char('a'), Match(0)], 0));
    }

    #[test]
    fn build_kleene_star() {
        use Instruction::*;
        let program = compile("a*", 0).unwrap();
        assert_eq!(
            program,
            (vec![Split(1, 3), Char('a'), Jump(0), Match(0)], 0)
        );
    }

    #[test]
    fn build_repetition() {
        use Instruction::*;
        let program = compile("a+", 0).unwrap();
        assert_eq!(program, (vec![Char('a'), Split(0, 2), Match(0)], 0));
    }

    #[test]
    fn build_any() {
        use Instruction::*;
        let program = compile(".", 0).unwrap();
        assert_eq!(program, (vec![Any, Match(0)], 0));
    }

    #[test]
    fn build_groups() {
        use Instruction::*;
        let program = compile("(a+)(b+)", 0).unwrap();
        assert_eq!(
            program,
            (
                vec![
                    Save(0),
                    Char('a'),
                    Split(1, 3),
                    Save(1),
                    Save(2),
                    Char('b'),
                    Split(5, 7),
                    Save(3),
                    Match(0)
                ],
                2
            )
        )
    }

    #[test]
    fn build_string() {
        use std::collections::Bound::Included;
        use Instruction::*;
        let program = compile(r"'(([^'\\]|\\[^\\]|\\\\)*)'", 0).unwrap();
        let mut first_char_class = IntervalTree::default();
        first_char_class.insert((Included('\''), Included('\'')));
        first_char_class.insert((Included('\\'), Included('\\')));
        let mut second_char_class = IntervalTree::default();
        second_char_class.insert((Included('\\'), Included('\\')));
        let correct = vec![
            Char('\''),
            Save(0),
            Split(3, 15),
            Save(2),
            Split(5, 11),
            Split(6, 8),
            CharacterClass(first_char_class, true),
            Jump(10),
            Char('\\'),
            CharacterClass(second_char_class, true),
            Jump(13),
            Char('\\'),
            Char('\\'),
            Save(3),
            Jump(2),
            Save(1),
            Char('\''),
            Match(0),
        ];
        assert_eq!(program, (correct, 2));
    }
}

/// # Summary
///
/// `Regex` represents any successfully parsed regex.
#[derive(PartialEq, Debug)]
pub enum Regex {
    Char(char),
    Option(Box<Regex>, Box<Regex>),
    Optional(Box<Regex>),
    Repetition(Box<Regex>),
    KleeneStar(Box<Regex>),
    Concat(Box<Regex>, Box<Regex>),
    Group(Box<Regex>, usize),
    CharacterClass(IntervalTree<char>, bool),
    WordChar,
    Digit,
    Whitespace,
    WordBoundary,
    EOF,
    Any,
    Empty,
}

/// # Summary
///
/// `RegexError` is an alias of a couple (usize, String).
/// The first element is the position at which the error occured,
/// and the second one is a description of the said error.
pub type RegexError = (usize, String);

impl From<(Regex, Option<Regex>)> for Regex {
    fn from(t: (Regex, Option<Regex>)) -> Self {
        match t {
            (r, Some(r2)) => Regex::Option(Box::new(r2), Box::new(r)),
            (r, None) => r,
        }
    }
}

/// Take a `Regex`, and a reference to a `Program` and
/// appends the implementation of the `Regex` at the
/// end of the `Program`.
///
/// **Warning**: this function is recursive, and a malicious input
/// may lead to a stack overflow. This is because this library
/// is really meant to be used in a context in which the end user
/// (the one providing the input) is also the one using the library.
pub fn build(regex: Regex, program: &mut Program) {
    match regex {
        Regex::Char(c) => {
            program.push(Instruction::Char(c));
        }
        Regex::Option(r1, r2) => {
            let split_pos = program.len();
            program.push(Instruction::Split(0, 0));
            build(*r1, program);
            let jmp_pos = program.len();
            program.push(Instruction::Jump(0));
            build(*r2, program);
            program[split_pos] = Instruction::Split(split_pos + 1, jmp_pos + 1);
            program[jmp_pos] = Instruction::Jump(program.len());
        }
        Regex::Concat(r1, r2) => {
            build(*r1, program);
            build(*r2, program);
        }
        Regex::Optional(r) => {
            let split_pos = program.len();
            program.push(Instruction::Split(0, 0));
            build(*r, program);
            program[split_pos] = Instruction::Split(split_pos + 1, program.len());
        }
        Regex::KleeneStar(r) => {
            let split_pos = program.len();
            program.push(Instruction::Split(0, 0));
            build(*r, program);
            program.push(Instruction::Jump(split_pos));
            program[split_pos] = Instruction::Split(split_pos + 1, program.len());
        }
        Regex::Repetition(r) => {
            let init_pos = program.len();
            build(*r, program);
            program.push(Instruction::Split(init_pos, program.len() + 1));
        }
        Regex::Group(r, i) => {
            program.push(Instruction::Save(2 * i));
            build(*r, program);
            program.push(Instruction::Save(2 * i + 1));
        }
        Regex::Any => program.push(Instruction::Any),
        Regex::WordChar => program.push(Instruction::WordChar),
        Regex::Digit => program.push(Instruction::Digit),
        Regex::WordBoundary => program.push(Instruction::WordBoundary),
        Regex::CharacterClass(class, negated) => {
            program.push(Instruction::CharacterClass(class, negated))
        }
        Regex::EOF => program.push(Instruction::EOF),
        Regex::Whitespace => program.push(Instruction::Whitespace),
        Regex::Empty => {}
    };
}

/// Parse a regex. The parsing technique is quite efficient,
/// essentially linear time.
/// **This is a private function, please use the API instead.**
pub fn read(regex: &str, mut groups: usize) -> Result<(Regex, usize), RegexError> {
    /// Parse a character class.
    fn read_char_class(
        input: &mut std::iter::Enumerate<std::str::Chars>,
        size: usize,
        actual: usize,
    ) -> Result<Regex, RegexError> {
        use std::ops::{Bound, Bound::Included};
        fn insert(
            interval: (Bound<char>, Bound<char>),
            tree: &mut IntervalTree<char>,
            overlaps: &mut Vec<Vec<(char, char)>>,
        ) {
            let over = tree.get_interval_overlaps(&interval);
            if !over.is_empty() {
                overlaps.push(
                    over.into_iter()
                        .chain(std::iter::once(&interval))
                        .map(|(start, end)| {
                            let s = match start {
                                Included(s) => *s,
                                _ => panic!(),
                            };
                            let e = match end {
                                Included(e) => *e,
                                _ => panic!(),
                            };
                            (s, e)
                        })
                        .collect(),
                );
            }
            tree.insert(interval);
        }
        let mut tree = IntervalTree::default();
        let mut last = None;
        let mut overlaps = Vec::new();
        let mut negated = false;
        while let Some((pos, chr)) = input.next() {
            match chr {
                '\\' => {
                    if let Some((pos, c)) = input.next() {
                        if let Some(cr) = last {
                            insert((Included(cr), Included(cr)), &mut tree, &mut overlaps);
                        }
                        match c {
                            ']' => last = Some(c),
                            't' => last = Some('\t'),
                            'n' => last = Some('\n'),
                            '^' => last = Some('^'),
                            '-' => last = Some('-'),
                            '\\' => last = Some('\\'),
                            _ => {
                                return Err((
                                    pos,
                                    format!(
                                    "Cannot escape character inside character class {}, try replacing /\\{}/ with /{}/",
                                    c, c, c
                                ),
                                ))
                            }
                        };
                    } else {
                        return Err((
                            pos,
                            String::from("Expected something after '\', but found EOF instead"),
                        ));
                    }
                }
                '^' if pos == actual + 1 => {
                    negated = true;
                }
                '-' => {
                    if let Some(c1) = last {
                        if let Some((_, c2)) = input.next() {
                            insert((Included(c1), Included(c2)), &mut tree, &mut overlaps);
                        } else {
                            return Err((
                                pos,
                                String::from("Expected something after '-', but found EOF instead"),
                            ));
                        }
                        last = None;
                    } else {
                        insert((Included('-'), Included('-')), &mut tree, &mut overlaps);
                    }
                }
                ']' => {
                    if let Some(c) = last {
                        insert((Included(c), Included(c)), &mut tree, &mut overlaps);
                    }
                    if !overlaps.is_empty() {
                        let mut err_str = format!("Found {} overlaps in character class from position {} to position {}: ", overlaps.len(), actual, pos+1);
                        for overlap in overlaps {
                            err_str.push('(');
                            for (s, e) in overlap {
                                err_str.push(s);
                                err_str.push('-');
                                err_str.push(e);
                                err_str.push(',');
                            }
                            err_str.pop();
                            err_str.push_str("), ");
                        }
                        err_str.pop();
                        err_str.pop();
                        return Err((pos + 1, err_str));
                    } else {
                        return Ok(Regex::CharacterClass(tree, negated));
                    }
                }
                c => {
                    if let Some(cr) = last {
                        insert((Included(cr), Included(cr)), &mut tree, &mut overlaps);
                    }
                    last = Some(c);
                }
            }
        }
        Err((size, format!("Expected end of character class, but found EOF. Try adding ']' if you really meant to have a character class, or adding '\\' at position {} if you didn't want a character class", actual)))
    }

    fn add(regex: Regex, stack: &mut Vec<(Regex, Option<Regex>, usize)>) {
        let (last, remainder, group) = stack.pop().unwrap();
        stack.push((concat(last, regex), remainder, group));
    }

    fn concat(left: Regex, right: Regex) -> Regex {
        if let Regex::Empty = left {
            right
        } else {
            Regex::Concat(Box::new(left), Box::new(right))
        }
    }

    fn kleene_star(exp: Regex, pos: usize) -> Result<Regex, RegexError> {
        match exp {
            Regex::Concat(r1, r2) => Ok(Regex::Concat(r1, Box::new(kleene_star(*r2, pos)?))),
            Regex::Option(r1, r2) => Ok(Regex::Option(r1, Box::new(kleene_star(*r2, pos)?))),
            Regex::Empty => Err((
                pos,
                String::from("Cannot apply kleene star to empty regex."),
            )),
            Regex::KleeneStar(..) => Err((pos, String::from("Cannot apply kleene star twice."))),
            Regex::Optional(..) => Err((
                pos,
                String::from("Cannot apply kleene star on an optional group."),
            )),
            Regex::Repetition(..) => Err((
                pos,
                String::from("Cannot apply kleene star and repetition."),
            )),
            r => Ok(Regex::KleeneStar(Box::new(r))),
        }
    }

    fn repetition(exp: Regex, pos: usize) -> Result<Regex, RegexError> {
        match exp {
            Regex::Concat(r1, r2) => Ok(Regex::Concat(r1, Box::new(repetition(*r2, pos)?))),
            Regex::Option(r1, r2) => Ok(Regex::Option(r1, Box::new(repetition(*r2, pos)?))),
            Regex::Empty => Err((pos, String::from("Cannot apply repetition to empty regex."))),
            Regex::KleeneStar(..) => Err((pos, String::from("Cannot apply repetition twice."))),
            Regex::Optional(..) => Err((
                pos,
                String::from("Cannot apply repetition on an optional group."),
            )),
            Regex::Repetition(..) => Err((
                pos,
                String::from("Cannot apply repetition and kleene star."),
            )),
            r => Ok(Regex::Repetition(Box::new(r))),
        }
    }

    fn optional(exp: Regex, pos: usize) -> Result<Regex, RegexError> {
        match exp {
            Regex::Concat(r1, r2) => Ok(Regex::Concat(r1, Box::new(optional(*r2, pos)?))),
            Regex::Option(r1, r2) => Ok(Regex::Option(r1, Box::new(optional(*r2, pos)?))),
            Regex::Empty => Err((pos, String::from("Cannot apply optional to empty regex."))),
            Regex::KleeneStar(..) => Err((
                pos,
                String::from("Non-greedy kleene star is not supported."),
            )),
            Regex::Optional(..) => {
                Err((pos, String::from("Non-greedy optional is not supported.")))
            }
            Regex::Repetition(..) => {
                Err((pos, String::from("Non-greedy repetition is not supported.")))
            }
            r => Ok(Regex::Optional(Box::new(r))),
        }
    }

    let mut stack = vec![(Regex::Empty, None, 0)];
    let mut chrs = regex.chars().enumerate();
    let size = regex.chars().count();
    while let Some((pos, chr)) = chrs.next() {
        match chr {
            '(' => {
                stack.push((Regex::Empty, None, groups));
		groups += 1;
            }
            ')' => {
                if stack.len() <= 1 {
                    return Err((
                        pos,
                        String::from("Closing parenthesis doesn't match any previously opened."),
                    ));
                }
                {
		    let (last, remainder, nb_group) = stack.pop().unwrap();
                    let group = Regex::Group(Box::new((last, remainder).into()), nb_group);
                    let (last, remainder, nb_group) = stack.pop().unwrap();
                    stack.push((concat(last, group), remainder, nb_group));
                }
            }
            '?' => {
                let (last, remainder, nb_group) = stack.pop().unwrap();
                stack.push((optional(last, pos)?, remainder, nb_group));
            }
            '*' => {
                let (last, remainder, nb_group) = stack.pop().unwrap();
                stack.push((kleene_star(last, pos)?, remainder, nb_group));
            }
            '+' => {
                let (last, remainder, nb_group) = stack.pop().unwrap();
                stack.push((repetition(last, pos)?, remainder, nb_group));
            }
            '|' => {
		let (l, remainder, group) = stack.pop().unwrap();
                let last = (l, remainder).into();
                stack.push((Regex::Empty, Some(last), group));
            }
	    '$' => return Err((
		pos,
		String::from("End-of-line /$/ is not supported. Matches are anchored anyways. Try /\\n/ instead.")
	    )),
	    '^' => return Err((
		pos,
		String::from("Start-of-line /^/ is not supported. Matches are anchored anyways.")
	    )),
            '.' => add(Regex::Any, &mut stack),
            '\\' => {
                if let Some((pos, chr)) = chrs.next() {
                    match chr {
			'\\' | '.' | '(' | ')' | '?' | '+' | '*' | '|' | '$' | '^' | '[' => add(Regex::Char(chr), &mut stack),
			'A' => return Err((
			    pos,
			    String::from("Start of the string anchor /\\A/ is not supported.")
			)),
			'N' => return Err((
			    pos,
			    String::from("Not a line break shorthand /\\N/ is not supported. Try /[^\\n]/ instead.")
			)),
			'Z' => add(Regex::EOF, &mut stack),
			'b' => add(Regex::WordBoundary, &mut stack),
			'd' => add(Regex::Digit, &mut stack),
			'h' => return Err((
			    pos,
			    String::from("Horizontal whitespace / hexadecimal digit shorthand /\\h/ is not supported. Try [0-9a-f] instead if you wanted hexadecimal digit.")
			)),
			'l' => return Err((
			    pos,
			    String::from("Lowercase shorthand /\\l/ is not supported. Try /[a-z]/ instead.")
			)),
			'n' => add(Regex::Char('\n'), &mut stack),
			'r' => return Err((
			    pos,
			    String::from("Nonsense EOL /\\r/ is not supported. Try /\\n/ instead.")
			)),
			's' => add(Regex::Whitespace, &mut stack),
			't' => add(Regex::Char('\t'), &mut stack),
			'u' => return Err((
			    pos,
			    String::from("Uppercase shorthand /\\u/ is not supported. Try /[A-Z]/ instead.")
			)),
			'v' => return Err((
			    pos,
			    String::from("Vertical whitespace shorthand /\\v/ is not supported.")
			)),
                        'w' => add(Regex::WordChar, &mut stack),
			'x' => return Err((
			    pos,
			    String::from("Hexadecimal escape /\\xFF/ is not supported.")
			)),
			'z' => add(Regex::EOF, &mut stack),
                        _ => {
                            return Err((
                                pos,
                                format!(
                                    "Cannot escape character {}, try replacing /\\{}/ with /{}/",
                                    chr, chr, chr
                                ),
                            ))
                        }
                    }
                } else {
                    return Err((
                        pos,
                        String::from(
                            "Expected something after character '\', but found EOF instead",
                        ),
                    ));
                }
            }
            '[' => {
                add(read_char_class(&mut chrs, size, pos)?, &mut stack);
            }
            c => add(Regex::Char(c), &mut stack),
        }
    }
    if stack.len() != 1 {
        Err((
            size,
            format!(
                "Expected {} closing parenthesis, found EOF",
                stack.len() - 1
            ),
        ))
    } else {
        Ok((
            {
                let (last, remainder, _) = stack.pop().unwrap();
                (last, remainder).into()
            },
            groups,
        ))
    }
}
