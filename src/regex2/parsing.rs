use super::matching::{Instruction, Program};
use unbounded_interval_tree::IntervalTree;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_char() {
        use Regex::*;
        assert_eq!(read("a", 0).unwrap(), (Char('a'), 0));
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
	assert_eq!(read("[a-zA-Z0-9_]", 0).unwrap(), (CharacterClass(tree, false), 0));

	let mut tree = IntervalTree::default();
	tree.insert((Included('a'), Included('z')));
	tree.insert((Included('A'), Included('Z')));
	tree.insert((Included('0'), Included('9')));
	tree.insert((Included('_'), Included('_')));
	assert_eq!(read("[^a-zA-Z0-9_]", 0).unwrap(), (CharacterClass(tree, true), 0));

	assert_eq!(read("[a-zb-z]", 0).unwrap_err(), (8, String::from("Found 1 overlaps in character class from position 0 to position 8: (a-z,b-z)")));
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
            if let Err((pos, msg)) = read(regex, 0) {
                assert_eq!(pos, p);
            } else {
                panic!(format!("/{}/ shouldn't succeed.", regex));
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
    Any,
    Empty,
}

pub type RegexError = (usize, String);

impl Into<Regex> for (Regex, Option<Regex>) {
    fn into(self) -> Regex {
        match self {
            (r, Some(r2)) => Regex::Option(Box::new(r2), Box::new(r)),
            (r, None) => r,
        }
    }
}

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
        Regex::Any => {
            program.push(Instruction::Any);
        }
        Regex::WordChar => {
            program.push(Instruction::WordChar);
        }
        Regex::CharacterClass(class, negated) => {
            program.push(Instruction::CharacterClass(class, negated));
        }
        Regex::Empty => {}
    };
}

pub fn compile(regex: &str, id: usize) -> Result<(Program, usize), RegexError> {
    let mut program = Vec::new();
    let (regex, nb_groups) = read(regex, 0)?;
    build(regex, &mut program);
    program.push(Instruction::Match(id));
    Ok((program, nb_groups))
}

pub fn read(regex: &str, mut groups: usize) -> Result<(Regex, usize), RegexError> {
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
                        last = Some(c);
                    } else {
                        return Err((
                            pos,
                            String::from("Expected something after '\', but found EOF instead"),
                        ));
                    }
                }
                '^' if pos == actual+1 => {
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
                        return Err((pos+1, err_str));
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

    fn add(regex: Regex, stack: &mut Vec<(Regex, Option<Regex>)>) {
        let (last, remainder) = stack.pop().unwrap();
        stack.push((concat(last, regex), remainder));
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

    let mut stack = vec![(Regex::Empty, None)];
    let mut chrs = regex.chars().enumerate();
    let size = regex.chars().count();
    while let Some((pos, chr)) = chrs.next() {
        match chr {
            '(' => {
                stack.push((Regex::Empty, None));
            }
            ')' => {
                if stack.len() <= 1 {
                    return Err((
                        pos,
                        String::from("Closing parenthesis doesn't match any previously opened."),
                    ));
                }
                {
                    let group = Regex::Group(Box::new(stack.pop().unwrap().into()), groups);
                    groups += 1;
                    let (last, remainder) = stack.pop().unwrap();
                    stack.push((concat(last, group), remainder));
                }
            }
            '?' => {
                let (last, remainder) = stack.pop().unwrap();
                stack.push((optional(last, pos)?, remainder));
            }
            '*' => {
                let (last, remainder) = stack.pop().unwrap();
                stack.push((kleene_star(last, pos)?, remainder));
            }
            '+' => {
                let (last, remainder) = stack.pop().unwrap();
                stack.push((repetition(last, pos)?, remainder));
            }
            '|' => {
                let last = stack.pop().unwrap().into();
                stack.push((Regex::Empty, Some(last)));
            }
            '.' => add(Regex::Any, &mut stack),
            '\\' => {
                if let Some((pos, chr)) = chrs.next() {
                    match chr {
                        'w' => add(Regex::WordChar, &mut stack),
                        _ => {
                            return Err((
                                pos,
                                format!(
                                    "Cannot escape character {}, try replacing \\{} with {}",
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
        Ok((stack.pop().unwrap().into(), groups))
    }
}
