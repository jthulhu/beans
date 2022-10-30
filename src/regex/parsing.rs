use std::{iter::Enumerate, str::Chars};

use super::matching::{Instruction, Program};
use crate::regex::matching::InstructionPointer;
use unbounded_interval_tree::interval_tree::IntervalTree;

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::lexer::TerminalId;

    /// Compile a regex into a program executable on the VM.
    pub fn compile(
        regex: &str,
        id: TerminalId,
    ) -> Result<(Program, usize), RegexError> {
        let mut program = Program::new();
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
    fn read_multiline_comment() {
        use std::ops::Bound::Included;
        use Regex::*;
        let a = read(r"/\*([^*]|\*[^/])*\*/", 0).unwrap();
        let mut first_class = IntervalTree::default();
        first_class.insert((Included('*'), Included('*')));
        let mut second_class = IntervalTree::default();
        second_class.insert((Included('/'), Included('/')));
        // let b = Concat(
        //     Box::new(Char('/')),
        //     Box::new(Concat(
        //         Box::new(Char('*')),
        //         Box::new(Concat(
        //             Box::new(KleeneStar(Box::new(Group(
        //                 Box::new(Option(
        //                     Box::new(CharacterClass(first_class, true)),
        //                     Box::new(Concat(
        //                         Box::new(Char('*')),
        //                         Box::new(CharacterClass(second_class, true)),
        //                     )),
        //                 )),
        //                 0,
        //             )))),
        //             Box::new(Concat(Box::new(Char('*')), Box::new(Char('/')))),
        //         )),
        //     )),
        // );
        let b = Concat(
            Box::new(Concat(
                Box::new(Concat(
                    Box::new(Concat(Box::new(Char('/')), Box::new(Char('*')))),
                    Box::new(KleeneStar(Box::new(Group(
                        Box::new(Option(
                            Box::new(CharacterClass(first_class, true)),
                            Box::new(Concat(
                                Box::new(Char('*')),
                                Box::new(CharacterClass(second_class, true)),
                            )),
                        )),
                        0,
                    )))),
                )),
                Box::new(Char('*')),
            )),
            Box::new(Char('/')),
        );
        assert_eq!(a, (b, 1));
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
            RegexError {
                position: 8,
                message: String::from(
                    "Found 1 overlaps in character class from position 0 to position 8: (a-z,b-z)"
                ),
            }
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
                    Box::new(Group(
                        Box::new(Repetition(Box::new(Char('a')))),
                        0
                    )),
                    Box::new(Group(
                        Box::new(Repetition(Box::new(Char('b')))),
                        1
                    ))
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
                    Box::new(Group(
                        Box::new(KleeneStar(Box::new(Char('a')))),
                        0
                    )),
                    Box::new(Group(
                        Box::new(KleeneStar(Box::new(Char('b')))),
                        1
                    ))
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
                        Box::new(Concat(
                            Box::new(Char('a')),
                            Box::new(Char('b')),
                        )),
                        Box::new(Char('c'))
                    )),
                    Box::new(Concat(
                        Box::new(Concat(
                            Box::new(Char('b')),
                            Box::new(Char('c')),
                        )),
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
                        Box::new(Concat(
                            Box::new(Char('a')),
                            Box::new(Char('b'))
                        )),
                        Box::new(Concat(
                            Box::new(Char('c')),
                            Box::new(Char('d'))
                        ))
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
            if let Err(RegexError { position: pos, .. }) = read(regex, 0) {
                assert_eq!(pos, p);
            } else {
                panic!("(/{}/ shouldn't succeed.", regex);
            }
        }
    }

    #[test]
    fn build_basic() {
        use Instruction::*;
        let program = compile("a+b+", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![
                    Char('a'),
                    Split(InstructionPointer(0), InstructionPointer(2)),
                    Char('b'),
                    Split(InstructionPointer(2), InstructionPointer(4)),
                    Match(TerminalId(0))
                ]),
                0
            )
        );
    }

    #[test]
    fn build_char() {
        use Instruction::*;
        let program = compile("a", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (Program::from(vec![Char('a'), Match(TerminalId(0))]), 0)
        );
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
        let program = compile("[a-zA-Z0-9_]", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![
                    CharacterClass(tree, false),
                    Match(TerminalId(0))
                ]),
                0
            )
        );
    }

    #[test]
    fn build_word_boundary() {
        use Instruction::*;
        let program = compile(r"\b", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (Program::from(vec![WordBoundary, Match(TerminalId(0))]), 0)
        );
    }

    #[test]
    fn build_eof() {
        use Instruction::*;
        let program = compile(r"\z", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (Program::from(vec![EOF, Match(TerminalId(0))]), 0)
        );
        let program = compile(r"\Z", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (Program::from(vec![EOF, Match(TerminalId(0))]), 0)
        );
    }

    #[test]
    fn build_escaped() {
        use Instruction::*;
        let program = compile(r"\w", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (Program::from(vec![WordChar, Match(TerminalId(0))]), 0)
        );
    }

    #[test]
    fn build_concat() {
        use Instruction::*;
        let program = compile("ab", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![Char('a'), Char('b'), Match(TerminalId(0))]),
                0
            )
        );
    }

    #[test]
    fn build_option() {
        use Instruction::*;
        let program = compile("a|b", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![
                    Split(InstructionPointer(1), InstructionPointer(3)),
                    Char('a'),
                    Jump(InstructionPointer(4)),
                    Char('b'),
                    Match(TerminalId(0))
                ]),
                0
            )
        );
    }

    #[test]
    fn build_optional() {
        use Instruction::*;
        let program = compile("a?", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![
                    Split(InstructionPointer(1), InstructionPointer(2)),
                    Char('a'),
                    Match(TerminalId(0))
                ]),
                0
            )
        );
    }

    #[test]
    fn build_kleene_star() {
        use Instruction::*;
        let program = compile("a*", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![
                    Split(InstructionPointer(1), InstructionPointer(3)),
                    Char('a'),
                    Jump(InstructionPointer(0)),
                    Match(TerminalId(0))
                ]),
                0
            )
        );
    }

    #[test]
    fn build_repetition() {
        use Instruction::*;
        let program = compile("a+", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![
                    Char('a'),
                    Split(InstructionPointer(0), InstructionPointer(2)),
                    Match(TerminalId(0))
                ]),
                0
            )
        );
    }

    #[test]
    fn build_any() {
        use Instruction::*;
        let program = compile(".", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (Program::from(vec![Any, Match(TerminalId(0))]), 0)
        );
    }

    #[test]
    fn build_groups() {
        use Instruction::*;
        let program = compile("(a+)(b+)", TerminalId(0)).unwrap();
        assert_eq!(
            program,
            (
                Program::from(vec![
                    Save(0),
                    Char('a'),
                    Split(InstructionPointer(1), InstructionPointer(3)),
                    Save(1),
                    Save(2),
                    Char('b'),
                    Split(InstructionPointer(5), InstructionPointer(7)),
                    Save(3),
                    Match(TerminalId(0))
                ]),
                2
            )
        )
    }

    #[test]
    fn build_string() {
        use std::collections::Bound::Included;
        use Instruction::*;
        let program =
            compile(r"'(([^'\\]|\\[^\\]|\\\\)*)'", TerminalId(0)).unwrap();
        let mut first_char_class = IntervalTree::default();
        first_char_class.insert((Included('\''), Included('\'')));
        first_char_class.insert((Included('\\'), Included('\\')));
        let mut second_char_class = IntervalTree::default();
        second_char_class.insert((Included('\\'), Included('\\')));
        let correct = Program::from(vec![
            Char('\''),
            Save(0),
            Split(InstructionPointer(3), InstructionPointer(15)),
            Save(2),
            Split(InstructionPointer(5), InstructionPointer(11)),
            Split(InstructionPointer(6), InstructionPointer(8)),
            CharacterClass(first_char_class, true),
            Jump(InstructionPointer(10)),
            Char('\\'),
            CharacterClass(second_char_class, true),
            Jump(InstructionPointer(13)),
            Char('\\'),
            Char('\\'),
            Save(3),
            Jump(InstructionPointer(2)),
            Save(1),
            Char('\''),
            Match(TerminalId(0)),
        ]);
        assert_eq!(program, (correct, 2));
    }
}

/// # Summary
///
/// `Regex` represents any successfully parsed regex.
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
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
#[derive(Debug, PartialEq, Eq)]
pub struct RegexError {
    pub position: usize,
    pub message: String,
}

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
            let split_pos = InstructionPointer(program.len());
            program.push(Instruction::Split(
                InstructionPointer(0),
                InstructionPointer(0),
            ));
            build(*r1, program);
            let jmp_pos = program.len_ip();
            program.push(Instruction::Jump(InstructionPointer(0)));
            build(*r2, program);
            program[split_pos] =
                Instruction::Split(split_pos.incr(), jmp_pos.incr());
            program[jmp_pos] = Instruction::Jump(program.len_ip());
        }
        Regex::Concat(r1, r2) => {
            build(*r1, program);
            build(*r2, program);
        }
        Regex::Optional(r) => {
            let split_pos = program.len_ip();
            program.push(Instruction::Split(
                InstructionPointer(0),
                InstructionPointer(0),
            ));
            build(*r, program);
            program[split_pos] =
                Instruction::Split(split_pos.incr(), program.len_ip());
        }
        Regex::KleeneStar(r) => {
            let split_pos = program.len_ip();
            program.push(Instruction::Split(
                InstructionPointer(0),
                InstructionPointer(0),
            ));
            build(*r, program);
            program.push(Instruction::Jump(split_pos));
            program[split_pos] =
                Instruction::Split(split_pos.incr(), program.len_ip());
        }
        Regex::Repetition(r) => {
            let init_pos = program.len_ip();
            build(*r, program);
            program.push(Instruction::Split(init_pos, program.len_ip().incr()));
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
pub fn read(
    regex: &str,
    mut groups: usize,
) -> Result<(Regex, usize), RegexError> {
    /// Parse a character class.
    fn read_char_class(
        input: &mut std::iter::Enumerate<std::str::Chars<'_>>,
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

        fn read_escaped_char(
            input: &mut Enumerate<Chars<'_>>,
            pos: usize,
        ) -> Result<char, RegexError> {
            if let Some((pos, c)) = input.next() {
                match c {
                    ']' => Ok(c),
                    't' => Ok('\t'),
                    'n' => Ok('\n'),
                    '^' => Ok('^'),
                    '-' => Ok('-'),
                    '\\' => Ok('\\'),
                    _ => {
                        Err(RegexError {
                            position: pos,
                            message: format!(
                                "Cannot escape character inside character class {}, try replacing /\\{}/ with /{}/",
                                c, c, c
                            ),
                        })
                    }
                }
            } else {
                Err(RegexError {
                    position: pos,
                    message: String::from(
                        "Expected something after '\', but found EOF instead",
                    ),
                })
            }
        }

        while let Some((pos, chr)) = input.next() {
            match chr {
                '\\' => {
                    if let Some(cr) = last {
                        insert(
                            (Included(cr), Included(cr)),
                            &mut tree,
                            &mut overlaps,
                        );
                    }
                    last = Some(read_escaped_char(input, pos)?);
                }
                '^' if pos == actual + 1 => {
                    negated = true;
                }
                '-' => {
                    if let Some(c1) = last {
                        if let Some((_, c2)) = input.next() {
                            let chr = if c2 == '\\' {
                                read_escaped_char(input, pos)?
                            } else {
                                c2
                            };
                            insert(
                                (Included(c1), Included(chr)),
                                &mut tree,
                                &mut overlaps,
                            );
                        } else {
                            return Err(RegexError {
                                position: pos,
                                message: String::from(
                                    "Expected something after '-', but found EOF instead",
                                ),
                            });
                        }
                        last = None;
                    } else {
                        insert(
                            (Included('-'), Included('-')),
                            &mut tree,
                            &mut overlaps,
                        );
                    }
                }
                ']' => {
                    if let Some(c) = last {
                        insert(
                            (Included(c), Included(c)),
                            &mut tree,
                            &mut overlaps,
                        );
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
                        return Err(RegexError {
                            position: pos + 1,
                            message: err_str,
                        });
                    } else {
                        return Ok(Regex::CharacterClass(tree, negated));
                    }
                }
                c => {
                    if let Some(cr) = last {
                        insert(
                            (Included(cr), Included(cr)),
                            &mut tree,
                            &mut overlaps,
                        );
                    }
                    last = Some(c);
                }
            }
        }
        Err(RegexError {position: size, message: format!("Expected end of character class, but found EOF. Try adding ']' if you really meant to have a character class, or adding '\\' at position {} if you didn't want a character class", actual)})
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
            Regex::Concat(r1, r2) => {
                Ok(Regex::Concat(r1, Box::new(kleene_star(*r2, pos)?)))
            }
            Regex::Option(r1, r2) => {
                Ok(Regex::Option(r1, Box::new(kleene_star(*r2, pos)?)))
            }
            Regex::Empty => Err(RegexError {
                position: pos,
                message: String::from(
                    "Cannot apply kleene star to empty regex.",
                ),
            }),
            Regex::KleeneStar(..) => Err(RegexError {
                position: pos,
                message: String::from("Cannot apply kleene star twice."),
            }),
            Regex::Optional(..) => Err(RegexError {
                position: pos,
                message: String::from(
                    "Cannot apply kleene star on an optional group.",
                ),
            }),
            Regex::Repetition(..) => Err(RegexError {
                position: pos,
                message: String::from(
                    "Cannot apply kleene star and repetition.",
                ),
            }),
            r => Ok(Regex::KleeneStar(Box::new(r))),
        }
    }

    fn repetition(exp: Regex, pos: usize) -> Result<Regex, RegexError> {
        match exp {
            Regex::Concat(r1, r2) => {
                Ok(Regex::Concat(r1, Box::new(repetition(*r2, pos)?)))
            }
            Regex::Option(r1, r2) => {
                Ok(Regex::Option(r1, Box::new(repetition(*r2, pos)?)))
            }
            Regex::Empty => Err(RegexError {
                position: pos,
                message: String::from(
                    "Cannot apply repetition to empty regex.",
                ),
            }),
            Regex::KleeneStar(..) => Err(RegexError {
                position: pos,
                message: String::from("Cannot apply repetition twice."),
            }),
            Regex::Optional(..) => Err(RegexError {
                position: pos,
                message: String::from(
                    "Cannot apply repetition on an optional group.",
                ),
            }),
            Regex::Repetition(..) => Err(RegexError {
                position: pos,
                message: String::from(
                    "Cannot apply repetition and kleene star.",
                ),
            }),
            r => Ok(Regex::Repetition(Box::new(r))),
        }
    }

    fn optional(exp: Regex, pos: usize) -> Result<Regex, RegexError> {
        match exp {
            Regex::Concat(r1, r2) => {
                Ok(Regex::Concat(r1, Box::new(optional(*r2, pos)?)))
            }
            Regex::Option(r1, r2) => {
                Ok(Regex::Option(r1, Box::new(optional(*r2, pos)?)))
            }
            Regex::Empty => Err(RegexError {
                position: pos,
                message: String::from("Cannot apply optional to empty regex."),
            }),
            Regex::KleeneStar(..) => Err(RegexError {
                position: pos,
                message: String::from(
                    "Non-greedy kleene star is not supported.",
                ),
            }),
            Regex::Optional(..) => Err(RegexError {
                position: pos,
                message: String::from("Non-greedy optional is not supported."),
            }),
            Regex::Repetition(..) => Err(RegexError {
                position: pos,
                message: String::from(
                    "Non-greedy repetition is not supported.",
                ),
            }),
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
                    return Err(RegexError {
                        position: pos,
                        message: String::from("Closing parenthesis doesn't match any previously opened."),
                    });
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
	    '$' => return Err(RegexError {
		position: pos,
		message: String::from("End-of-line /$/ is not supported. Matches are anchored anyways. Try /\\n/ instead.")
	    }),
	    '^' => return Err(RegexError {
		position: pos,
		message: String::from("Start-of-line /^/ is not supported. Matches are anchored anyways.")
	    }),
            '.' => add(Regex::Any, &mut stack),
            '\\' => {
                if let Some((pos, chr)) = chrs.next() {
                    match chr {
			'\\' | '.' | '(' | ')' | '?' | '+' | '*' | '|' | '$' | '^' | '[' => add(Regex::Char(chr), &mut stack),
			'A' => return Err(RegexError {
			    position: pos,
			    message: String::from("Start of the string anchor /\\A/ is not supported.")
			}),
			'N' => return Err(RegexError {
			    position: pos,
			    message: String::from("Not a line break shorthand /\\N/ is not supported. Try /[^\\n]/ instead.")
			}),
			'Z' => add(Regex::EOF, &mut stack),
			'b' => add(Regex::WordBoundary, &mut stack),
			'd' => add(Regex::Digit, &mut stack),
			'h' => return Err(RegexError {
			    position: pos,
			    message: String::from("Horizontal whitespace / hexadecimal digit shorthand /\\h/ is not supported. Try [0-9a-f] instead if you wanted hexadecimal digit.")
			}),
			'l' => return Err(RegexError {
			    position: pos,
			    message: String::from("Lowercase shorthand /\\l/ is not supported. Try /[a-z]/ instead.")
			}),
			'n' => add(Regex::Char('\n'), &mut stack),
			'r' => return Err(RegexError {
			    position: pos,
			    message: String::from("Nonsense EOL /\\r/ is not supported. Try /\\n/ instead.")
			}),
			's' => add(Regex::Whitespace, &mut stack),
			't' => add(Regex::Char('\t'), &mut stack),
			'u' => return Err(RegexError {
			    position: pos,
			    message: String::from("Uppercase shorthand /\\u/ is not supported. Try /[A-Z]/ instead.")
			}),
			'v' => return Err(RegexError {
			    position: pos,
			    message: String::from("Vertical whitespace shorthand /\\v/ is not supported.")
			}),
                        'w' => add(Regex::WordChar, &mut stack),
			'x' => return Err(RegexError {
			    position: pos,
			    message: String::from("Hexadecimal escape /\\xFF/ is not supported.")
			}),
			'z' => add(Regex::EOF, &mut stack),
                        _ => {
                            return Err(RegexError {
                                position: pos,
                                message: format!(
                                    "Cannot escape character {}, try replacing /\\{}/ with /{}/",
                                    chr, chr, chr
                                ),
                            })
                        }
                    }
                } else {
                    return Err(RegexError {
                        position: pos,
                        message: String::from(
                            "Expected something after character '\', but found EOF instead",
                        ),
                    });
                }
            }
            '[' => {
                add(read_char_class(&mut chrs, size, pos)?, &mut stack);
            }
            c => add(Regex::Char(c), &mut stack),
        }
    }
    if stack.len() != 1 {
        Err(RegexError {
            position: size,
            message: format!(
                "Expected {} closing parenthesis, found EOF",
                stack.len() - 1
            ),
        })
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
