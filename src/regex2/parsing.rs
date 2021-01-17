use super::matching::{Instruction, Program};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_char() {
        use Regex::*;
        assert_eq!(read("a", 0).unwrap(), (Char('a'), 0));
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

#[derive(PartialEq, Eq, Debug)]
pub enum Regex {
    Char(char),
    Option(Box<Regex>, Box<Regex>),
    Optional(Box<Regex>),
    Repetition(Box<Regex>),
    KleeneStar(Box<Regex>),
    Concat(Box<Regex>, Box<Regex>),
    Group(Box<Regex>, usize),
    Any,
    Empty,
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
        Regex::Optional(..) => Err((pos, String::from("Non-greedy optional is not supported."))),
        Regex::Repetition(..) => {
            Err((pos, String::from("Non-greedy repetition is not supported.")))
        }
        r => Ok(Regex::Optional(Box::new(r))),
    }
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
    let mut stack = vec![(Regex::Empty, None)];
    let mut chrs = regex.chars().enumerate();
    for (pos, chr) in chrs {
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
            '.' => {
                let (last, remainder) = stack.pop().unwrap();
                stack.push((concat(last, Regex::Any), remainder));
            }
            c => {
                let (last, remainder) = stack.pop().unwrap();
                stack.push((concat(last, Regex::Char(c)), remainder));
            }
        }
    }
    if stack.len() != 1 {
        Err((
            regex.chars().count(),
            format!(
                "Expected {} closing parenthesis, found EOF",
                stack.len() - 1
            ),
        ))
    } else {
        Ok((stack.pop().unwrap().into(), groups))
    }
}
