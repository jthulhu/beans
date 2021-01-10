use super::matching::{Instruction, Program};

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_read() {
        use Regex::*;
        assert_eq!(
            read("a+b+").unwrap(),
            (
		Concat(
                    Box::new(Concat(
			Box::new(Empty),
			Box::new(Repetition(Box::new(Char('a')),))
                    )),
                    Box::new(Repetition(Box::new(Char('b'))))
		),
		0
	    )
        )
    }

    #[test]
    fn test_build() {
        use Instruction::*;
        let program = compile("a+b+").unwrap();
        assert_eq!(
            program,
            (
		vec![Char('a'), Split(0, 2), Char('b'), Split(2, 4), Match],
		0
	    )
        );
    }
}

#[derive(PartialEq, Eq, Debug)]
enum Regex {
    Char(char),
    Option(Box<Regex>, Box<Regex>),
    Optional(Box<Regex>),
    Repetition(Box<Regex>),
    KleeneStar(Box<Regex>),
    Concat(Box<Regex>, Box<Regex>),
    Group(Box<Regex>, usize),
    Empty,
}

type RegexError = (usize, String);

impl Into<Regex> for (Regex, Option<Regex>) {
    fn into(self) -> Regex {
        match self {
            (r, Some(r2)) => Regex::Option(Box::new(r), Box::new(r2)),
            (r, None) => r,
        }
    }
}

fn build(regex: Regex, program: &mut Program) {
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
        Regex::Empty => {}
    };
}

pub fn compile(regex: &str) -> Result<(Program, usize), RegexError> {
    let mut program = Vec::new();
    let (regex, nb_groups) = read(regex)?;
    build(regex, &mut program);
    program.push(Instruction::Match);
    Ok((program, nb_groups))
}

fn read(regex: &str) -> Result<(Regex, usize), RegexError> {
    let mut stack = vec![(Regex::Empty, None)];
    let mut groups = 0;
    for (pos, chr) in regex.chars().enumerate() {
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
                    stack.push((Regex::Concat(Box::new(last), Box::new(group)), remainder));
                }
            }
            '?' => {
                let (last, remainder) = stack.pop().unwrap();
                let new = match last {
		    Regex::Empty => return Err((pos, String::from("Cannot make empty regex optional."))),
		    Regex::Concat(r1, r2) => match *r2 {
			Regex::KleeneStar(..) => return Err((pos, String::from("Non-greedy kleene star is not supported."))),
			Regex::Repetition(..) => return Err((pos, String::from("Non-greedy repetition is not supported."))),
			Regex::Optional(..) => return Err((pos, String::from("Non-greedy optional is not supported."))),
			r => Regex::Optional(Box::new(r))
		    },
		    _ => return Err((pos, String::from("Internal parsing engine error: top regex must be either Empty or Concat.")))
		};
                stack.push((new, remainder));
            }
            '*' => {
                let (last, remainder) = stack.pop().unwrap();
                let new = match last {
		    Regex::Empty => return Err((pos, String::from("Cannot apply kleene star to empty regex."))),
		    Regex::Concat(r1, r2) => match *r2 {
			Regex::Empty => return Err((pos, String::from("Cannot apply kleene star to empty regex (this should not happen)"))),
			Regex::KleeneStar(..) => return Err((pos, String::from("Cannot apply kleene star twice."))),
			Regex::Repetition(..) => return Err((pos, String::from("Cannot apply kleene star and repetition."))),
			Regex::Optional(..) => return Err((pos, String::from("Cannot apply kleene star on an optional group."))),
			r => Regex::Concat(r1, Box::new(Regex::KleeneStar(Box::new(r)))),
		    },
		    _ => return Err((pos, String::from("Internal parsing engine error: top regex must be either Empty or Concat.")))
		};
                stack.push((new, remainder));
            }
            '+' => {
                let (last, remainder) = stack.pop().unwrap();
                let new = match last {
		    Regex::Empty => return Err((pos, String::from("Cannot apply repetition to empty regex."))),
		    Regex::Concat(r1, r2) => match *r2 {
			Regex::Empty => return Err((pos, String::from("Cannot apply repetition to empty regex (this should not happen)."))),
			Regex::KleeneStar(..) => return Err((pos, String::from("Cannot apply repetition on kleene star"))),
			Regex::Repetition(..) => return Err((pos, String::from("Cannot apply repetition on repetition"))),
			Regex::Optional(..) => return Err((pos, String::from("Cannot apply repetition on an optional group"))),
			r => Regex::Concat(r1, Box::new(Regex::Repetition(Box::new(r))))
		    },
		    _ => return Err((pos, String::from("Internal parsing engine error: top regex must be either Empty or Concat.")))
		};
                stack.push((new, remainder));
            }
            '|' => {
                let last = stack.pop().unwrap().into();
                stack.push((Regex::Empty, Some(last)));
            }
            c => {
                let (last, remainder) = stack.pop().unwrap();
                let new = Regex::Concat(Box::new(last), Box::new(Regex::Char(c)));
                stack.push((new, remainder));
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
