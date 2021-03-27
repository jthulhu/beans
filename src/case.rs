#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn case_recogniser() {
	use Case::*;
	assert_eq!(Case::case("snakecase"), SnakeCase);
	assert_eq!(Case::case("snake_case"), SnakeCase);
	assert_eq!(Case::case("PascalCase"), PascalCase);
	assert_eq!(Case::case("camelCase"), CamelCase);
	assert_eq!(Case::case("Pascal_Snake_Case"), PascalSnakeCase);
	assert_eq!(Case::case("UPPERCASE"), UpperCase);
	assert_eq!(Case::case("SCREAMING_SNAKE_CASE"), ScreamingSnakeCase);
	assert_eq!(Case::case(""), Other);
	assert_eq!(Case::case("__"), Other);
	assert_eq!(Case::case("fake snake case"), Other);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Case {
    // snakecase, snake_case
    SnakeCase,
    // PascalCase
    PascalCase,
    // camelCase,
    CamelCase,
    // Pascal_Snake_Case,
    PascalSnakeCase,
    // UPPERCASE
    UpperCase,
    // SCREAMING_SNAKE_CASE,
    ScreamingSnakeCase,
    Other
}

impl Case {
    pub fn case(string: &str) -> Self {
	if string.len() == 0 {
	    return Self::Other;
	}

	let mut contains_underscore = false;
	let mut first_is_capital = false;
	let mut contains_small = false;
	let mut contains_capital = false;

	let mut contains_non_alphanumeric = false;
	
	let mut first_alpha = true;
	
	for c in string.chars() {
	    if c == '_' {
		contains_underscore = true;
	    } else if c.is_ascii_uppercase() {
		contains_capital = true;
		if first_alpha {
		    first_is_capital = true;
		}
		first_alpha = false;
	    } else if c.is_ascii_lowercase() {
		contains_small = true;
		first_alpha = false;
	    } else {
		contains_non_alphanumeric = true;
	    }
	}

	if first_alpha || contains_non_alphanumeric {
	    Self::Other
	} else if !contains_capital {
	    Self::SnakeCase
	} else if !first_is_capital {
	    Self::CamelCase
	} else if contains_small {
	    if contains_underscore {
		Self::PascalSnakeCase
	    } else {
		Self::PascalCase
	    }
	} else {
	    if contains_underscore {
		Self::ScreamingSnakeCase
	    } else {
		Self::UpperCase
	    }
	}
    }
}
