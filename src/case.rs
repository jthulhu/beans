//! # Case
//!
//! This module contains primitives about case.
//! It provides an interface to compute the case of a given string.

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn case_recogniser() {
        use Case::*;
        assert_eq!(Case::compute("snakecase"), SnakeCase);
        assert_eq!(Case::compute("snake_case"), SnakeCase);
        assert_eq!(Case::compute("PascalCase"), PascalCase);
        assert_eq!(Case::compute("camelCase"), CamelCase);
        assert_eq!(Case::compute("Pascal_Snake_Case"), PascalSnakeCase);
        assert_eq!(Case::compute("UPPERCASE"), UpperCase);
        assert_eq!(Case::compute("SCREAMING_SNAKE_CASE"), ScreamingSnakeCase);
        assert_eq!(Case::compute(""), Other);
        assert_eq!(Case::compute("__"), Other);
        assert_eq!(Case::compute("fake snake case"), Other);
    }
}

/// # Summary
///
/// Indicate which type of case a certain string is written in.
#[derive(Debug, PartialEq, Eq)]
#[allow(clippy::enum_variant_names)]
pub enum Case {
    /// snakecase, snake_case
    SnakeCase,
    /// PascalCase
    PascalCase,
    /// camelCase,
    CamelCase,
    /// Pascal_Snake_Case,
    PascalSnakeCase,
    /// UPPERCASE
    UpperCase,
    /// SCREAMING_SNAKE_CASE,
    ScreamingSnakeCase,
    /// Not a known case
    Other,
}

impl Case {
    pub fn compute(string: &str) -> Self {
        if string.is_empty() {
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
        } else if contains_underscore {
            Self::ScreamingSnakeCase
        } else {
            Self::UpperCase
        }
    }
}

pub trait CaseProcess {
    fn case(&self) -> Case;
}

impl CaseProcess for &str {
    fn case(&self) -> Case {
        Case::compute(self)
    }
}

impl CaseProcess for String {
    fn case(&self) -> Case {
        Case::compute(self.as_str())
    }
}
