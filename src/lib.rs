#![allow(clippy::upper_case_acronyms, unstable_name_collisions)]
#![warn(missing_debug_implementations)]

//! # Beans
//!
//! Language-oriented programming and scripting language.
//! This library contains an API to parse grammars, compile them and compile source code.

/// If this flag is set, Beans will use its precompiled grammars. Otherwise, it will used
/// preparsed grammars. This is never useful except specifically while doing grammar version
/// migrations, and it should all be handled by the Makefile so you should never touch this.
/// In particular, this is only useful for development, not for release.
/// For this reason, never set the `_from-ast` feature (it will be used by automated tools when
/// needed).
pub const PRECOMPILED: bool = cfg!(not(feature = "_from-ast"));

pub mod builder;
mod case;
pub mod error;
pub mod lexer;
mod list;
pub mod parser;
pub mod printer;
pub mod regex;
pub mod span;
pub mod stream;
pub mod typed;
// Macros
#[macro_use]
mod utilities;
#[cfg(test)]
#[macro_use]
mod test_utilities;
