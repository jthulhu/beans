//! # Lexer
//!
//! This module contains structs and primitives related to tokenization, or lexing. The most useful are:
//!  - the [`Lexer`], which is the base type for lexing. It essentially contains information about the lexing grammar
//!    and the methods required to lex
//!  - [`LexerBuilder`] is, as the name explains, the builder struct for [`Lexer`]. It is highly recommended to construct
//!    [`Lexer`] through its builder.
//!  - [`Token`] is the basic interface to deal with the result of the tokenization. The goal of a lexer is to turn a stream
//!    characters into a stream of tokens.
//!  - [`LexedStream`] is the interface for generating a stream of [`Token`] from a [`Lexer`] and a [`StringStream`].
//!
//!  [`StringStream`]: crate::stream::StringStream

mod ast;
mod grammar;
#[allow(clippy::module_inception)]
mod lexer;

pub use grammar::{Grammar, Ignores};
pub use lexer::{LexedStream, Lexer, TerminalId, Token};
