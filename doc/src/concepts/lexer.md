# Lexer

## What does a lexer do?

A lexer performs the initial, important step of grouping together characters that couldn't be
morphologically split, while removing useless ones. For instance, in most programming languages,
spaces are only useful to split words, they do not have any intrinsic meaning. Therefore, they
should be dumped by the lexer, whereas all the characters that form an identifier or a keyword
should be grouped together to form a single *token*.

> Note: a *token*, also called a *terminal symbol* or more shortly a *terminal*, is a minimal
> span of text of the input with an identified meaning. For instance, any identifier, keyword
> or operator would be considered a token.

Both the parser and the lexer in Beans use online algorithms, meaning that they will consume 
their input as they process it. Beans' lexer will consume the input string one unicode character
at a time. The lexer might backtrack, but this is, in practice, very rare. Non-degenerate 
grammars will never trigger such backtracking.

As the lexer reads the input, it will produce tokens. Sometimes (as with whitespace), it will
discard them. Other times, it might forget what the exact characters where, it will just remember
which token has been read.

## Regular expression

Each terminal in Beans is recognized by matching its associated regular expression. Prior 
knowledge of regular expressions is assumed. Since regular expressions have loads of different
specifications, here is an exhaustive list of features allowed in Beans regular expressions,
besides the usual disjunction operator `|`, character classes `[...]` or `[^...]` and repetition
with `+`, `*` and `?`.

| Escaped character | Name           | Meaning                                                                   |
|-------------------|----------------|---------------------------------------------------------------------------|
| `\b`              | Word bounary   | matches `ϵ` if the previous or the next character are not word characters |
| `\w`              | Word character | equivalent to [a-zA-Z0-9]                                                 |
| `\t`              | Tabulation     | matches a tabulation                                                      |
| `\Z` or `\z`      | End of file    | matches `ϵ` at the end of the line                                        |
| `\d`              | Digit          | equivalent to [0-9]                                                       |
| `\n`              | Newline        | matches an end of line                                                    |
| `\s`              | Whitespace     | matches whatever unicode considers whitespace                             |
|                   |                |                                                                           |

# Simple arithmetic lexer

Let's try to write a lexer grammar for the simple arithmetic expression language. Ideally, we 
would like to parse expressions such as `1+2*3`. So let's start by defining an integer token.
In `arith.lx`, write
```beans-lx
INTEGER ::= \d+
```
Let's get through this first definition. `INTEGER` is the name of the terminal, whereas what is
on the right side of `::=` is the regular expression used to match it.

> Note: spaces between `::=` and the start of the regular expression are ignored, but every other
> space will be taken into account, including trailing ones, which are easy to overlook. If the
> regular expression starts with a space, you can always wrap it in a singleton class `[ ]`.

> Note: terminals are always SCREAMING CASED. While this is not very readable nor practical to
> type, it is coherent with the literature, and will allow you to distinguish between variables
> (which will be snake_cased), non terminals (which will be Pascal Cased) and terminals later on.

We can also add the terminals for the four other operators
```beans-lx
ADD ::= \+
MULTIPLY ::= \*
SUBTRACT ::= -
DIVIDE ::= /
```
If we were to try to lex a file `input` containing the expression `1+2*3`, we would get
```bash
$ beans lex --lexer arith.lx input
INTEGER
ADD
INTEGER
MULTIPLY
INTEGER
Error: Could not lex anything in file input, at character 5 of line 1.
$
```
This is bad for two reasons. The first is, of course, that we get an error. This is because our 
file ended with a newline `\n`, and that there is no terminal that matches it. In fact, we would
also have a problem if we tried to lex `1 + 2*3`, because no terminal can read spaces. However,
we also *don't* want to produce any token related to such spaces: `1+2*3` and `1 + 2*3` should
be lexed indentically. Thus we will introduce a `SPACE` token with the `ignore` flag, telling
the lexer not to output it. Similarly for `NEWLINE`.
```beanx-lx
ignore SPACE ::= \s+
ignore NEWLINE ::= \n+
```
Let's try again
```bash
$ beans lex --lexer arith.lx input
INTEGER
ADD
INTEGER
MULTIPLY
INTEGER
$
```
Nice!

However, we now face the second issue: it was probably wise to forget the specific character that
was lexed to `ADD` or `MULTIPLY`, because we don't care; but we don't want to forget the actual
integer we lexed. To correct this, we will use regex groups. In `arith.lx`, we will replace the
definition of `INTEGER` with
```beans-lx
INTEGER ::= (\d+)
```
This will create a group that will contain everything that `\d+` will match, and this information
will be passed with the created token.
```bash
$ beans lex --lexer arith.lx input
INTEGER {0: 1}
ADD
INTEGER {0: 2}
MULTIPLY
INTEGER {0: 3}
$
```
We will see in the next section how to manipulate a stream of tokens.

