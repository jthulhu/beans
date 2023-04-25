# Common Concepts

When parsing with Beans, as with most other similar tools, three steps
are performed, in this order:
 * [Lexing](lexer.md)
 * [Parsing](parser.md)
 * [Syntax tree building](ast.md)
 
The first step, lexing, operates directly on plain text inputs, while
the last is in charge of producing the abstract syntax tree. For more
details on the operations that can be performed on the latter, please
refer to the [Rewriting the AST Chapter](ast/README.md).

# Simple arithmetic expression

Throughout the explanation of the core concepts of parsing, some
simple grammars will be written to allow parsing a language of simple
arithmetic expressions, consisting of numbers or binary operations
(addition, multiplication, subtraction and division) on
expressions. All the grammars will be available at
https://github.com/jthulhu/beans, in the directory
`doc/examples/arith`.
