# Compilation

Beans can be used in two ways, which are very much related but, in practice, will require entierly
different approaches.

Beans can be used as a on-the-fly parser-generator, meaning that you expect your
end user to give you a grammar for a language they just though of, and you have to parse files
written in that language. This is mainly useful for 
[domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language). An example of
this is Beans itself, which has to parse the grammars you feed it. Since this aspect of Beans is not
as mature as the other one, it's not the one this book will focus on.

The other purpose of Beans is to be used as a regular parser-generator (think 
[Yacc](https://en.wikipedia.org/wiki/Yacc), [Bison](https://fr.wikipedia.org/wiki/GNU_Bison),
[Menhir](http://gallium.inria.fr/~fpottier/menhir/), ...). The main difference is that, unlike these tools, Beans will
never generate Rust code to be compiled alongside your code. Instead, it does its own compilation: it
compiles a grammar to a binary blob, which is then included in the final binary. This means that you
need to compile Beans grammars "by hand", using `beans`. `beans` is also useful for debugging 
purposes, as it can give you helpful insights or advices on your grammars.

# The grammars

Beans contains two kind of grammars: the lexer grammars (extension `.lx`), and the parser grammars (extension `.gr`).
They are written in different languages, and are compiled separatly, although the parser grammar relies on the lexer
grammar, because the terminals defined in the lexer grammar are used in the parser grammar.

## Lexer grammars

The lexer grammar can be compiled with `beans compile lexer path/to/grammar.lx`. It will produce a binary blob at
`path/to/grammar.clx`.

## Parser grammars

The parser grammar can be compiled with `beans compile parser --lexer path/to/grammar.clx path/to/grammar.gr`. It will
produce a binary blob at `path/to/grammar.cgr`. Note that we had to provide a lexer grammar (so that Beans can find
the definitions of the terminals used in the parser grammar), and in this case it was a *compiled* lexer grammar.
A non-compiled lexer grammar will also be accepted, but the process will be slower because Beans has to interpret it.
