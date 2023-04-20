# Compilation

Beans can be used in two ways, which are very much related but, in practice, will require entierly
different approaches.

Beans can be used as a on-the-fly parser-generator, meaning that you expect your
end user to give you a grammar for a language they just though of, and you have to parse files
written in that language. This is mainly useful for 
[domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language). An example of
this is Beans itself, which has to parse the grammars you feed it. Since this aspect of Beans is not
as mature as the other one, it's not the one this book will focus on.

The other purpose of Beans is to be used as a regular parser-generator (think [Yacc](https://
en.wikipedia.org/wiki/Yacc), [Bison](https://fr.wikipedia.org/wiki/GNU_Bison), [Menhir](http://
gallium.inria.fr/~fpottier/menhir/), ...). The main difference is that, unlike these tools, Beans will
never generate Rust code to be compiled alongside your code. Instead, it does its own compilation: it
compiles a grammar to a binary blob, which is then included in the final binary. This means that you
need to compile Beans grammars "by hand", using `beans`. `beans` is also useful for debugging 
purposes, as it can give you helpful insights or advices on your grammars.
