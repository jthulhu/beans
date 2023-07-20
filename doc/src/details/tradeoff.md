# Tradeoffs

Several tradeoffs have been made while developping Beans. You can find
here some of them that I remembered to write down.
<span style=color:red>Er...</span>

# Scannerless parsing

There are some parsers, called [scannerless
parsers](https://en.wikipedia.org/wiki/Scannerless_parsing), that do
not rely on a lexer. Indeed, a parser is *more powerful* than a lexer,
meaning that anything that a lexer could do, a parser could also
do. So, in fact, one might wonder why Beans bothers having a lexer at
all. There are several reasons for this.

## Performance

The first reason for this separation is *performance*. Parsers could
do what lexers do, but because lexing is simpler than parsing, there
is more space for specific optimizations. In fact, Beans ships its own
regex library which is tailored for the lexing use case.

## Error reporting

Usually, lexing errors are very much different than syntax
errors. It's quite rare to encounter a lexing error in practice,
because it's quite hard to write invalid tokens. This means that lexing
errors should be reported differently than syntax errors, and this
would be harder (if not impossible) in scannerless parsers.

Another aspect to be taken into account is that parsers may have a
recovery mode, which triggers when encountering a syntax error. In
this special mode, the parser cannot fully understand the input but
will try to guess how to correct the input so that it can provide
better user feedback. This is much easier to perform if the parser
works on tokens, rather than characters.

## Logical separation

Parsing and lexing are two logically distinct steps, even though there
is quite some interleaving in Beans. Having them kept as different
steps makes it easier to debug a grammar one is writing, as it's easier
to see what happens step by step, where each step is simpler than the
whole parsing operation.
