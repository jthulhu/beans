# Parser

The parser is given a stream of tokens, which is a "flat"
representation of the input, in the sense that every part of it is at
the same level, and should transform it into a Concrete Syntax Tree
(CST).

> Note: a CST is a tree whose leaves are terminals, and no inner node
> is a terminal. It represents the way the input was understood. For
> instance, given the input `1+2*3`, a CST could be
> ```
>           Expression
>      ┌───────┘│└───────┐
>  INTEGER 1   ADD   Expression
>               ┌───────┘│└───────┐
>         INTEGER 2   MULTIPLY  INTEGER 3
> ```
> Inner nodes of a syntax tree are called *non terminals*.

In a Concrete Syntax Tree, every single token is remembered. This can
be annoying, as we usually want to forget tokens: if a given token
held some information, we can extract that information before dumping
the token, but having the actual token is not very useful.

After having pruned the CST from tokens (while still having extracted
the useful information), we get an Abstract Syntax Tree (AST).

> The AST for the input `1+2*3` might look light
> ```
>           Expression (+)
>      ┌───────┘ └───────┐
>      1            Expression (x)
>               ┌───────┘ └───────┐
>               2                 3
> ```
> All tokens have disappeared. From `INTEGER` tokens, we have
> extracted the actual number that was lexed, and we have remembered
> that each `Expression` corresponds to a certain operation, but the
> token corresponding to that operation has been forgotten.
 

Similarly to terminals, non terminals are defined by telling Beans how
to recognise them. Regular expressions, however, are not powerful
enough for general parsing. Therefore, non terminals use production
rules instead.

# Production rules

Production rules are at the core of the recognition and syntax-tree
building steps of every parser, but there are several (equivalent)
ways of understanding them. These different points of view produce in
turn very different parsing algorithms.

## Production rules as recognisers (bottom-up)

A production rule is of the form `A -> A_1 ... A_n`, and means that
the non terminal `A` can be recognised if `A_1` through `A_n` where
recognised before.

For instance, for our simple arithmetic expression language, we could
define a single non terminal `Expression` with the following
production rules
```
  Expression -> Expression ADD Expression
  Expression -> Expression MULTIPLY Expression
  Expression -> Expression SUBRTACT Expression
  Expression -> Expression DIVIDE Expression
  Expression -> INTEGER
```

This matches the definition of an expression we gave <span style=color:red>earlier [Dove?]</span>
> [expressions are] numbers or binary operations (addition,
> multiplication, subtraction and division) on expressions.

On the input `1+2*3`, which has been lexed to `INTEGER ADD INTEGER
MULTIPLY INTEGER` (note that, at this step, we don't care about
information that tokens hold, such as the actual value of an integer;
these don't come into play when doing a syntactic analysis), a parser
could analyze it in the following way.

1. Every `INTEGER` token is a valid `Expression`, so we can replace
   them by the `Expression` non terminal.  We get `Expression ADD
   Expression MULTIPLY Expression`.
> The operation of finding a place in the input that matches the
> right-hand side of a production rule and replacing it with its non
> terminal on the left-hand size is called a *reduction*.  The place
> in the input where the reduction occurs is called a *handle*.
2. `Expression MULTIPLY Expression` is a *handle* for `Expression`, so
   we *reduce it*.  We get `Expression ADD Expression`.
3. Finally, `Expression ADD Expression` is a handle `Expression` too,
   so after reduction we are left with `Expression`.

Here, our recognition ends successfully: the input `1+2*3` is an
arithmetic expression, or at least according to our definition.

There are several things to note on this example.

> Note 1: at step 2., an `Expression` could have been recognised in
> different places in the partially-recognised input `Expression ADD
> Expression MULTIPLY Expression`. These recognition point are called
> *handles*. There is a very important difference between choosing
> `Expression ADD Expression` as the handle to perform the recognition
> of `Expression`, and choosing `Expression MULTIPLY Expression`,
> because in the first case we would end up with a tree that matches
> the parenthesing of `(1+2)*3`; in the second one we would obtain
> `1+(2*3)`. If we were to, say, evaluate these expression, we
> wouldn't get the same result.  So, for this grammar, Beans would
> have to choose between which rule to apply, and this decision is
> crucial in the result. We will see later on how to instruct Beans to
> apply the "good" rule (which, in this case, would be the one that
> leads to parsing as `1+(2*3)`, if we want to apply the usual
> operators precedence).

> Note 2: in this example, we have limited ourselves to recognise the
> input, not trying to parse it. It wouldn't be too hard to expand our
> current "algorithm" to remember which reductions have been applied,
> and in turn construct a syntax tree from that information, but we
> won't try to do this *yet*.

The order in which we have recognised the input is called "bottom-up",
because we have started with the terminals, and iteratively replaced
them with non terminals, ending up with a single non terminal (if the
recognition goes well). Since in the end we want to produce a syntax
tree, and that in a tree, the root is at the top, whereas the leaves
are at the bottom, we have effectively traversed that tree start from
the bottom all the way up. But we could have done the opposite...

## Production rules as generators (top-down)

So far, it might not be clear why production rules are called as such,
when we have happily been using them as recognition rules instead;
even the arrow seems in the wrong direction: when we apply a
reduction, we transform the right-hand side into the left-hand side of
a rule.

Now, we will see that production rules can be used instead to
*generate* valid expressions.  Starting with a single non terminal
`Expression`, we can *expand* it to `Expression ADD Expression` using
the corresponding production rule. The first `Expression` can further
be expanded to `INTEGER`, using the last rule, to get `INTEGER ADD
Expression`.  If we expand `Expression` with the multiplication rule,
we get `INTEGER ADD Expression MULTIPLY Expression`. Again, by
expanding all `Expression`s with `INTEGER`, we get `INTEGER ADD
INTEGER MULTIPLY INTEGER`. Notice that this correponds to the input
`1+2*3`, and so `1+2*3` is a valid expression!

This "algorithm" might seem a little weird at first, because we have
too many choices! In the previous one, we had only one choice, and by
taking the "wrong" option we could have ended with the wrong
parenthesing *if we decided to build a syntax tree*. Otherwise, both
options were ok.  Here, we had to choose which `Expression` to expand
at each step and, more importantly, which rule to apply for the
expansion. Note that we could easily have blocked ourselves by
expanding `Expression` to `INTEGER` right away, or we could have kept
expanding forever, only ever applying the `Expression -> Expression
ADD Expression` rule.

While this seems a lot more complicated than its bottom-up
counterpart, top-down algorithms are usually much easier to implement,
mainly because it often suffices to look at a few tokens to "guess"
what the right expansion is at any moment.

<span style=color:red>Paragrafo confuso. Non capito<span style=color:red>

Correspondingly to the bottom-up strategy, if we were to look at how
we traverse the syntax tree while building it, this strategy would
actually start by examining the root of the tree, and we would be
visiting the leaves at the end, so we would be traversing the tree
top-down.

## Production rules in Beans

Before going further, let's try to write a parser grammar for Beans to
recognise simple arithmetic expressions. Beans' syntax is a little
different from production rules, because the parser does not only
recognise, it also tries to build up a syntax tree; since we are not
(yet) interested in doing that, we will ignore some syntax quirks that
will appear. In `arith.gr`, write
```beans-gr
@Expression ::=
  Expression ADD Expression <>
  Expression MULTIPLY Expression <>
  Expression SUBTRACT Expression <>
  Expression DIVIDE Expression <>
  INTEGER <>;
```
We have defined the non terminal `Expression` with five production
rules. Each production rule ends with `<>` (you can ignore this for
now), and the whole definition ends with a semicolon.

Furthermore, `Expression` is tagged with `@`, which means it's an
*axiom non-terminal*, or, in other words, it's the non terminal we are
allowed to start from in a top-down stategy. Since we only have a
single non-terminal for now, this isn't very important (but don't
forget it, or it won't work!).
```bash
$ beans parse --lexer arith.lx --parser arith.gr input
Expression
$
```
Yay! It works. Well, the output isn't very impressive, because Beans
prints the syntax tree we have produced, but we currently have no
rules that manipulate the syntax tree, and in particular we don't add
any node or leaves to it.

You can also try it on wrong inputs, for example `1+2*` or `1+2*3 4`
to check it fails as it should.

# Building a syntax tree

Checking if a string is a valid arithmetic expression is a bit
boring. We would like to get more information than just whether a
certain string is valid or not. Furthermore, as pointed earlier, our
grammar is currently ambiguous, meaning that the expression `1+2*3`
could be understood in two different ways, and it would be interesting
to see how Beans solves that ambiguity.

To do so, we need to expand our grammar a little bit. First of all, we
might want to bind expressions that we use to recognise further
expressions. For instance, when we have a `Expression ADD Expression`
and we recognise an `Expression` there, we would like to remember the
two sub expressions. To do so, we will add `@name` to every element of
a rule that we would like to remember under the name `name`.
```beans-gr
@Expression ::=
  Expression@left ADD Expression@right <>
  Expression@left MULTIPLY Expression@right <>
  Expression@left SUBTRACT Expression@right <>
  Expression@left DIVIDE Expression@right<>
  INTEGER@value <>;
```

As said in the introduction to this chapter, the goal is also to
extract information from tokens, and then dump these. The only token
that holds some information is `INTEGER`, which has a single group
(labeled `0`). We can therefore bind that group, instead of the whole
token, by accessing it with a field-like syntax.
```beans-gr
@Expression ::=
  ...
  INTEGER.0@value <>;
```

Finally, we need to remember what kind of expression each expression
is. This is very similar to naming variants of enumerations: here,
each rule bound to `Expression` is a constructor of `Expression`, and
when we will match on `Expression`, we will need to distinguish how
that particular instance of `Expression` was constructed.
```beans-gr
@Expression ::=
  Expression@left ADD Expression@right <Add>
  Expression@left MULTIPLY Expression@right <Mult>
  Expression@left SUBTRACT Expression@right <Sub>
  Expression@left DIVIDE Expression@right <Div>
  INTEGER.0@value <Literal>;
```

<span style=color:red>NB: Il mio albero è stampato in ordine inverso: prima i left e poi i right</span>

Let's see what the tree looks like now.
```bash
$ beans parse --lexer arith.lx --parser arith.gr input
Expression(Mult)
├─ right: Expression(Literal)
│  └─ value: 3
└─ left: Expression(Add)
   ├─ left: Expression(Literal)
   │  └─ value: 1
   └─ right: Expression(Literal)
      └─ value: 2
$
```
Well, it works, but... if you stare at that syntax tree long enough,
you'll realize that it was parsed like `(1+2)*3`, not like
`1+(2*3)`. We will see in the next section how to solve this issue,
and how ambiguity is handled in general.

# Ambiguity

A grammar is said to be *ambiguous* when there exists an input that
can be parsed in two different ways, that is, there are two
*derivation tree* for that input. Most of the time, an ambiguity in
the grammar is symptomatic of a semantic ambiguity, that is, the
language that we are trying to parse is somehow ill defined.

This is the case, for instance, of our simple arithmetic
expressions. Our plain-text, intuitive definition of what is an
arithmetic expression is *bad* because it doesn't say which of
`(1+2)*3` or `1+(2*3)` should be understood when reading `1+2*3`, that
is, it contains no operator priority information. But it also lacks
something else, as we will see.

> Note: one might wonder why Beans did not report this. After all, if
> it's often an actual mistake to define an ambiguous grammar, it
> would make sense for Beans to at least warn you about that. In fact,
> there is some work being done in that direction, but there is a
> fundamental issue: ambiguity is undecidable, that is, there *can't
> exist* an algorithm which, given a grammar, tells us whether it's
> ambiguous or not.  Actually, Beans will perform much better if the
> grammar in unambiguous, and even better if it belongs to a more
> restrictive class of grammars called `LR(k)`. If you have ever used
> tools like Bison, Menhir or Yacc, and you are trying to port
> grammars from them to Beans, good news!  These tools force your
> grammars to be in that restricted class (for the sake of
> performance), and so will also lead to fast parsing with Beans.

## Priority

The first issue is operator priority. Beans has a very simple rule to
determine priority: rules that come first have higher priority. So,
simply moving the division and multiplication rules up will patch our
example:
```beans-gr
@Expression ::=
  Expression@left MULTIPLY Expression@right <Mult>
  Expression@left DIVIDE Expression@right <Div>
  Expression@left ADD Expression@right <Add>
  Expression@left SUBTRACT Expression@right <Sub>
  INTEGER.0@value <Literal>;
```
```bash
$ beans parse --lexer arith.lx --parser arith.gr input
Expression(Add)
├─ left: Expression(Literal)
│  └─ value: 1
└─ right: Expression(Mult)
   ├─ left: Expression(Literal)
   │  └─ value: 2
   └─ right: Expression(Literal)
      └─ value: 3
```
Much better! However, we now have a more subtle issue. Usually,
multiplication and division have the same priority, and the leftmost
operator is chosen (same for addition and subtraction).  However, as
is, multiplication will be prioritized over division: `1/2*3` should
be parsed `(1/2)*3` but will be parsed as `1/(2*3)`. To solve this, we
need to merge the multiplication and division rules, by introducing
other non terminals.

```beans-gr
@Expression ::=
  Expression@left MultDiv@op Expression@right <MultDiv>
  Expression@left AddSub@op Expression@right <AddSub>
  INTEGER.0@value <Literal>;
  
MultDiv ::=
  MULTIPLY <Mult>
  DIVIDE <Div>;
  
AddSub ::=
  ADD <Add>
  SUBTRACT <Sub>;
```

Indeed, if we try both on inputs `1/2*3` and `1*2/3`, we have
```bash
$ beans parse --lexer arith.lx --parser arith.gr input-prior-1
Expression(MultDiv)
├─ op: MultDiv(Mult)
├─ left: Expression(MultDiv)
│  ├─ op: MultDiv(Div)
│  ├─ left: Expression(Literal)
│  │  └─ value: 1
│  └─ right: Expression(Literal)
│     └─ value: 2
└─ right: Expression(Literal)
   └─ value: 3
$ beans parse --lexer arith.lx --parser arith.gr input-prior-2
Expression(MultDiv)
├─ right: Expression(Literal)
│  └─ value: 3
├─ left: Expression(MultDiv)
│  ├─ op: MultDiv(Mult)
│  ├─ left: Expression(Literal)
│  │  └─ value: 1
│  └─ right: Expression(Literal)
│     └─ value: 2
└─ op: MultDiv(Div)
$
```
They correspond, respectively, to `(1/2)*3` and `(1*2)/3`. Victory!

> Note that makes the information a little more nested, which is fine
> for now, but will make some pretty ugly pattern matching in the
> future. In fact, this technique produces some artifacts of

<span style=color:red>of?..</span>

<span style=color:red>E il prossimo esempio che è?</span>

```beans-gr
@Expression ::=
  Expression@left MultDiv.variant@op Expression@right <Op>
  Expression@left AddSub.variant@op Expression@right <Op>
  INTEGER.0@value <Literal>;
  
MultDiv ::=
  MULTIPLY <Mult>
  DIVIDE <Div

## Associativity
