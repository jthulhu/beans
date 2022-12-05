 ```text
  _                                  
 | |__     ___    __ _   _ __    ___ 
 | '_ \   / _ \  / _` | | '_ \  / __|
 | |_) | |  __/ | (_| | | | | | \__ \
 |_.__/   \___|  \__,_| |_| |_| |___/
                                     
```

Beans is a lexing and parsing library. It allows both compiling the grammars,
and loading them at runtime, making it usable both for languages that have
fixed grammars, and languages whose syntax may vary during their compilation
phase.

Installation
============

Beans is not currently properly packaged, so unless you have [Nix](https://nixos.org),
the installation is manual.

Manual Installation
-------------------

### Dependencies

Beans is written in pure Rust and, as such, its only dependencies are (latest 
stable) Rust and a few crates. This means that, to build Beans, you only need to
have on your machine the latest stable version of the Rust compiler, as well as
cargo (and make if you don't want to do everything manually).

### Downloading Beans sources

If you have git installed, then you can simply run `git clone https://github.com/jthulhu/beans`.
Otherwise, you can download the [zip archive](https://github.com/jthulhu/beans/archive/release.zip).

### Building Beans

#### Building Beans with make

If you are using make to install Beans as well, you can skip this step, as it's a
dependency of the `install` rule.

```shell
$ make RELEASE=1 build
```

The binary can be found at `out/beans`.

#### Building Beans manually

```shell
$ cargo build --release
```

The binary can be found at `target/release/beans`

### Installing Beans

#### Installing Beans with make

```shell
$ make install
```
This will install `beans` at `/usr/local/bin/beans`. If you want to change the
installation directory, you can set the environment variables `DESTDIR` and `PREFIX`.
By default, it uses `DESTDIR=` and `PREFIX=/usr/local`.

#### Install Beans manually
```shell
$ install -D -m755 target/release/beans /usr/local/bin/beans
```
Make sure to replace `target/release/beans` with the directory where the binary
has been produced, and `/usr/local/bin/beans` where you wish `beans` to be installed.

### Uninstalling Beans
No matter the installation method used, Beans is a single, self-contained binary, that
will not create any configuration file whatsoever. Just remove the binary where you 
installed it.

Nix Flake installation
----------------------

Beans is provided as `github:jthulhu/beans#${system}.defaultPackage` as a package,
and as `github:jthulhu/beans#${system}.defaultApp` as an application, where `${system}`
can be `aarch64-darwin`, `aarch64-linux`, `x86_64-darwin` and `x86_64-linux`.

Usage
=====

Beans can be used as a library, and as an application. The application is used if
you want to compile lexer and parser grammars. The application can also do lexing
and parsing, which might be helpful for debugging.

Compilation
-----------

The first step to use Beans is to write a lexer grammar and a parser grammar, and
to compile them, in this order. This is important because the parser grammar
depends on the definition of terminals, which can be found in the lexer grammar.
To do so, assuming you have two files names `lexer.lx` and `parser.gr`, run
```shell
$ beans compile lexer lexer.lx
# Will produce a file `lexer.clx`
$ beans compile parser --lexer lexer.clx parser.gr
# Will produce a file `lexer.cgr`
$ 
```
These two files can now be used within Rust code, as follows
```rust
use beans::include_parser;

let (lexer, parser) = include_parser!(
    lexer => compiled "path/to/lexer.clx",
	parser => compiled "path/to/parser.cgr",
).unwrap();
```

This will ship in the final binary the two blobs. Refer to [the library 
documentation](https://docs.rs/beans/latest/beans) for more details on how to use
`lexer` and `parser` to parse input.

Note that the compilation step is, in fact, *optional*. It is possible to use
non-compiled grammars. This is useful when you want the user to be able to modify
the grammar during the compilation of a program. **Currently, this feature may be
broken.**

Lexing
------

Beans can produce a stream of tokens in stdout, given the appropriate grammar.
For instance, on a the file `input.c` shown
```c
void f() {
  int x;
  int y;
  y = x = 0;
}
```
and a lexer grammar corresponding to the C programming language, the lexing would
show the following result:
```shell
$ beans lex --lexer c.clx input.c
VOID { }
IDENT { 0: f, }
LPAR { }
RPAR { }
LBRACE { }
INTTY { }
IDENT { 0: x, }
SEMICOLON { }
INTTY { }
IDENT { 0: y, }
SEMICOLON { }
IDENT { 0: y, }
EQUAL { }
IDENT { 0: x, }
EQUAL { }
INT { 0: 0, }
SEMICOLON { }
RBRACE { }
```

Since the output is currently quite ugly, it will most likely be changed in the
foreseeable future.

Parsing
-------

Beans can produce an AST in stdout, given the appropriate grammars.
For instance, on the same `input.c` shown before, the result would be
```shell
$ beans parse --lexer c.clx --parser c.cgr input.c
AST
└─ decls
   └─ value
      ├─ value
      │  ├─ variant
      │  │  └─ Nil
      │  └─ head
      │     ├─ name
      │     │  └─ f
      │     ├─ block
      │     │  └─ stmts
      │     │     └─ value
      │     │        ├─ value
      │     │        │  ├─ variant
      │     │        │  │  └─ Cons
      │     │        │  ├─ head
      │     │        │  │  ├─ declaration
      │     │        │  │  │  ├─ type
      │     │        │  │  │  │  └─ variant
      │     │        │  │  │  │     └─ Int
      │     │        │  │  │  ├─ value
      │     │        │  │  │  │  └─ variant
      │     │        │  │  │  │     └─ None
      │     │        │  │  │  └─ name
      │     │        │  │  │     └─ x
      │     │        │  │  └─ variant
      │     │        │  │     └─ Declaration
      │     │        │  └─ tail
      │     │        │     ├─ variant
      │     │        │     │  └─ Cons
      │     │        │     ├─ head
      │     │        │     │  ├─ variant
      │     │        │     │  │  └─ Declaration
      │     │        │     │  └─ declaration
      │     │        │     │     ├─ name
      │     │        │     │     │  └─ y
      │     │        │     │     ├─ value
      │     │        │     │     │  └─ variant
      │     │        │     │     │     └─ None
      │     │        │     │     └─ type
      │     │        │     │        └─ variant
      │     │        │     │           └─ Int
      │     │        │     └─ tail
      │     │        │        ├─ head
      │     │        │        │  ├─ stmt
      │     │        │        │  │  ├─ stmt
      │     │        │        │  │  │  ├─ key
      │     │        │        │  │  │  │  ├─ value
      │     │        │        │  │  │  │  │  └─ y
      │     │        │        │  │  │  │  └─ variant
      │     │        │        │  │  │  │     └─ Ident
      │     │        │        │  │  │  ├─ value
      │     │        │        │  │  │  │  ├─ value
      │     │        │        │  │  │  │  │  ├─ variant
      │     │        │        │  │  │  │  │  │  └─ Int
      │     │        │        │  │  │  │  │  └─ value
      │     │        │        │  │  │  │  │     ├─ variant
      │     │        │        │  │  │  │  │     │  └─ Int
      │     │        │        │  │  │  │  │     └─ value
      │     │        │        │  │  │  │  │        └─ 0
      │     │        │        │  │  │  │  ├─ key
      │     │        │        │  │  │  │  │  ├─ variant
      │     │        │        │  │  │  │  │  │  └─ Ident
      │     │        │        │  │  │  │  │  └─ value
      │     │        │        │  │  │  │  │     └─ x
      │     │        │        │  │  │  │  └─ variant
      │     │        │        │  │  │  │     └─ Assign
      │     │        │        │  │  │  └─ variant
      │     │        │        │  │  │     └─ Assign
      │     │        │        │  │  └─ variant
      │     │        │        │  │     └─ Regular
      │     │        │        │  └─ variant
      │     │        │        │     └─ Statement
      │     │        │        └─ variant
      │     │        │           └─ Nil
      │     │        └─ variant
      │     │           └─ Some
      │     ├─ rettype
      │     │  └─ variant
      │     │     └─ Void
      │     └─ args
      │        └─ value
      │           └─ variant
      │              └─ None
      └─ variant
         └─ Some
$
```

The result is very verbose, so this will likely change in the foreseeable future.
