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
cargo.

### Downloading Beans sources

If you have git installed, then you can simply run `git clone https://github.com/TheBlackBeans/beans`.
Otherwise, you can download the [zip archive](https://github.com/TheBlackBeans/beans/archive/release.zip).

### Building Beans

Assuming you're at the root of the project, indipendently on how you downloaded it.
```shell
$ cargo build --release
$ cp target/release/beans /path/to/bin/
```
Make sure to replace `/path/to/bin` with the directory where you put manually 
installed packages, and make sure it's in your `$PATH`.

Usage
=====

Beans can be used as a library, and as an application. The application is used if
you want to compile lexer and parser grammars. The application can also do lexing
and parsing, which might be helpful for debugging.
