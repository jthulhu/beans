# Installation

Using Beans as a library in Rust is done as with any other library:
adding it in the dependencies in the Cargo manifest is
enough. However, as explained in the [Compiling Section](compile.md),
a command line tool is also required to use Beans. It allows
compilation of Beans grammars, some introspection and debugging
information.  There are ways to install `beans`:
 * Installing with [Nix](https://nixos.org). This is the preferred way
   if you already have nix.
 * Installing with Cargo. This is the preferred way is you don't have
   nix.
 * Installing manually.
 
# Nix installation

Beans is flake-packaged for nix. You can find the appropriate flake at
[Beans' repo](https://github.com/jthulhu/beans). The actual
installation procedure depends on how you use nix.

# Cargo installation

Beans can be installed with

```bash
cargo install beans
```

# Manual compilation and installation

Beans has three dependencies: Cargo, the latest rustc compiler and
make. Optionally, having git makes it easy to download the source
code.

## Downloading the source code

If you have git, you can `git clone https://github.com/jthulhu/beans`
which will download a copy of the source code in the directory
`beans`.

Otherwise, you need to download it from the [git
repository](https://github.com/jthulhu/beans).

## Building and installing the source code

Once the `beans` directory entered, you simply need to run
```bash
make install
```
This will install a single binary at `/usr/local/bin/beans`. You can
overwrite the target destination using the environment variables
`DESTDIR` and `PREFIX`.
