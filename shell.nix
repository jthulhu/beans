let
  rust-version = "1.56.0";
  nixpkgs = <nixpkgs>;
  mozilla-overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  pkgs = import nixpkgs {
    overlays = [ mozilla-overlay ];
  };
  rust-channel = pkgs.rustChannelOf {
    channel = rust-version;
  };
  rust = rust-channel.rust.override {
    extensions = [ "rust-src" "rustfmt-preview" "clippy-preview" ];
  };
  cargo = rust-channel.cargo;
in
pkgs.mkShell {
  name = "beans-dev";
  buildInputs = [ rust cargo ];
}
