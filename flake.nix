{
  description = "Beans";
  
  inputs = {
    naersk.url = "github:nmattia/naersk/master";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = github:oxalica/rust-overlay;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "utils";
      };
    };
  };

  outputs = { self, nixpkgs, utils, naersk, rust-overlay }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
          ];
        };
        rust = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "clippy" ];
        };
        naersk-lib = naersk.lib.${system}.override {
          cargo = rust;
          rustc = rust;
        };
      in {
        defaultPackage = naersk-lib.buildPackage {
          pname = "Beans";
          root = ./.;
        };
        defaultApp = utils.lib.mkApp {
            drv = self.defaultPackage.${system};
        };
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            gdb
            rust
            cargo
            cargo-edit
            rustfmt
            clippy
            rust-analyzer
          ];
        };
      });
}
