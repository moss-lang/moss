{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
      in
      {
        devShells.default = pkgs.mkShellNoCC {
          buildInputs = [
            pkgs.bun
            pkgs.nixfmt
            pkgs.nodejs # Used by vsce.
            pkgs.python3
            pkgs.wasm-tools
            pkgs.wasmtime
            (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
          ];
          shellHook = ''
            PATH=$PWD/bin:$PATH
          '';
        };
      }
    );
}
