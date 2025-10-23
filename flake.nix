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
            # Necessary tools.
            pkgs.bun
            pkgs.nodejs # Used by vsce.
            pkgs.python3
            pkgs.rust-bin.stable.latest.default

            # Convenient tools.
            pkgs.binaryen
            pkgs.nixfmt
            pkgs.wasm-tools
            pkgs.wasmtime
          ];
          shellHook = ''
            PATH=$PWD/bin:$PATH
          '';
        };
      }
    );
}
