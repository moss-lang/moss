{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
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
      crane,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
        craneLib = crane.mkLib pkgs;
        commonArgs = {
          pname = "connie";
          src = ./.;
          strictDeps = true;
        };
      in
      {
        packages.default = craneLib.buildPackage (
          commonArgs
          // {
            cargoArtifacts = craneLib.buildDepsOnly commonArgs;
            cargoExtraArgs = "--locked --package=connie-cli";
          }
        );
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
