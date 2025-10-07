{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      fenix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShellNoCC {
          buildInputs = [
            pkgs.bun
            pkgs.nixfmt
            pkgs.nodejs # Used by vsce.
            pkgs.python3
            (fenix.packages.${system}.stable.toolchain)
          ];
          shellHook = ''
            PATH=$PWD/bin:$PATH
          '';
        };
      }
    );
}
