{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };
  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
    in
    {
      packages = forAll (pkgs: rec {
        # Just the compiler and the standard library it resolves relative to
        # its own location (see PRELUDE in bootstrap/mossc/__main__.py).
        source = pkgs.runCommand "moss-source" { } ''
          mkdir $out
          cp -r ${./bootstrap} $out/bootstrap
          cp -r ${./lib} $out/lib
        '';
        default = pkgs.writeShellApplication {
          name = "moss";
          runtimeInputs = [ pkgs.python3 ];
          text = ''
            cmd=run
            case "''${1-}" in
              lex | parse | run | build)
                cmd=$1
                shift
                ;;
            esac
            PYTHONPATH=${source}/bootstrap exec python3 -m mossc "$cmd" "$@"
          '';
        };
      });
      checks = forAll (pkgs: {
        # The Core Moss calculus paper (docs/design/core/core-moss.tex).
        core-calculus-pdf =
          pkgs.runCommand "core-moss-pdf"
            {
              nativeBuildInputs = [
                (pkgs.texliveMedium.withPackages (ps: [ ps.mathpartir ]))
              ];
            }
            ''
              export HOME=$TMPDIR
              cp ${./docs/design/core/core-moss.tex} core-moss.tex
              pdflatex -interaction=nonstopmode core-moss.tex
              pdflatex -interaction=nonstopmode core-moss.tex # cross-references
              mkdir $out
              cp core-moss.pdf $out/
            '';
        bootstrap =
          pkgs.runCommand "moss-bootstrap-test"
            {
              nativeBuildInputs = [
                pkgs.binaryen # `wasm-opt`, which the self-hosting tests run.
                pkgs.python3
                pkgs.wasmtime
              ];
            }
            ''
              export HOME=$TMPDIR # wasmtime wants a writable cache directory
              cd ${./.}/bootstrap
              python3 -m unittest
              touch $out
            '';
      });
      devShells = forAll (pkgs: {
        default = pkgs.mkShellNoCC {
          buildInputs = [
            pkgs.binaryen
            pkgs.bun # For the VS Code extension.
            pkgs.nodejs
            pkgs.python3
            pkgs.wasm-tools
            pkgs.wasmtime # The bootstrap's Wasm backend tests run it.
          ];
          shellHook = ''
            PATH=$PWD/bin:$PATH
          '';
        };
      });
      overlays.default = final: prev: {
        moss = self.packages.${prev.stdenv.hostPlatform.system}.default;
      };
    };
}
