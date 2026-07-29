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
        vscode = pkgs.vscode-utils.buildVscodeExtension {
          pname = "moss-vscode";
          version = "0.0.0";
          src = pkgs.lib.fileset.toSource {
            root = ./packages/moss-vscode;
            fileset = pkgs.lib.fileset.unions [
              ./packages/moss-vscode/language-configuration.json
              ./packages/moss-vscode/LICENSE
              ./packages/moss-vscode/package.json
              ./packages/moss-vscode/syntaxes
            ];
          };
          sourceRoot = "source";
          vscodeExtPublisher = "moss-lang";
          vscodeExtName = "moss-vscode";
          vscodeExtUniqueId = "moss-lang.moss-vscode";
        };
      });
      checks = forAll (pkgs: {
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
        vscode = self.packages.${pkgs.stdenv.hostPlatform.system}.vscode;
      });
      devShells = forAll (pkgs: {
        default = pkgs.mkShellNoCC {
          buildInputs = [
            pkgs.binaryen
            pkgs.python3
            pkgs.vsce # For manually packaging the VS Code extension.
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
        vscode-extensions = prev.vscode-extensions // {
          moss-lang = (prev.vscode-extensions.moss-lang or { }) // {
            moss-vscode = self.packages.${prev.stdenv.hostPlatform.system}.vscode;
          };
        };
      };
    };
}
