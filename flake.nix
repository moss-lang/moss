{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAll =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          f (
            import nixpkgs {
              inherit system;
              overlays = [ (import rust-overlay) ];
            }
          )
        );
    in
    {
      packages = forAll (
        pkgs:
        let
          source = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./Cargo.toml
              ./Cargo.lock
              ./crates
            ];
          };
          compilerSource = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./bootstrap/mossc
              ./lib
              ./src
            ];
          };
          installData = compiler: ''
            mkdir -p $out/share/moss
            cp ${compiler} $out/share/moss/mossc.wasm
            cp -r ${./lib} $out/share/moss/lib
          '';
          staticBinaryen = pkgs.pkgsStatic.binaryen.overrideAttrs (old: {
            doCheck = false;
            nativeBuildInputs = [
              pkgs.cmake
              pkgs.python3
            ];
            nativeCheckInputs = [ ];
            checkInputs = [ ];
            cmakeFlags = (old.cmakeFlags or [ ]) ++ [
              "-DBUILD_STATIC_LIB=ON"
              "-DBUILD_SHARED_LIB=OFF"
              "-DBUILD_SHARED_LIBS=OFF"
            ];
          });
        in
        rec {
          # B(S), followed by S0(S): the compiler believed to be at the
          # self-hosting fixpoint. The test suite turns the crank once more.
          compiler =
            pkgs.runCommand "mossc.wasm"
              {
                nativeBuildInputs = [
                  pkgs.binaryen
                  pkgs.python3
                  pkgs.wasmtime
                ];
              }
              ''
                export HOME=$TMPDIR
                export PYTHONPATH=${compilerSource}/bootstrap
                export MOSS_LIB=${compilerSource}/lib
                python3 -m mossc ${compilerSource}/src/main.moss > s0.wasm
                wasm-opt -all -O3 s0.wasm -o s0-opt.wasm
                wasmtime run --argv0 lib/prelude.moss --dir ${compilerSource}::. \
                  s0-opt.wasm src/main.moss > s1.wasm
                wasm-opt -all -O3 s1.wasm -o $out
              '';
          default = pkgs.rustPlatform.buildRustPackage {
            pname = "moss";
            version = "0.0.0";
            src = source;
            cargoLock.lockFile = ./Cargo.lock;
            buildInputs = [ pkgs.binaryen ];
            BINARYEN_LIB_DIR = "${pkgs.binaryen}/lib";
            postInstall = installData compiler;
          };
          standalone =
            if pkgs.stdenv.hostPlatform.isLinux then
              pkgs.pkgsStatic.rustPlatform.buildRustPackage {
                pname = "moss-standalone";
                version = "0.0.0";
                src = source;
                cargoLock.lockFile = ./Cargo.lock;
                buildInputs = [ staticBinaryen ];
                BINARYEN_LIB_DIR = "${staticBinaryen}/lib";
                BINARYEN_STATIC = "1";
                BINARYEN_STATIC_STDCPP = "1";
                postInstall = installData compiler;
                postFixup = ''
                  ${pkgs.removeReferencesTo}/bin/remove-references-to \
                    -t ${staticBinaryen} $out/bin/moss
                  rm -f $out/nix-support/propagated-build-inputs
                '';
              }
            else
              default;
          vscode = pkgs.vscode-utils.buildVscodeExtension {
            pname = "moss-vscode";
            version = "0.0.0";
            src = pkgs.lib.fileset.toSource {
              root = ./vscode;
              fileset = pkgs.lib.fileset.unions [
                ./vscode/language-configuration.json
                ./vscode/LICENSE
                ./vscode/package.json
                ./vscode/syntaxes
              ];
            };
            sourceRoot = "source";
            vscodeExtPublisher = "moss-lang";
            vscodeExtName = "moss-vscode";
            vscodeExtUniqueId = "moss-lang.moss-vscode";
          };
        }
      );
      checks = forAll (
        pkgs:
        {
          cli = pkgs.runCommand "moss-cli-check" { } ''
            moss=${self.packages.${pkgs.stdenv.hostPlatform.system}.default}/bin/moss
            source=${./.}/examples/hello.moss
            "$moss" --help >/dev/null
            "$moss" -O3 "$source" | grep -qx 'Hello, world!'
            if "$moss" -O "$source" 2>/dev/null; then
              false
            fi
            touch $out
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
          vscode = self.packages.${pkgs.stdenv.hostPlatform.system}.vscode;
        }
        // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          standalone =
            pkgs.runCommand "moss-standalone-check"
              {
                nativeBuildInputs = [ pkgs.file ];
              }
              ''
                executable=${self.packages.${pkgs.stdenv.hostPlatform.system}.standalone}/bin/moss
                file "$executable" | grep -q 'statically linked'
                ! grep -R -a -q /nix/store/ \
                  ${self.packages.${pkgs.stdenv.hostPlatform.system}.standalone}
                "$executable" --help >/dev/null
                touch $out
              '';
        }
      );
      devShells = forAll (pkgs: {
        default = pkgs.mkShellNoCC {
          buildInputs = [
            pkgs.binaryen
            pkgs.python3
            pkgs.rust-bin.stable.latest.default
            pkgs.vsce # For manually packaging the VS Code extension.
            pkgs.wasm-tools
            pkgs.wasmtime # The bootstrap's Wasm backend tests run it.
          ];
          BINARYEN_LIB_DIR = "${pkgs.binaryen}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.binaryen ];
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
