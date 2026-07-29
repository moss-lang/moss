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
          installData = ''
            mkdir -p $out/share/moss
            cp -r ${./lib} $out/share/moss/lib
          '';
          staticBinaryenFor = binaryen: binaryen.overrideAttrs (old: {
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
          staticBinaryen = staticBinaryenFor pkgs.pkgsStatic.binaryen;
          # B(S), followed by S0(S): the compiler believed to be at the
          # self-hosting fixpoint. The test suite turns the crank once more.
          portableCompiler =
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
          precompiler = pkgs.rustPlatform.buildRustPackage {
            pname = "moss-precompiler";
            version = "0.0.0";
            src = source;
            cargoLock.lockFile = ./Cargo.lock;
            cargoBuildFlags = [
              "--package"
              "moss-precompiler"
            ];
            cargoTestFlags = [
              "--package"
              "moss-precompiler"
            ];
          };
          compilerFor =
            target:
            pkgs.runCommand "moss-${target}.cwasm"
              {
                nativeBuildInputs = [ precompiler ];
              }
              ''
                moss-precompiler ${target} ${portableCompiler} $out
              '';
          packageFor =
            {
              rustPlatform,
              binaryen,
              target,
              pname ? "moss",
              extra ? { },
            }:
            rustPlatform.buildRustPackage (
              {
                inherit pname;
                version = "0.0.0";
                src = source;
                cargoLock.lockFile = ./Cargo.lock;
                cargoBuildFlags = [
                  "--package"
                  "moss-cli"
                  "--bin"
                  "moss"
                ];
                buildInputs = [ binaryen ];
                BINARYEN_LIB_DIR = "${binaryen}/lib";
                MOSS_COMPILER_CWASM = compilerFor target;
                postInstall = installData;
              }
              // extra
            );
        in
        (rec {
          compiler = portableCompiler;
          # The Core Moss calculus paper (docs/design/core/core-moss.tex).
          pdf =
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
          default = packageFor {
            inherit (pkgs) rustPlatform;
            binaryen = pkgs.binaryen;
            target = pkgs.stdenv.hostPlatform.rust.rustcTarget;
          };
          standalone =
            if pkgs.stdenv.hostPlatform.isLinux then
              packageFor {
                rustPlatform = pkgs.pkgsStatic.rustPlatform;
                binaryen = staticBinaryen;
                target = pkgs.pkgsStatic.stdenv.hostPlatform.rust.rustcTarget;
                pname = "moss-standalone";
                extra = {
                  BINARYEN_STATIC = "1";
                  BINARYEN_STATIC_STDCPP = "1";
                  RUSTFLAGS = pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isx86_64 (
                    "-C relocation-model=static -C link-arg=-no-pie"
                  );
                  postFixup = ''
                    ${pkgs.removeReferencesTo}/bin/remove-references-to \
                      -t ${staticBinaryen} $out/bin/moss
                    rm -rf $out/nix-support
                  '';
                };
              }
            else
              let
                binaryen = staticBinaryenFor pkgs.binaryen;
              in
              packageFor {
                inherit (pkgs) rustPlatform;
                inherit binaryen;
                target = pkgs.stdenv.hostPlatform.rust.rustcTarget;
                pname = "moss-standalone";
                extra = {
                  BINARYEN_STATIC = "1";
                  postFixup = ''
                    ${pkgs.darwin.cctools}/bin/install_name_tool \
                      -change ${pkgs.libiconv}/lib/libiconv.2.dylib \
                      /usr/lib/libiconv.2.dylib $out/bin/moss
                    ${pkgs.removeReferencesTo}/bin/remove-references-to \
                      -t ${binaryen} $out/bin/moss
                    rm -rf $out/nix-support
                  '';
                };
              };
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
        })
        // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
          windows =
            let
              crossPkgs = import nixpkgs {
                localSystem = pkgs.stdenv.hostPlatform.system;
                crossSystem.config = "x86_64-w64-mingw32";
                overlays = [ (import rust-overlay) ];
              };
              binaryen = staticBinaryenFor crossPkgs.binaryen;
              mcfgthreads =
                crossPkgs.callPackage "${nixpkgs}/pkgs/os-specific/windows/mcfgthreads" { };
            in
            packageFor {
              rustPlatform = crossPkgs.rustPlatform;
              inherit binaryen;
              target = crossPkgs.stdenv.hostPlatform.rust.rustcTarget;
              pname = "moss-windows";
              extra = {
                buildInputs = [
                  binaryen
                  mcfgthreads
                ];
                BINARYEN_STATIC = "1";
                BINARYEN_STATIC_STDCPP = "0";
                MCFGTHREAD_LIB_DIR = "${mcfgthreads}/lib";
                doCheck = false;
                postFixup = ''
                  rm -rf $out/nix-support
                '';
              };
            };
        }
      );
      checks = forAll (
        pkgs:
        {
          # The Core Moss calculus paper: CI builds the same derivation
          # exposed as `nix build .#pdf`.
          core-calculus-pdf = self.packages.${pkgs.stdenv.hostPlatform.system}.pdf;
          # The mechanization of the paper's definitions and metatheory.
          core-calculus-rocq =
            pkgs.runCommand "core-moss-rocq" { nativeBuildInputs = [ pkgs.coq ]; } ''
              export ROCQPATH=${pkgs.coqPackages.stdlib}/lib/coq/${pkgs.coq.coq-version}/user-contrib
              cp ${./docs/design/core/CoreMoss.v} CoreMoss.v
              coqc -q CoreMoss.v
              touch $out
            '';
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
                test ! -e ${self.packages.${pkgs.stdenv.hostPlatform.system}.standalone}/nix-support
                touch $out
              '';
        }
        // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
          standalone =
            pkgs.runCommand "moss-standalone-check"
              {
                nativeBuildInputs = [ pkgs.darwin.cctools ];
              }
              ''
                package=${self.packages.${pkgs.stdenv.hostPlatform.system}.standalone}
                ! otool -L "$package/bin/moss" | sed 1d | grep -q /nix/store/
                ! grep -R -a -q /nix/store/ "$package"
                test ! -e "$package/nix-support"
                "$package/bin/moss" --help >/dev/null
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
