{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
              ./wasm-features.txt
            ];
          };
          # The Wasm features Binaryen may use, from the one list the
          # bootstrap tests and the driver read too: `wasm-features.txt`
          # says what it covers, and why `-all` is not an option.
          wasmFeatures = builtins.filter (line: line != "" && !pkgs.lib.hasPrefix "#" line) (
            pkgs.lib.splitString "\n" (builtins.readFile ./wasm-features.txt)
          );
          wasmOptFlags = pkgs.lib.concatStringsSep " " (
            [ "-mvp" ] ++ map (feature: "--enable-${feature}") wasmFeatures
          );
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
          # Nixpkgs ships Wasmtime's C API as a shared library, and only for
          # Unix. The bundles that have to run without Nix need a static one
          # instead. Keep building the command-line tool alongside the C API,
          # even though nothing here installs it: Cargo unifies the features
          # of the `wasmtime` crate across both, and dropping the tool would
          # turn off `component-model-async`, whose tunables the compiler's
          # `.cwasm` records. `wasmtime compile` and this library have to
          # agree on all of them.
          staticWasmtimeFor = wasmtime: wasmtime.overrideAttrs (old: {
            pname = "wasmtime-static";
            doCheck = false;
            # The stock package installs shell completions and runs
            # `wasmtime --version`, neither of which works when the tool is
            # built for another platform.
            doInstallCheck = false;
            postInstall = ''
              moveToOutput lib $lib
              rm -f $lib/lib/*.so{,.*} $lib/lib/*.dylib $lib/lib/*.dll{,.a}
              mkdir $dev
              cp -r target/*/release/build/wasmtime-c-api-impl-*/out/include \
                $dev/include
            '';
            meta = old.meta // {
              platforms = old.meta.platforms ++ [ "x86_64-windows" ];
            };
          });
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
                wasm-opt ${wasmOptFlags} -O3 s0.wasm -o s0-opt.wasm
                wasmtime run --argv0 lib/prelude.moss --dir ${compilerSource}::. \
                  s0-opt.wasm src/main.moss > s1.wasm
                wasm-opt ${wasmOptFlags} -O3 s1.wasm -o $out
              '';
          # Machine code for the compiler, compiled ahead of time by the same
          # Wasmtime the driver links, so the driver can just map it in.
          compilerFor =
            target:
            pkgs.runCommand "moss-${target}.cwasm"
              {
                nativeBuildInputs = [ pkgs.wasmtime ];
              }
              ''
                export HOME=$TMPDIR # wasmtime wants a writable cache directory
                wasmtime compile --target ${target} -o $out ${portableCompiler}
              '';
          # `cwasm` is the compiler's machine code to build into the driver.
          # A bundle that has to run without Nix passes one, so that the
          # executable carries the compiler; `null` leaves the driver looking
          # for `compiler.cwasm` next to its library data at run time, which is
          # what `default` installs. Keeping the two apart there means neither
          # rebuilds when only the other one changes.
          packageFor =
            {
              rustPlatform,
              binaryen,
              wasmtime,
              cwasm ? null,
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
                buildInputs = [
                  binaryen
                  wasmtime
                ];
                BINARYEN_LIB_DIR = "${binaryen}/lib";
                WASMTIME_LIB_DIR = "${wasmtime}/lib";
                postInstall = installData;
              }
              // pkgs.lib.optionalAttrs (cwasm != null) { MOSS_COMPILER_CWASM = cwasm; }
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
          # Just the Rust driver: no compiler baked in, so editing `src` or
          # `lib` does not recompile it.
          #
          # This is the only build that links Wasmtime as a shared library, and
          # so the only one that can afford whole-program LTO. The bundles link
          # its static C API, a Rust staticlib carrying its own copy of the
          # standard library, and fat LTO turns this crate's copy into strong
          # definitions rather than mergeable ones: `rust_eh_personality` and
          # `std::panicking::EMPTY_PANIC` end up multiply defined.
          driver = packageFor {
            inherit (pkgs) rustPlatform;
            binaryen = pkgs.binaryen;
            wasmtime = pkgs.wasmtime.lib;
            pname = "moss-bin";
            extra.CARGO_PROFILE_RELEASE_LTO = "fat";
          };
          # The two halves side by side. Copy rather than symlink the
          # executable: the driver finds the compiler through `current_exe`,
          # which resolves symlinks, and would otherwise look inside
          # `moss-bin`, where there is none. Nix deduplicates the two copies
          # again when the store is optimised.
          #
          # Both halves arrive finished, so this only assembles them: leave
          # `fixupPhase` from stripping the driver a second time or rewriting
          # the `.cwasm`, whose sections Wasmtime reads by name.
          default = pkgs.runCommand "moss-0.0.0"
            {
              dontStrip = true;
              dontPatchELF = true;
            }
            ''
              mkdir -p $out/bin $out/libexec/moss
              cp ${driver}/bin/moss $out/bin/moss
              cp -r ${driver}/share $out/share
              cp ${compilerFor pkgs.stdenv.hostPlatform.rust.rustcTarget} \
                $out/libexec/moss/compiler.cwasm
              chmod -R u+w $out
            '';
          standalone =
            if pkgs.stdenv.hostPlatform.isLinux then
              let
                wasmtime = pkgs.pkgsStatic.wasmtime.lib;
              in
              packageFor {
                rustPlatform = pkgs.pkgsStatic.rustPlatform;
                binaryen = staticBinaryen;
                inherit wasmtime;
                cwasm = compilerFor pkgs.pkgsStatic.stdenv.hostPlatform.rust.rustcTarget;
                pname = "moss-standalone";
                extra = {
                  BINARYEN_STATIC = "1";
                  BINARYEN_STATIC_STDCPP = "1";
                  WASMTIME_STATIC = "1";
                  RUSTFLAGS = pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isx86_64 (
                    "-C relocation-model=static -C link-arg=-no-pie"
                  );
                  postFixup = ''
                    ${pkgs.removeReferencesTo}/bin/remove-references-to \
                      -t ${staticBinaryen} -t ${wasmtime} $out/bin/moss
                    rm -rf $out/nix-support
                  '';
                };
              }
            else
              let
                binaryen = staticBinaryenFor pkgs.binaryen;
                wasmtime = (staticWasmtimeFor pkgs.wasmtime).lib;
              in
              packageFor {
                inherit (pkgs) rustPlatform;
                inherit binaryen wasmtime;
                cwasm = compilerFor pkgs.stdenv.hostPlatform.rust.rustcTarget;
                pname = "moss-standalone";
                extra = {
                  BINARYEN_STATIC = "1";
                  WASMTIME_STATIC = "1";
                  postFixup = ''
                    ${pkgs.darwin.cctools}/bin/install_name_tool \
                      -change ${pkgs.libiconv}/lib/libiconv.2.dylib \
                      /usr/lib/libiconv.2.dylib $out/bin/moss
                    ${pkgs.removeReferencesTo}/bin/remove-references-to \
                      -t ${binaryen} -t ${wasmtime} $out/bin/moss
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
                # Nixpkgs marks Wasmtime as Unix-only, but its C API builds
                # for Windows; `staticWasmtimeFor` widens `meta.platforms`.
                config.allowUnsupportedSystem = true;
                overlays = [ (import rust-overlay) ];
              };
              binaryen = staticBinaryenFor crossPkgs.binaryen;
              wasmtime = (staticWasmtimeFor crossPkgs.wasmtime).lib;
              mcfgthreads =
                crossPkgs.callPackage "${nixpkgs}/pkgs/os-specific/windows/mcfgthreads" { };
            in
            packageFor {
              rustPlatform = crossPkgs.rustPlatform;
              inherit binaryen wasmtime;
              cwasm = compilerFor crossPkgs.stdenv.hostPlatform.rust.rustcTarget;
              pname = "moss-windows";
              extra = {
                buildInputs = [
                  binaryen
                  wasmtime
                  mcfgthreads
                ];
                BINARYEN_STATIC = "1";
                BINARYEN_STATIC_STDCPP = "0";
                WASMTIME_STATIC = "1";
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
          # Compiling is not enough on its own: coqc accepts `Admitted`, so
          # this also audits the assumptions of every theorem (§13 of the
          # file), and checks that every top-level Theorem is audited.
          core-calculus-rocq =
            pkgs.runCommand "core-moss-rocq" { nativeBuildInputs = [ pkgs.coq ]; } ''
              export ROCQPATH=${pkgs.coqPackages.stdlib}/lib/coq/${pkgs.coq.coq-version}/user-contrib
              cp ${./docs/design/core/CoreMoss.v} CoreMoss.v

              coqc -q CoreMoss.v > audit.log 2>&1 || { cat audit.log; exit 1; }
              cat audit.log

              # Every top-level Theorem must have a Print Assumptions line.
              grep -oE '^Theorem [A-Za-z0-9_'"'"']+' CoreMoss.v \
                | cut -d' ' -f2 | sort > theorems.txt
              grep -oE '^Print Assumptions [A-Za-z0-9_'"'"']+' CoreMoss.v \
                | cut -d' ' -f3 | sort > audited.txt
              if ! diff -u theorems.txt audited.txt; then
                echo "FAIL: the audited names (§13) are not exactly the" \
                     "top-level Theorems." >&2
                exit 1
              fi

              # ... and each must report closed under the global context.
              expected=$(wc -l < audited.txt)
              actual=$(grep -c '^Closed under the global context$' audit.log \
                       || true)
              if [ "$expected" -ne "$actual" ]; then
                echo "FAIL: $actual of $expected theorems are closed under" \
                     "the global context; the rest depend on axioms or" \
                     "Admitted lemmas (see the log above)." >&2
                exit 1
              fi
              echo "audit: all $expected theorems closed under the global context"

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
                # Also runs the compiler that the bundle carries precompiled.
                "$executable" -O3 ${./.}/examples/hello.moss | grep -qx 'Hello, world!'
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
                # Also runs the compiler that the bundle carries precompiled.
                "$package/bin/moss" -O3 ${./.}/examples/hello.moss | grep -qx 'Hello, world!'
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
          WASMTIME_LIB_DIR = "${pkgs.wasmtime.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.binaryen
            pkgs.wasmtime.lib
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
