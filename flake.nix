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
      # The oracle's toolchain: an OCaml with flambda, the only one that
      # honours the `[@inlined]` the extracted interpreter relies on to keep
      # its memory in check (bootstrap/README.md); Rocq and its standard
      # library built with it (a few minutes); and dune. The extracted code
      # links Rocq's runtime library for the primitive integers, floats and
      # arrays, and flambda cannot link what another OCaml compiled, so
      # Rocq has to be built with the same one. A flambda OCaml isn't in the
      # binary cache, so it is built from source; its own test suite is
      # skipped, since it adds minutes to every CI run and tests OCaml, not
      # Moss.
      oracleTools = pkgs: rec {
        ocamlFlambda =
          (pkgs.ocaml-ng.ocamlPackages_4_14.ocaml.override { flambdaSupport = true; }).overrideAttrs
            { doCheck = false; };
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14.overrideScope (_: _: { ocaml = ocamlFlambda; });
        rocqPackages = pkgs.mkRocqPackages (
          pkgs.rocqPackages.rocq-core.override { customOCamlPackages = ocamlPackages; }
        );
        # Everything `dune build` in bootstrap/ needs.
        inputs = [
          rocqPackages.rocq-core
          rocqPackages.stdlib
          ocamlPackages.ocaml
          ocamlPackages.findlib
          pkgs.dune_3
        ];
      };
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
          # test suite and the driver read too: `wasm-features.txt`
          # says what it covers, and why `-all` is not an option.
          wasmFeatures = builtins.filter (line: line != "" && !pkgs.lib.hasPrefix "#" line) (
            pkgs.lib.splitString "\n" (builtins.readFile ./wasm-features.txt)
          );
          wasmOptFlags = pkgs.lib.concatStringsSep " " (
            [ "-mvp" ] ++ map (feature: "--enable-${feature}") wasmFeatures
          );
          # The same features as `wasm-tools validate` (wasmparser, which
          # Wasmtime uses) names them, for validating a module before
          # Binaryen reads it; crates/moss-tests/src/lib.rs and
          # crates/moss-cli/src/binaryen.rs map the same names.
          validatorNames = {
            mutable-globals = [ "mutable-global" ];
            nontrapping-float-to-int = [ "saturating-float-to-int" ];
            sign-ext = [ "sign-extension" ];
            multivalue = [ "multi-value" ];
            bulk-memory = [ "bulk-memory" ];
            reference-types = [ "reference-types" ];
            simd = [ "simd" ];
            gc = [
              "gc"
              "function-references"
            ];
            multimemory = [ "multi-memory" ];
          };
          validatorFeatures = pkgs.lib.concatStringsSep "," (
            [
              "-all"
              "mvp"
            ]
            ++ pkgs.lib.concatMap (feature: validatorNames.${feature}) wasmFeatures
          );
          compilerSource = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./lib
              ./src
            ];
          };
          bootstrapSource = pkgs.lib.fileset.toSource {
            root = ./bootstrap;
            fileset = pkgs.lib.fileset.unions [
              ./bootstrap/assumptions
              ./bootstrap/check.sh
              ./bootstrap/dune-project
              ./bootstrap/ocaml
              ./bootstrap/theories
            ];
          };
          # The bootstrap: the Rocq front end, elaborator and interpreter,
          # extracted to OCaml (bootstrap/README.md).
          oracle = pkgs.stdenv.mkDerivation {
            pname = "moss-oracle";
            version = "0.0.0";
            src = bootstrapSource;
            nativeBuildInputs = (oracleTools pkgs).inputs;
            DUNE_CACHE = "disabled"; # there is no home to keep it in
            buildPhase = ''
              runHook preBuild
              dune build --release -j $NIX_BUILD_CORES ocaml/moss_m2.exe
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              install -D _build/default/ocaml/moss_m2.exe $out/bin/moss_m2
              runHook postInstall
            '';
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
          # The compiler, bootstrapped by the oracle. `moss_m2` reads
          # src/main.moss with the Rocq front end, elaborates it, and
          # interprets it compiling src/main.moss: what it writes is the
          # module the compiler written in src/ makes of itself, S1, without
          # any compiled compiler involved. That takes about 20 minutes and
          # 3 GB; local development reuses a compiler built before
          # (CONTRIBUTING.md).
          #
          # S1, optimized, then compiles src/ again, and the result must be
          # S1 byte for byte: the compiler is a fixpoint of itself, and the
          # interpreted and the compiled compiler agree on the one input
          # that exercises all of it. The raw modules are compared, never
          # the optimized ones; the optimized S1 is the package.
          portableCompiler =
            pkgs.runCommand "mossc.wasm"
              {
                nativeBuildInputs = [
                  pkgs.binaryen
                  pkgs.wasm-tools
                  pkgs.wasmtime
                ];
              }
              ''
                export HOME=$TMPDIR # wasmtime wants a writable cache directory
                # The interpreter recurses as deeply as the program it runs.
                ulimit -s unlimited 2>/dev/null || ulimit -s "$(ulimit -H -s)"
                (cd ${compilerSource} && ${oracle}/bin/moss_m2 --pages 2 \
                  --argv0 lib/prelude.moss src/main.moss src/main.moss) > s1.wasm
                wasm-tools validate --features=${validatorFeatures} s1.wasm
                wasm-opt ${wasmOptFlags} -O3 s1.wasm -o s1-opt.wasm
                wasmtime run --argv0 lib/prelude.moss --dir ${compilerSource}::. \
                  s1-opt.wasm src/main.moss > s2.wasm
                if ! cmp s1.wasm s2.wasm; then
                  echo "the compiler is not a fixpoint: S1 != S2" >&2
                  exit 1
                fi
                cp s1-opt.wasm $out
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
                # The workspace's other crate is the test suite, which the
                # `tests` check runs with a compiler to test.
                cargoTestFlags = [
                  "--package"
                  "moss-cli"
                ];
                buildInputs = [
                  binaryen
                  wasmtime
                ];
                BINARYEN_LIB_DIR = "${binaryen}/lib";
                # The native pass uses Binaryen's typed IR. Headers must come
                # from the exact source revision of the linked library.
                BINARYEN_INCLUDE_DIR = "${binaryen.src}/src";
                WASMTIME_LIB_DIR = "${wasmtime}/lib";
                postInstall = installData;
              }
              // pkgs.lib.optionalAttrs (cwasm != null) { MOSS_COMPILER_CWASM = cwasm; }
              // extra
            );
        in
        (rec {
          compiler = portableCompiler;
          inherit oracle;
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
          # The Rocq development of bootstrap/: every file compiles, and the
          # certificate's assumptions are audited (bootstrap/check.sh says
          # what is enforced). About five minutes and 6 GB.
          bootstrap-rocq =
            pkgs.runCommand "moss-bootstrap-rocq"
              {
                nativeBuildInputs = (oracleTools pkgs).inputs;
                DUNE_CACHE = "disabled"; # there is no home to keep it in
              }
              ''
                cp -r ${self.packages.${pkgs.stdenv.hostPlatform.system}.oracle.src} bootstrap
                chmod -R u+w bootstrap
                export DUNE_JOBS=$NIX_BUILD_CORES
                patchShebangs bootstrap/check.sh
                bootstrap/check.sh
                touch $out
              '';
          cli = pkgs.runCommand "moss-cli-check" { nativeBuildInputs = [ pkgs.wasm-tools ]; } ''
            moss=${self.packages.${pkgs.stdenv.hostPlatform.system}.default}/bin/moss
            source=${./.}/examples/hello.moss
            "$moss" --help >/dev/null
            "$moss" -O3 "$source" | grep -qx 'Hello, world!'
            if "$moss" -O "$source" 2>/dev/null; then
              false
            fi
            # Away from the source tree, the library is the installed one,
            # and a bare import path still finds it (D64).
            cp "$source" hello.moss
            "$moss" hello.moss | grep -qx 'Hello, world!'
            # Even inside another tree with a `lib/` of its own (another
            # checkout or worktree), the library is the compiler's own: a
            # broken prelude above the entry file must not be read.
            mkdir -p other/lib other/src
            echo 'this is not Moss' > other/lib/prelude.moss
            cp "$source" other/src/hello.moss
            "$moss" other/src/hello.moss | grep -qx 'Hello, world!'
            # An invalid module is never written, optimized or not, nor run:
            # a stand-in compiler writes one, which the driver refuses before
            # Binaryen reads it.
            wasm-tools parse ${./tests/cli/invalid-output.wat} -o invalid-output.wasm
            export MOSS_COMPILER=$PWD/invalid-output.wasm
            for level in "" -O0 -O3; do
              if "$moss" $level build hello.moss -o out.wasm 2>err.txt; then
                false
              fi
              test ! -e out.wasm
              grep -q 'function params size is out of bounds' err.txt
              grep -q 'This is a compiler bug' err.txt
            done
            if "$moss" hello.moss 2>err.txt; then
              false
            fi
            grep -q 'This is a compiler bug' err.txt
            unset MOSS_COMPILER
            touch $out
          '';
          # The test suite (crates/moss-tests): every program of tests/ and
          # examples/ compiled and run, the expected errors, the fixpoint,
          # the generated sources, and the oracle's agreement with the
          # compiler, with the compiler built above as the seed.
          tests = pkgs.rustPlatform.buildRustPackage {
            pname = "moss-tests";
            version = "0.0.0";
            src = pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.unions [
                ./Cargo.toml
                ./Cargo.lock
                ./crates
                ./examples
                ./lib
                ./src
                ./tests
                ./vscode/syntaxes
                ./wasm-features.txt
              ];
            };
            cargoLock.lockFile = ./Cargo.lock;
            cargoBuildFlags = [
              "--package"
              "moss-tests"
            ];
            cargoTestFlags = [
              "--package"
              "moss-tests"
            ];
            buildInputs = [
              pkgs.binaryen
              pkgs.wasmtime.lib
            ];
            BINARYEN_LIB_DIR = "${pkgs.binaryen}/lib";
            BINARYEN_INCLUDE_DIR = "${pkgs.binaryen.src}/src";
            WASMTIME_LIB_DIR = "${pkgs.wasmtime.lib}/lib";
            nativeCheckInputs = [
              pkgs.binaryen # `wasm-opt` and `wasm-dis`
              pkgs.wasm-tools # Validates every module the tests write.
              pkgs.wasmtime
            ];
            MOSS_SEED = self.packages.${pkgs.stdenv.hostPlatform.system}.compiler;
            MOSS_ORACLE = "${self.packages.${pkgs.stdenv.hostPlatform.system}.oracle}/bin/moss_m2";
            preCheck = ''
              export HOME=$TMPDIR # wasmtime wants a writable cache directory
            '';
          };
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
          buildInputs =
            let
              tools = oracleTools pkgs;
            in
            tools.inputs
            ++ [
              # VsRocq also needs the Coq compatibility shim, built with the
              # same flambda OCaml as Rocq to avoid mixing runtime libraries.
              (tools.rocqPackages.vsrocq-language-server.override {
                coq = tools.rocqPackages.coq.override {
                  customOCamlPackages = tools.ocamlPackages;
                };
              })
              pkgs.binaryen
              pkgs.stdenv.cc
              pkgs.rust-bin.stable.latest.default
              pkgs.vsce # For manually packaging the VS Code extension.
              pkgs.wasm-tools # The test suite validates every module with it.
              pkgs.wasmtime # The test suite runs every module with it.
            ];
          BINARYEN_LIB_DIR = "${pkgs.binaryen}/lib";
          BINARYEN_INCLUDE_DIR = "${pkgs.binaryen.src}/src";
          WASMTIME_LIB_DIR = "${pkgs.wasmtime.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.binaryen
            pkgs.wasmtime.lib
          ];
          shellHook = ''
            PATH=$PWD/bin:$PATH
          '';
        };
        # For working on bootstrap/: `dune build` there and
        # `bootstrap/check.sh` need nothing else.
        bootstrap = pkgs.mkShellNoCC { buildInputs = (oracleTools pkgs).inputs; };
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
