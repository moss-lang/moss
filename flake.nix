{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      # Dirty checkouts still link to their base commit. Source archives
      # without Git metadata fall back to the default branch.
      mossRev = self.rev or (nixpkgs.lib.removeSuffix "-dirty" (self.dirtyRev or "main"));
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      # The reference implementation's package and executable name.
      # tools/suite/support.py uses it when building the oracle.
      referenceName = "moss-ref";
      # The reference implementation's toolchain: an OCaml with flambda, the only one that
      # honours the `[@inlined]` the extracted interpreter relies on to keep
      # its memory in check; Rocq and its standard
      # library built with it (a few minutes); and dune. The extracted code
      # links Rocq's runtime library for the primitive integers, floats and
      # arrays, and flambda cannot link what another OCaml compiled, so
      # Rocq has to be built with the same one. A flambda OCaml isn't in the
      # binary cache, so it is built from source; its own test suite is
      # skipped, since it adds minutes to every CI run and tests OCaml, not
      # Moss.
      rocqTools = pkgs: rec {
        ocamlFlambda =
          (pkgs.ocaml-ng.ocamlPackages_4_14.ocaml.override { flambdaSupport = true; }).overrideAttrs
            { doCheck = false; };
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14.overrideScope (_: _: { ocaml = ocamlFlambda; });
        rocqPackages = pkgs.mkRocqPackages (
          pkgs.rocqPackages.rocq-core.override { customOCamlPackages = ocamlPackages; }
        );
        # Build the proof library with the same Rocq kernel as the bootstrap.
        menhirProofs =
          (rocqPackages.MenhirLib.override {
            mkCoqDerivation = args: rocqPackages.mkRocqDerivation args;
          }).overrideAttrs
            (old: {
              postPatch = (old.postPatch or "") + ''
                substituteInPlace coq-menhirlib/src/Makefile \
                  --replace-fail '$(COQBIN)coq_makefile' 'rocq makefile'
              '';
            });
        # Tools for building and auditing the bootstrap.
        inputs = [
          rocqPackages.rocq-core
          rocqPackages.stdlib
          menhirProofs
          ocamlPackages.ocaml
          pkgs.ocaml-ng.ocamlPackages_4_14.menhir
          ocamlPackages.findlib
          ocamlPackages.zarith
          pkgs.dune_3
          pkgs.python3
        ];
      };
    in
    {
      packages = forAll (
        pkgs:
        let
          inherit (pkgs) lib;
          launcherSource = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./launcher
              ./tools/build.py
              ./tools/wasm.py
              ./wasm-features.txt
            ];
          };
          buildTools = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./tools/bootstrap.py
              ./tools/wasm.py
              ./wasm-features.txt
            ];
          };
          wasmTools = [
            pkgs.python3
            pkgs.binaryen
            pkgs.wasm-tools
            pkgs.wasmtime
          ];
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
              ./bootstrap/check.py
              ./bootstrap/dune-project
              (lib.fileset.fileFilter (
                file: file.hasExt "v" || file.hasExt "ml" || file.name == "dune"
              ) ./bootstrap/run)
              ./bootstrap/theories
            ];
          };
          # The reference implementation (bootstrap/run/README.md): the
          # Rocq front end, the executable elaborator, the proved passes and
          # Audit.v's reference interpreter, extracted to OCaml. Its build
          # compiles every theory and audits the assumptions (check.py), in
          # the release build tree the extraction then reuses.
          reference = pkgs.stdenv.mkDerivation {
            pname = referenceName;
            version = "0.0.0";
            src = bootstrapSource;
            nativeBuildInputs = (rocqTools pkgs).inputs;
            DUNE_CACHE = "disabled"; # there is no home to keep it in
            buildPhase = ''
              runHook preBuild
              export DUNE_JOBS=$NIX_BUILD_CORES
              DUNE_PROFILE=release python3 check.py run/driver.exe
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              install -D _build/default/run/driver.exe $out/bin/${referenceName}
              mkdir -p $out/share/moss/audit
              cp _build/Certificate.log _build/Closed.log $out/share/moss/audit/
              runHook postInstall
            '';
            meta.mainProgram = referenceName;
          };
          staticBinaryenFor =
            binaryen:
            binaryen.overrideAttrs (old: {
              doCheck = false;
              nativeBuildInputs = [
                pkgs.cmake
                pkgs.python3
              ];
              nativeCheckInputs = [ ];
              checkInputs = [ ];
              cmakeFlags = (old.cmakeFlags or [ ]) ++ [
                "-DBUILD_SHARED_LIBS=OFF"
                "-DBUILD_TOOLS=OFF"
              ];
            });
          # Nixpkgs ships Wasmtime's C API as a shared library, and only for
          # Unix. The bundles that have to run without Nix need a static one
          # instead. Keep building the command-line tool alongside the C API,
          # even though nothing here installs it: Cargo unifies the features
          # of the `wasmtime` crate across both, and dropping the tool would
          # turn off `component-model-async`, whose tunables the compiler's
          # `.cwasm` records. `wasmtime compile` and this library have to
          # agree on all of them.
          staticWasmtimeFor =
            wasmtime:
            wasmtime.overrideAttrs (old: {
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
          # Keep the raw generations for the tests. The public compiler output
          # remains the optimized, portable module.
          portableCompiler =
            pkgs.runCommand "mossc.wasm"
              {
                outputs = [
                  "out"
                  "details"
                ];
                nativeBuildInputs = wasmTools;
              }
              ''
                export HOME=$TMPDIR
                python3 ${buildTools}/tools/bootstrap.py bootstrap ${compilerSource} "$out" \
                  ${lib.getExe reference} "$details"
              '';
          compilerFromSeed =
            seed:
            pkgs.runCommand "mossc.wasm"
              {
                nativeBuildInputs = wasmTools;
              }
              ''
                export HOME=$TMPDIR
                python3 ${buildTools}/tools/bootstrap.py seeded ${compilerSource} "$out" ${seed}/bin/moss
              '';
          precompile =
            compiler: target:
            pkgs.runCommand "moss-${target}.cwasm"
              {
                nativeBuildInputs = [ pkgs.wasmtime ];
              }
              ''
                export HOME=$TMPDIR
                wasmtime compile --target ${target} -o "$out" ${compiler}
              '';

          # A single launcher build and install layout for each distribution.
          # Keep the Wasmtime machine code in libexec. The launcher can be
          # built and linked independently of compiler changes.
          buildLauncher =
            {
              stdenv,
              binaryen,
              wasmtime,
              platform ? "native",
              extraLibraries ? [ ],
            }:
            stdenv.mkDerivation {
              pname = "moss-launcher";
              version = "0.0.0";
              src = launcherSource;
              nativeBuildInputs = [ pkgs.python3 ];
              buildInputs = [
                binaryen
                wasmtime.dev
                wasmtime.lib
              ]
              ++ extraLibraries;
              MOSS_BINARYEN = binaryen;
              MOSS_WASMTIME_INCLUDE = "${wasmtime.dev}/include";
              MOSS_WASMTIME_LIB = "${wasmtime.lib}/lib";
              buildPhase = ''
                runHook preBuild
                python3 tools/build.py "build/moss${stdenv.hostPlatform.extensions.executable}" \
                  ${lib.escapeShellArg mossRev} ${platform}
                runHook postBuild
              '';
              installPhase = ''
                runHook preInstall
                install -Dm755 build/moss${stdenv.hostPlatform.extensions.executable} \
                  "$out/bin/moss${stdenv.hostPlatform.extensions.executable}"
                runHook postInstall
              '';
              meta.mainProgram = "moss";
            };
          package =
            {
              name,
              launcher,
              compiler,
              target,
              cleanReferences ? [ ],
              fixup ? "",
            }:
            pkgs.runCommand name
              {
                dontStrip = true;
                dontPatchELF = true;
                passthru = { inherit launcher compiler; };
              }
              ''
                mkdir -p "$out/bin" "$out/share/moss" "$out/libexec/moss"
                cp -r ${launcher}/bin/. "$out/bin/"
                cp -r ${./lib} "$out/share/moss/lib"
                cp ${precompile compiler target} "$out/libexec/moss/compiler.cwasm"
                chmod -R u+w "$out"
                ${fixup}
                ${lib.optionalString (cleanReferences != [ ]) ''
                  ${pkgs.removeReferencesTo}/bin/remove-references-to \
                    ${lib.concatMapStringsSep " " (dependency: "-t ${dependency}") cleanReferences} "$out"/bin/*
                  rm -rf "$out/nix-support"
                ''}
              '';
        in
        (rec {
          compiler = portableCompiler;
          ${referenceName} = reference;
          driver = buildLauncher {
            inherit (pkgs) stdenv binaryen wasmtime;
          };
          default = lib.makeOverridable (
            {
              seed ? null,
            }:
            package {
              name = "moss-0.0.0";
              launcher = driver;
              compiler = if seed == null then portableCompiler else compilerFromSeed seed;
              target = pkgs.stdenv.hostPlatform.rust.rustcTarget;
            }
          ) { };
          standalone =
            let
              linux = pkgs.stdenv.hostPlatform.isLinux;
              runtime = if linux then pkgs.pkgsStatic else pkgs;
              binaryen = staticBinaryenFor runtime.binaryen;
              wasmtime = if linux then runtime.wasmtime else staticWasmtimeFor runtime.wasmtime;
              launcher = buildLauncher {
                inherit binaryen wasmtime;
                inherit (runtime) stdenv;
                platform = if linux then "linux-static" else "darwin-static";
                extraLibraries = lib.optional (!linux) (runtime.zstd.override { static = true; });
              };
            in
            package {
              name = "moss-standalone-0.0.0";
              inherit launcher compiler;
              target = runtime.stdenv.hostPlatform.rust.rustcTarget;
              cleanReferences = [
                binaryen
                wasmtime.lib
              ];
              fixup = lib.optionalString (!linux) ''
                ${pkgs.darwin.cctools}/bin/install_name_tool                   -change ${pkgs.libiconv}/lib/libiconv.2.dylib                   /usr/lib/libiconv.2.dylib "$out/bin/moss"
              '';
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
        // lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
          windows =
            let
              cross = import nixpkgs {
                localSystem = pkgs.stdenv.hostPlatform.system;
                crossSystem.config = "x86_64-w64-mingw32";
                config.allowUnsupportedSystem = true;
              };
              binaryen = staticBinaryenFor cross.binaryen;
              wasmtime = staticWasmtimeFor cross.wasmtime;
              launcher = buildLauncher {
                inherit (cross) stdenv;
                inherit binaryen wasmtime;
                platform = "windows";
                extraLibraries = [ (cross.zstd.override { static = true; }) ];
              };
            in
            package {
              name = "moss-windows-0.0.0";
              inherit launcher;
              compiler = portableCompiler;
              target = cross.stdenv.hostPlatform.rust.rustcTarget;
              cleanReferences = [
                binaryen
                wasmtime.lib
              ];
            };
        }
      );
      checks = forAll (
        pkgs:
        {
          # Building the reference implementation already compiles every
          # theory and audits assumptions. Keep the named check without a
          # second proof build.
          bootstrap-rocq = pkgs.runCommand "moss-bootstrap-rocq" { } ''
            audit=${self.packages.${pkgs.stdenv.hostPlatform.system}.${referenceName}}/share/moss/audit
            test -s "$audit/Certificate.log"
            test -s "$audit/Closed.log"
            mkdir -p $out
            cp "$audit/Certificate.log" "$audit/Closed.log" $out/
          '';
          seeded =
            let
              packages = self.packages.${pkgs.stdenv.hostPlatform.system};
              moss = packages.default.override { seed = packages.default; };
            in
            pkgs.runCommand "moss-seeded-check" { } ''
              ${moss}/bin/moss ${./examples/hello.moss} | grep -qx 'Hello, world!'
              touch $out
            '';
          tests =
            let
              packages = self.packages.${pkgs.stdenv.hostPlatform.system};
              source = pkgs.lib.fileset.toSource {
                root = ./.;
                fileset = pkgs.lib.fileset.unions [
                  ./examples
                  ./lib
                  ./src
                  ./tests
                  ./tools
                  ./vscode/syntaxes
                  ./wasm-features.txt
                ];
              };
            in
            pkgs.runCommand "moss-tests"
              {
                nativeBuildInputs = [
                  pkgs.python3
                  pkgs.binaryen
                  pkgs.wasm-tools
                  pkgs.wasmtime
                ];
                MOSS_SEED = packages.compiler;
                MOSS_BOOTSTRAP_DETAILS = packages.compiler.details;
                MOSS_ORACLE = pkgs.lib.getExe packages.${referenceName};
                MOSS_LAUNCHER = pkgs.lib.getExe packages.default;
                MOSS_REV = mossRev;
              }
              ''
                export HOME=$TMPDIR
                cp -r ${source} checkout
                chmod -R u+w checkout
                cd checkout
                python3 tools/test.py
                touch "$out"
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
        default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.python3
            pkgs.binaryen
            pkgs.wasm-tools
            pkgs.wasmtime
          ];
          buildInputs = [
            pkgs.wasmtime.dev
            pkgs.wasmtime.lib
          ];
          MOSS_BINARYEN = pkgs.binaryen;
          MOSS_WASMTIME_INCLUDE = "${pkgs.wasmtime.dev}/include";
          MOSS_WASMTIME_LIB = "${pkgs.wasmtime.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.binaryen
            pkgs.wasmtime.lib
          ];
        };
        bootstrap =
          let
            tools = rocqTools pkgs;
          in
          pkgs.mkShellNoCC {
            buildInputs = tools.inputs ++ [
              (tools.rocqPackages.vsrocq-language-server.override {
                coq = tools.rocqPackages.coq.override {
                  customOCamlPackages = tools.ocamlPackages;
                };
              })
            ];
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
