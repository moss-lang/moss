{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bun2nix = {
      url = "github:fleek-platform/bun2nix";
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
      bun2nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [
          (import rust-overlay)
          bun2nix.overlays.default
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        lib = pkgs.lib;
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          targets = [
            "x86_64-unknown-linux-musl"
            "x86_64-pc-windows-gnu"
          ];
        };
        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
        src = ./.;
        defaultPname = craneLib.crateNameFromCargoToml {
          cargoToml = ./crates/connie/Cargo.toml;
        };
        commonArgs = {
          inherit src;
          strictDeps = true;
        };
        mkPackage =
          args:
          let
            argsWithName = args // {
              pname = args.pname or defaultPname;
            };
          in
          craneLib.buildPackage (
            argsWithName
            // {
              cargoArtifacts = craneLib.buildDepsOnly argsWithName;
            }
          );
        connieCliArgs = commonArgs // {
          pname = "connie";
          cargoExtraArgs = "--locked --package=connie-cli";
        };
        connieCli = mkPackage connieCliArgs;
        connieDev = mkPackage (
          commonArgs
          // {
            pname = "connie-dev";
            cargoExtraArgs = "--locked --package=connie-dev";
          }
        );
        targetEnvNameUpper = target: lib.toUpper (lib.replaceStrings [ "-" ] [ "_" ] target);
        targetEnvNameLower = target: lib.replaceStrings [ "-" ] [ "_" ] target;
        mkCrossPackage =
          {
            pname,
            target,
            cc,
            ccPrefix,
            extraArgs ? { },
          }:
          let
            envNameUpper = targetEnvNameUpper target;
            envNameLower = targetEnvNameLower target;
            targetArgs = {
              inherit pname;
              cargoExtraArgs = "--locked --package=connie-cli --target ${target}";
              CARGO_BUILD_TARGET = target;
              nativeBuildInputs = [ cc ];
              doCheck = false;
              "AR_${envNameUpper}" = "${cc}/bin/${ccPrefix}-ar";
              "CC_${envNameUpper}" = "${cc}/bin/${ccPrefix}-gcc";
              "CARGO_TARGET_${envNameUpper}_AR" = "${cc}/bin/${ccPrefix}-ar";
              "CARGO_TARGET_${envNameUpper}_LINKER" = "${cc}/bin/${ccPrefix}-gcc";
              "AR_${envNameLower}" = "${cc}/bin/${ccPrefix}-ar";
              "CC_${envNameLower}" = "${cc}/bin/${ccPrefix}-gcc";
            }
            // extraArgs;
          in
          mkPackage (commonArgs // targetArgs);
        crossPackages = lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
          musl = mkCrossPackage {
            pname = "connie-musl";
            target = "x86_64-unknown-linux-musl";
            cc = pkgs.pkgsStatic.stdenv.cc;
            ccPrefix = "x86_64-unknown-linux-musl";
            extraArgs = {
              RUSTFLAGS = "-C target-feature=+crt-static";
            };
          };
          windows = mkCrossPackage {
            pname = "connie-windows";
            target = "x86_64-pc-windows-gnu";
            cc = pkgs.pkgsCross.mingwW64.stdenv.cc;
            ccPrefix = "x86_64-w64-mingw32";
            extraArgs = {
              buildInputs = [
                pkgs.pkgsCross.mingwW64.windows.pthreads
              ];
            };
          };
        };
        devTests = pkgs.runCommand "connie-dev-tests" { } ''
          set -euo pipefail
          cp -r ${src} source
          chmod -R +w source
          cd source
          ${connieDev}/bin/dev test --binary ${connieCli}/bin/connie --skip-cargo-tests
          touch $out
        '';
        workspacePackageJson = ./package.json;
        bunDeps =
          let
            bunNixSource = pkgs.runCommand "bun-nix-src" { } ''
              set -euo pipefail

              workDir="$(pwd)/work"
              mkdir -p "$workDir"
              cp ${./bun.lock} "$workDir/bun.lock"
              cp ${./package.json} "$workDir/package.json"
              cp -R ${./packages} "$workDir/packages"

              cd "$workDir"
              ${pkgs.bun2nix}/bin/bun2nix -l bun.lock -o bun.nix

              mkdir -p "$out"
              cp bun.nix "$out/bun.nix"
              cp -R packages "$out/packages"
            '';
          in
          pkgs.bun2nix.fetchBunDeps {
            bunNix = "${bunNixSource}/bun.nix";
          };
        vscodePackage = builtins.fromJSON (builtins.readFile ./packages/connie-vscode/package.json);
        hostVsceTargets = {
          "x86_64-linux" = "linux-x64";
          "aarch64-linux" = "linux-arm64";
          "x86_64-darwin" = "darwin-x64";
          "aarch64-darwin" = "darwin-arm64";
        };
        hostVsceTarget = hostVsceTargets.${pkgs.stdenv.hostPlatform.system} or null;
        defaultVsixTargets = [
          {
            target = "linux-x64";
            exeExtension = "";
            binaryDrv = crossPackages.musl or null;
          }
          {
            target = "win32-x64";
            exeExtension = ".exe";
            binaryDrv = crossPackages.windows or null;
          }
        ];
        hostVsixTargets =
          lib.optional
            (hostVsceTarget != null && (!lib.any (t: t.target == hostVsceTarget) defaultVsixTargets))
            {
              target = hostVsceTarget;
              exeExtension = if lib.hasPrefix "win32" hostVsceTarget then ".exe" else "";
              binaryDrv = connieCli;
            };
        availableVsixTargets = lib.filter (t: t.binaryDrv != null) (defaultVsixTargets ++ hostVsixTargets);
        mkVsixEntry =
          targetSpec:
          let
            vsixFile = "${vscodePackage.name}-${vscodePackage.version}-${targetSpec.target}.vsix";
            binName = "connie${targetSpec.exeExtension}";
            drv = pkgs.bun2nix.mkDerivation {
              pname = "${vscodePackage.name}-${targetSpec.target}";
              version = vscodePackage.version;
              packageJson = workspacePackageJson;
              inherit src bunDeps;
              strictDeps = true;
              nativeBuildInputs = [
                pkgs.bun
                pkgs.nodejs
                pkgs.gitMinimal
                pkgs.bun2nix.hook
              ];
              buildPhase = ''
                runHook preBuild
                set -euo pipefail

                binDir=packages/connie-vscode/bin
                rm -rf "$binDir"
                mkdir -p "$binDir"
                install -m755 ${targetSpec.binaryDrv}/bin/${binName} "$binDir/${binName}"

                rm -f packages/connie-vscode/*.vsix
                vsixDir=.nix-vsix
                rm -rf "$vsixDir"
                mkdir -p "$vsixDir"

                bun run --filter=connie-vscode build -- --target ${targetSpec.target}

                producedVsix=packages/connie-vscode/${vscodePackage.name}-${targetSpec.target}-${vscodePackage.version}.vsix
                if [ ! -f "$producedVsix" ]; then
                  echo "expected VSIX at $producedVsix" >&2
                  exit 1
                fi
                cp "$producedVsix" "$vsixDir/${vsixFile}"

                runHook postBuild
              '';
              installPhase = ''
                runHook preInstall
                mkdir -p "$out"
                cp ".nix-vsix/${vsixFile}" "$out/${vsixFile}"
                runHook postInstall
              '';
            };
          in
          {
            inherit drv vsixFile;
            target = targetSpec.target;
          };
        vsixEntries =
          let
            entries = map mkVsixEntry availableVsixTargets;
          in
          assert entries != [ ];
          entries;
        vsixPackages = lib.listToAttrs (
          map (entry: {
            name = "vscode-${entry.target}";
            value = entry.drv;
          }) vsixEntries
        );
      in
      {
        packages =
          (
            {
              default = connieCli;
              vscode = pkgs.linkFarm "connie-vscode" (
                map (entry: {
                  name = entry.vsixFile;
                  path = "${entry.drv}/${entry.vsixFile}";
                }) vsixEntries
              );
            }
            // vsixPackages
          )
          // crossPackages;
        checks =
          let
            cargoTestArgs = commonArgs // {
              pname = "connie-tests";
              cargoExtraArgs = "--locked";
            };
          in
          {
            cargo = craneLib.cargoTest (
              cargoTestArgs
              // {
                cargoArtifacts = craneLib.buildDepsOnly cargoTestArgs;
              }
            );
            e2e = devTests;
          };
        devShells.default = pkgs.mkShellNoCC {
          buildInputs = [
            # Necessary tools.
            pkgs.bun
            pkgs.bun2nix
            pkgs.nodejs # Used by vsce.
            pkgs.python3
            rustToolchain

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
