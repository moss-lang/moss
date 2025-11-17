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
        overlays = [ (import rust-overlay) ];
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
        defaultPname =
          craneLib.crateNameFromCargoToml {
            cargoToml = ./crates/connie/Cargo.toml;
          };
        commonArgs = {
          inherit src;
          strictDeps = true;
        };
        mkPackage =
          args:
          let
            argsWithName =
              args
              // {
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
      in
      {
        packages = {
          default = connieCli;
        }
        // crossPackages;
        checks =
          let
            cargoTestArgs =
              commonArgs
              // {
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
