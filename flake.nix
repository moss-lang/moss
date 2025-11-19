{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
      crane,
      rust-overlay,
      bun2nix,
    }:
    let
      mkOutputs = system: f: {
        inherit system;
        outputs = f rec {
          version = "0.0.0";
          src = ./.;
          commonArgs = {
            pname = "connie";
            inherit src;
            strictDeps = true;
          };
          # `cargoExtraArgs` default is "--locked": https://crane.dev/API.html
          cliArgs = {
            cargoExtraArgs = "--locked --package=connie-cli";
          };
          devArgs = {
            cargoExtraArgs = "--locked --package=connie-dev";
          };
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (import rust-overlay)
              bun2nix.overlays.default
            ];
          };
          craneLib = crane.mkLib pkgs;
          cacheArgs = {
            # Reuse built deps across `connie-cli`/`connie-dev`/`cargo test`.
            cargoArtifacts = craneLib.buildDepsOnly commonArgs;
          };
          musl =
            target:
            (craneLib.overrideToolchain (
              p: p.rust-bin.stable.latest.default.override { targets = [ target ]; }
            )).buildPackage
              (commonArgs // cliArgs // { CARGO_BUILD_TARGET = target; });
          windows =
            crossSystem:
            let
              pkgs = import nixpkgs {
                localSystem = system;
                crossSystem.config = crossSystem;
              };
            in
            (crane.mkLib pkgs).buildPackage (commonArgs // cliArgs);
          macos =
            package:
            pkgs.runCommand "connie-standalone" { nativeBuildInputs = [ pkgs.darwin.cctools ]; } ''
              mkdir -p $out/bin
              cp ${package}/bin/connie $out/bin/connie
              install_name_tool -change ${pkgs.libiconv}/lib/libiconv.2.dylib /usr/lib/libiconv.2.dylib $out/bin/connie
            '';
          bunDeps = pkgs.bun2nix.fetchBunDeps {
            bunNix = "${
              pkgs.runCommand "connie-bun-nix" { } ''
                mkdir $out
                ln -s ${./packages} $out/packages
                ${pkgs.bun2nix}/bin/bun2nix -l ${./bun.lock} -o $out/bun.nix
              ''
            }/bun.nix";
          };
          vsixRaw = pkgs.bun2nix.mkDerivation rec {
            pname = "connie-vsix";
            inherit version src bunDeps;
            packageJson = ./package.json;
            strictDeps = true;
            nativeBuildInputs = [ pkgs.nodejs ];
            buildPhase = ''
              runHook preBuild
              bun run --filter=connie-vscode build
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              mv packages/connie-vscode/connie-vscode-${version}.vsix $out
              runHook postInstall
            '';
          };
          vsix =
            exe:
            vsixRaw.overrideAttrs (_: {
              preBuild = ''
                mkdir packages/connie-vscode/bin
                cp ${exe} packages/connie-vscode/bin/
              '';
            });
          packages = {
            default = craneLib.buildPackage (commonArgs // cacheArgs // cliArgs);
            vscode =
              let
                vscodeExtPublisher = "samestep";
                vscodeExtName = "connie-vscode";
                vscodeExtUniqueId = "${vscodeExtPublisher}.${vscodeExtName}";
              in
              pkgs.runCommand vscodeExtName
                {
                  inherit
                    version
                    vscodeExtPublisher
                    vscodeExtName
                    vscodeExtUniqueId
                    ;
                  nativeBuildInputs = [ pkgs.unzip ];
                }
                ''
                  mkdir -p $out/share/vscode/extensions
                  unzip ${vsixRaw}
                  ln -s ${packages.default}/bin extension/bin
                  mv extension $out/share/vscode/extensions/${vscodeExtUniqueId}
                '';
          };
          checks = {
            cargo = craneLib.cargoTest (commonArgs // cacheArgs);
            e2e =
              let
                dev = craneLib.buildPackage (commonArgs // cacheArgs // devArgs);
              in
              pkgs.runCommand "connie-dev-test" { } ''
                cd ${src}
                ${dev}/bin/dev test --binary ${packages.default}/bin/connie --skip-cargo-tests
                touch $out
              '';
            vscode = pkgs.writeText "connie-vscode.json" (
              builtins.toJSON (pkgs.vscode-utils.toExtensionJsonEntry packages.vscode)
            );
          };
          devShells.default = pkgs.mkShellNoCC {
            buildInputs = [
              # Necessary tools.
              pkgs.bun
              pkgs.nodejs # Used by vsce.
              pkgs.python3
              pkgs.rust-bin.stable.latest.default

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
        };
      };
      transpose =
        list:
        builtins.zipAttrsWith (_: entries: builtins.foldl' (acc: entry: acc // entry) { } entries) (
          builtins.map (item: builtins.mapAttrs (_: value: { ${item.system} = value; }) item.outputs) list
        );
    in
    transpose [
      (mkOutputs "x86_64-linux" (mk: {
        packages = rec {
          default = mk.packages.default;
          vscode = mk.packages.vscode;
          standalone = mk.musl "x86_64-unknown-linux-musl";
          windows = mk.windows "x86_64-w64-mingw32";
          vsix-linux-x64 = mk.vsix "${standalone}/bin/connie";
          vsix-win32-x64 = mk.vsix "${windows}/bin/connie.exe";
        };
        checks = mk.checks;
        devShells = mk.devShells;
      }))
      (mkOutputs "aarch64-linux" (mk: {
        packages = rec {
          default = mk.packages.default;
          vscode = mk.packages.vscode;
          standalone = mk.musl "aarch64-unknown-linux-musl";
          vsix-linux-arm64 = mk.vsix "${standalone}/bin/connie";
        };
        checks = mk.checks;
        devShells = mk.devShells;
      }))
      (mkOutputs "x86_64-darwin" (mk: {
        packages = rec {
          default = mk.packages.default;
          standalone = mk.macos default;
          vscode = mk.packages.vscode;
          vsix-darwin-x64 = mk.vsix "${standalone}/bin/connie";
        };
        checks = mk.checks;
        devShells = mk.devShells;
      }))
      (mkOutputs "aarch64-darwin" (mk: {
        packages = rec {
          default = mk.packages.default;
          standalone = mk.macos default;
          vscode = mk.packages.vscode;
          vsix-darwin-arm64 = mk.vsix "${standalone}/bin/connie";
        };
        checks = mk.checks;
        devShells = mk.devShells;
      }))
    ];
}
