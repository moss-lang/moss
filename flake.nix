{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bun2nix = {
      url = "github:nix-community/bun2nix";
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
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (import rust-overlay)
              bun2nix.overlays.default
            ];
          };
          # Without source filtering, things get rebuilt too often.
          filterSource =
            paths: root:
            builtins.path {
              path = root;
              name = "source";
              filter = (
                path: _:
                let
                  start = builtins.stringLength (toString root);
                  string = toString path;
                  relative = builtins.substring start (builtins.stringLength string - start) string;
                in
                builtins.any (allow: relative == allow || pkgs.lib.hasPrefix "${allow}/" relative) paths
              );
            };
          commonArgs = {
            pname = "moss";
            src = filterSource [
              "/Cargo.toml"
              "/Cargo.lock"
              "/crates"
              "/lib"
            ] ./.;
            strictDeps = true;
          };
          # `cargoExtraArgs` default is "--locked": https://crane.dev/API.html
          cliArgs = {
            cargoExtraArgs = "--locked --package=moss-cli";
          };
          devArgs = {
            cargoExtraArgs = "--locked --package=moss-dev";
          };
          craneLib = crane.mkLib pkgs;
          cacheArgs = {
            # Reuse built deps across `moss-cli`/`moss-dev`/`cargo test`.
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
            pkgs.runCommand "moss" { nativeBuildInputs = [ pkgs.darwin.cctools ]; } ''
              mkdir -p $out/bin
              cp ${package}/bin/moss $out/bin/moss
              install_name_tool -change ${pkgs.libiconv}/lib/libiconv.2.dylib /usr/lib/libiconv.2.dylib $out/bin/moss
            '';
          standalone =
            package:
            pkgs.runCommand "moss-standalone" { nativeBuildInputs = [ pkgs.darwin.cctools ]; } ''
              if otool -L ${package}/bin/moss | sed 1d | grep /nix/store/; then
                false
              fi
              touch $out
            '';
          bunDeps = pkgs.bun2nix.fetchBunDeps {
            bunNix = "${
              pkgs.runCommand "moss-bun-nix" { } ''
                mkdir $out
                ln -s ${./packages} $out/packages
                ${pkgs.bun2nix}/bin/bun2nix -l ${./bun.lock} -o $out/bun.nix
              ''
            }/bun.nix";
          };
          vsixRaw = pkgs.bun2nix.mkDerivation rec {
            pname = "moss-vsix";
            inherit version bunDeps;
            src = filterSource [ "/package.json" "/bun.lock" "/packages" ] ./.;
            packageJson = ./package.json;
            strictDeps = true;
            nativeBuildInputs = [ pkgs.nodejs ];
            buildPhase = ''
              runHook preBuild
              bun run --filter=moss-vscode build
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              mv packages/moss-vscode/moss-vscode-${version}.vsix $out
              runHook postInstall
            '';
          };
          vsix =
            exe:
            vsixRaw.overrideAttrs (_: {
              preBuild = ''
                mkdir packages/moss-vscode/bin
                cp ${exe} packages/moss-vscode/bin/
              '';
            });
          packages = {
            default = craneLib.buildPackage (commonArgs // cacheArgs // cliArgs);
            vscode = pkgs.vscode-utils.buildVscodeExtension rec {
              vscodeExtPublisher = "moss-lang";
              vscodeExtName = "moss-vscode";
              vscodeExtUniqueId = "${vscodeExtPublisher}.${vscodeExtName}";
              pname = vscodeExtUniqueId;
              inherit version;
              src = vsixRaw;
              unpackPhase = ''
                runHook preUnpack
                unzip $src
                ln -s ${packages.default}/bin extension/bin
                runHook postUnpack
              '';
            };
          };
          checks = {
            cargo = craneLib.cargoTest (commonArgs // cacheArgs);
            e2e =
              let
                dev = craneLib.buildPackage (commonArgs // cacheArgs // devArgs);
              in
              pkgs.runCommand "moss-dev-test" { } ''
                cd ${./.}
                ${dev}/bin/dev test --skip-cargo-test --prebuilt ${packages.default}/bin/moss
                touch $out
              '';
            inherit (packages) vscode;
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
        packages = mk.packages // rec {
          standalone = mk.musl "x86_64-unknown-linux-musl";
          windows = mk.windows "x86_64-w64-mingw32";
          vsix-linux-x64 = mk.vsix "${standalone}/bin/moss";
          vsix-win32-x64 = mk.vsix "${windows}/bin/moss.exe";
        };
        checks = mk.checks;
        devShells = mk.devShells;
      }))
      (mkOutputs "aarch64-linux" (mk: {
        packages = mk.packages // rec {
          standalone = mk.musl "aarch64-unknown-linux-musl";
          vsix-linux-arm64 = mk.vsix "${standalone}/bin/moss";
        };
        checks = mk.checks;
        devShells = mk.devShells;
      }))
      (mkOutputs "x86_64-darwin" (mk: rec {
        packages = mk.packages // {
          standalone = mk.macos packages.default;
          vsix-darwin-x64 = mk.vsix "${packages.standalone}/bin/moss";
        };
        checks = mk.checks // {
          standalone = mk.standalone packages.standalone;
        };
        devShells = mk.devShells;
      }))
      (mkOutputs "aarch64-darwin" (mk: rec {
        packages = mk.packages // {
          standalone = mk.macos packages.default;
          vsix-darwin-arm64 = mk.vsix "${packages.standalone}/bin/moss";
        };
        checks = mk.checks // {
          standalone = mk.standalone packages.standalone;
        };
        devShells = mk.devShells;
      }))
    ];
}
