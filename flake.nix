{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
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
      basic = final: prev: rec {
        version = "0.0.0";
        llvm = prev.llvmPackages_20;
        llvmPrefix = llvm.llvm.dev;
        llvmConfig = "${llvmPrefix}/bin/llvm-config";
        nativeEnv =
          pkgs:
          {
            LLVM_SYS_201_PREFIX = pkgs.llvmPackages_20.llvm.dev;
            LLVM_CONFIG_PATH = "${pkgs.llvmPackages_20.llvm.dev}/bin/llvm-config";
          }
          // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isStatic {
            LLVM_SYS_STATIC = "ON";
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
              builtins.any (allow: relative == allow || prev.lib.hasPrefix "${allow}/" relative) paths
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
          buildInputs = [ llvm.llvm prev.zlib prev.libxml2 ];
          env = nativeEnv prev;
        };
        # `cargoExtraArgs` default is "--locked": https://crane.dev/API.html
        cliArgs = {
          cargoExtraArgs = "--locked --package=moss-cli";
        };
        devArgs = {
          cargoExtraArgs = "--locked --package=moss-dev";
        };
        craneLib = crane.mkLib prev;
        cacheArgs = {
          # Reuse built deps across `moss-cli`/`moss-dev`/`cargo test`.
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        };
        b2n = (bun2nix.overlays.default final prev).bun2nix;
        bunDeps = b2n.fetchBunDeps {
          bunNix = "${
            prev.runCommand "moss-bun-nix" { } ''
              mkdir $out
              ln -s ${./packages} $out/packages
              ${b2n}/bin/bun2nix -l ${./bun.lock} -o $out/bun.nix
            ''
          }/bun.nix";
        };
        vsixRaw = b2n.mkDerivation rec {
          pname = "moss-vsix";
          inherit version bunDeps;
          src = filterSource [ "/package.json" "/bun.lock" "/packages" ] ./.;
          packageJson = ./package.json;
          strictDeps = true;
          nativeBuildInputs = [ final.nodejs ];
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
        packages = {
          default = craneLib.buildPackage (commonArgs // cacheArgs // cliArgs);
          vscode = prev.vscode-utils.buildVscodeExtension rec {
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
      };
      mkOutputs = system: f: {
        inherit system;
        outputs =
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ (import rust-overlay) ];
            };
            mk = basic pkgs pkgs;
          in
          with mk;
          f (
            mk
            // rec {
              musl =
                target:
                (craneLib.overrideToolchain (
                  p: p.rust-bin.stable.latest.default.override { targets = [ target ]; }
                )).buildPackage
                  (commonArgs // cliArgs // {
                    CARGO_BUILD_TARGET = target;
                    env =
                      (nativeEnv pkgs)
                      // {
                        LLVM_SYS_STATIC = "ON";
                      }
                      // (
                        if target == "x86_64-unknown-linux-musl" then {
                          LLVM_SYS_201_PREFIX = pkgs.pkgsCross.musl64.llvmPackages_20.llvm.dev;
                          LLVM_CONFIG_PATH = "${pkgs.pkgsCross.musl64.llvmPackages_20.llvm.dev}/bin/llvm-config";
                        } else
                          { }
                      );
                    buildInputs =
                      if target == "x86_64-unknown-linux-musl" then
                        [
                          pkgs.pkgsCross.musl64.llvmPackages_20.llvm
                          pkgs.pkgsCross.musl64.zlib
                          pkgs.pkgsCross.musl64.libxml2
                        ]
                      else
                        commonArgs.buildInputs;
                  });
              windows =
                crossSystem:
                let
                  pkgs = import nixpkgs {
                    localSystem = system;
                    crossSystem.config = crossSystem;
                  };
                in
                (crane.mkLib pkgs).buildPackage (commonArgs // cliArgs // { env = nativeEnv pkgs; });
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
              vsix =
                exe:
                vsixRaw.overrideAttrs (_: {
                  preBuild = ''
                    mkdir packages/moss-vscode/bin
                    cp ${exe} packages/moss-vscode/bin/
                  '';
                });
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
                  llvm.clang
                  llvm.llvm
                  pkgs.zlib
                  pkgs.libxml2

                  # Convenient tools.
                  pkgs.binaryen
                  pkgs.wasm-tools
                  pkgs.wasmtime
                ];
                shellHook = ''
                  PATH=$PWD/bin:$PATH
                  export LLVM_SYS_201_PREFIX=${llvm.llvm.dev}
                  export LLVM_CONFIG_PATH=${llvm.llvm.dev}/bin/llvm-config
                '';
              };
            }
          );
      };
      transpose =
        list:
        builtins.zipAttrsWith (_: entries: builtins.foldl' (acc: entry: acc // entry) { } entries) (
          builtins.map (item: builtins.mapAttrs (_: value: { ${item.system} = value; }) item.outputs) list
        );
    in
    {
      overlays.default =
        final: prev:
        let
          mk = basic final prev;
        in
        {
          moss = mk.packages.default;
          vscode-extensions = prev.vscode-extensions // {
            moss-lang = (prev.vscode-extensions.moss-lang or { }) // {
              moss-vscode = mk.packages.vscode;
            };
          };
        };
    }
    // transpose [
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
