{
  # Use 'nix flake show' to discover the structure of the output.
  # Multiple versions of compiler is supported.
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs:
    let
      cabalPackages = [
        {
          name = "timeline";
          path = ./package.nix;
        }
      ];
      supportedCompilers = [ "ghc8107" "ghc92" "ghc94" ];
      defaultCompiler = "ghc92";
    in
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = import inputs.nixpkgs { inherit system; };

        makePackageSet = haskellPackages: haskellPackages.override {
          overrides = final: prev: with nixpkgs.haskell.lib;
            builtins.listToAttrs
              (
                builtins.map
                  (cabalPackage: {
                    name = cabalPackage.name;
                    value = prev.callPackage cabalPackage.path { };
                  })
                  cabalPackages
              );
        };

        essentialTools = with nixpkgs; [
          cabal-install
          hlint
          ormolu
          haskellPackages.cabal-fmt
          cabal2nix
          miniserve
        ];

        makeShell = haskellPackages: (makePackageSet haskellPackages).shellFor {
          packages = p: builtins.map (cabalPackage: p.${cabalPackage.name}) cabalPackages;
          withHoogle = true;
          buildInputs = essentialTools ++ [
            nixpkgs.haskellPackages.haskell-language-server
          ];
        };

        lightShell = nixpkgs.mkShell {
          packages = essentialTools ++ [ nixpkgs.ghc ];
        };
      in
      {
        packages =
          let packagesWithoutDefault =
            builtins.listToAttrs
              (
                builtins.concatMap
                  (compilerName:
                    let pkgSet = makePackageSet nixpkgs.haskell.packages.${compilerName};
                    in
                    builtins.map
                      (cabalPackage: {
                        name = "${compilerName}-${cabalPackage.name}";
                        value = pkgSet.${cabalPackage.name};
                      })
                      cabalPackages
                  )
                  supportedCompilers
              );
          in
          packagesWithoutDefault // {
            default = nixpkgs.runCommand "aggregate"
              {
                buildInputs = builtins.map (name: packagesWithoutDefault.${name})
                  (builtins.attrNames packagesWithoutDefault);
              } "touch $out";
          };

        devShells =
          let devShellsWithoutDefault =
            builtins.listToAttrs
              (
                builtins.map
                  (compilerName: {
                    name = compilerName;
                    value = makeShell nixpkgs.haskell.packages.${compilerName};
                  })
                  supportedCompilers
              ); in
          devShellsWithoutDefault // {
            default = devShellsWithoutDefault.${defaultCompiler};
            light = lightShell;
          };
      }
    );
}
