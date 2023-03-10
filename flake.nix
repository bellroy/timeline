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
      cabalPackages = [ "timeline" "timeline-hedgehog" "timeline-tests" ];
      supportedCompilers = [ "ghc8107" "ghc926" "ghc944" ];
      defaultCompiler = "ghc926";
    in
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = import inputs.nixpkgs { inherit system; };

        makePackageSet = haskellPackages: haskellPackages.override {
          overrides = final: prev: with nixpkgs.haskell.lib;
            builtins.listToAttrs
              (
                builtins.map
                  (name: {
                    inherit name;
                    value = prev.callPackage (./. + "/${name}") { };
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
          packages = p: builtins.map (name: p.${name}) cabalPackages;
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
                      (name: {
                        name = "${compilerName}-${name}";
                        value = pkgSet.${name};
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
