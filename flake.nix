{
  description = "A simple library for handling data that changes over time";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        makePackage = haskellPackages: haskellPackages.callPackage ./timeline.nix { };
      in
      rec
      {
        packages = {
          default = makePackage pkgs.haskellPackages;
          ghc884 = makePackage pkgs.haskell.packages.ghc884;
          ghc8107 = makePackage pkgs.haskell.packages.ghc8107;
          ghc902 = makePackage pkgs.haskell.packages.ghc902;
          ghc921 = makePackage pkgs.haskell.packages.ghc921;
        };

        devShells = builtins.mapAttrs
          (_: v: v.env.overrideAttrs (oldAttrs: {
            buildInputs = oldAttrs.buildInputs
              ++ [ pkgs.nixpkgs-fmt ]
              ++ (with pkgs.haskellPackages; [
              cabal-fmt
              cabal-install
              doctest
              haskell-ci
              hlint
            ]);
          }))
          packages;
      });
}