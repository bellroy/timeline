{
  # The flake not intended to be consumed by downstream direcly. Please fetch from Hackage.
  # Continue reading if you want to develop this package.
  # Useful commands:
  #   - Build with the default copmiler: nix build .#timeline:lib:timeline
  #   - Test: nix build .#checks.x86_64-linux.timeline:test:tests
  #   - Test with other versions of GHC: .#ghc944.checks.x86_64-linux.timeline:test:tests
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      lib = nixpkgs.lib;
      supportedCompilers = [ "ghc944" "ghc926" "ghc8107" ];
      defaultCompiler = "ghc926";
      makeOverlays = compilerNixName: [
        haskellNix.overlay
        (final: prev:
          {
            timeline = final.haskell-nix.project' {
              src = ./.;
              name = "timeline";
              compiler-nix-name = compilerNixName;
              shell = {
                additional = pkgs: [ pkgs.tasty-discover ];
                tools = {
                  cabal = "latest";
                  cabal-fmt = "latest";
                } // (if compilerNixName == "ghc926" then {
                  haskell-language-server = {
                    version = "latest";
                    configureArgs = ''--constraint "haskell-language-server -dynamic"'';
                  };
                  hlint = "latest";
                  ormolu = "latest";
                } else { });
                buildInputs = with prev; [
                  rnix-lsp
                  nixpkgs-fmt
                ];
                withHoogle = true;
              };
            };
          })
      ];
    in
    let
      makeFlake = compilerNixName: flake-utils.lib.eachDefaultSystem (system:
        let pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = makeOverlays compilerNixName;
        };
        in
        pkgs.timeline.flake { });
      defaultFlake = makeFlake defaultCompiler;
    in
    defaultFlake // builtins.listToAttrs
      (
        builtins.map
          (compilerNixName: {
            name = compilerNixName;
            value = makeFlake compilerNixName;
          })
          supportedCompilers
      );

  nixConfig = {
    allow-import-from-derivation = "true";
  };
}
