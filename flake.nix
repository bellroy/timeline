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
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        lib = nixpkgs.lib;
        supportedCompilers = [ "ghc925" "ghc944" "ghc8107" ];
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
                    # haskell-language-server = {
                    #   version = "latest";
                    #   configureArgs = ''--constraint "haskell-language-server -dynamic"'';
                    # };
                    hlint = "latest";
                    cabal-fmt = "latest";
                    ormolu = "latest";
                  };
                  buildInputs = with prev; [
                    rnix-lsp
                    nixpkgs-fmt
                  ];
                  withHoogle = true;
                };
              };
            })
        ];
        makePkgs = compilerNixName: import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = makeOverlays compilerNixName;
        };
        makeFlake = compilerNixName: (makePkgs compilerNixName).timeline.flake { };
        defaultFlake = makeFlake (builtins.head supportedCompilers);
      in
      defaultFlake // builtins.listToAttrs
        (
          builtins.map
            (compilerNixName: {
              name = compilerNixName;
              value = makeFlake compilerNixName;
            })
            supportedCompilers
        )
    );

  nixConfig = {
    allow-import-from-derivation = "true";
  };
}
