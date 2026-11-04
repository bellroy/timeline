{
  # Use 'nix flake show' to discover the structure of the output.
  # Multiple versions of compiler is supported.
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs =
    inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      src = ./.;
      # GHC 8.10 is listed in tested-with in the cabal file but is
      # no longer available in our nixpkgs channel. CI
      # (haskell-ci) verifies this older version still builds.
      supportedCompilers = [
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
        "ghc914"
      ];
      defaultCompiler = "ghc914";
    };
}
