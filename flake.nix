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
      # GHC 8.10 and 9.2 are listed in tested-with in the cabal file
      # but are no longer available in our nixpkgs channel. CI
      # (haskell-ci) verifies these older versions still build.
      supportedCompilers = [
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
      ];
      defaultCompiler = "ghc910";
    };
}
