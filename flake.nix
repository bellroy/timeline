{
  # Use 'nix flake show' to discover the structure of the output.
  # Multiple versions of compiler is supported.
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs = inputs: inputs.bellroy-nix-foss.lib.haskellProject {
    cabalPackages = [
      {
        name = "timeline";
        path = ./package.nix;
      }
    ];
    supportedCompilers = [ "ghc8107" "ghc92" "ghc94" ];
    defaultCompiler = "ghc92";
  };
}
