{
  # Use 'nix flake show' to discover the structure of the output.
  # Multiple versions of compiler is supported.
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs = inputs: inputs.bellroy-nix-foss.lib.haskellProject {
    src = ./.;
    supportedCompilers = [ "ghc92" "ghc94" "ghc96" "ghc98" "ghc910" ];
    defaultCompiler = "ghc96";
  };
}
