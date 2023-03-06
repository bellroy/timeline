{ mkDerivation, base, lib }:
mkDerivation {
  pname = "timeline";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "A simple library for handling data that changes over time";
  license = lib.licenses.asl20;
}
