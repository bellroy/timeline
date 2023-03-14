{ mkDerivation, base, hedgehog, lib, time, timeline-core }:
mkDerivation {
  pname = "timeline-hedgehog";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hedgehog time timeline-core ];
  description = "Hedgehog generators for the timeline library";
  license = lib.licenses.bsd3;
}
