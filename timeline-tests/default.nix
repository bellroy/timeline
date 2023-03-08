{ mkDerivation, base, bytestring, containers, hashable, hedgehog
, indexed-traversable, lib, tasty, tasty-discover, tasty-golden
, tasty-hedgehog, tasty-hunit, text, time, timeline
, timeline-hedgehog, transformers
}:
mkDerivation {
  pname = "timeline-tests";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring containers hashable hedgehog indexed-traversable
    tasty tasty-golden tasty-hedgehog tasty-hunit text time timeline
    timeline-hedgehog transformers
  ];
  testToolDepends = [ tasty-discover ];
  doHaddock = false;
  description = "Tests for the timeline library";
  license = lib.licenses.bsd3;
}
