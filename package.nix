{ mkDerivation, base, bytestring, containers, hashable, hedgehog
, indexed-traversable, lib, semigroupoids, tasty, tasty-discover
, tasty-golden, tasty-hedgehog, tasty-hunit, template-haskell, text
, th-compat, time, transformers
}:
mkDerivation {
  pname = "timeline";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers hedgehog indexed-traversable semigroupoids
    template-haskell text th-compat time
  ];
  testHaskellDepends = [
    base bytestring containers hashable hedgehog indexed-traversable
    tasty tasty-golden tasty-hedgehog tasty-hunit text time
    transformers
  ];
  testToolDepends = [ tasty-discover ];
  description = "Data type representing a piecewise-constant function over time";
  license = lib.licenses.bsd3;
}
