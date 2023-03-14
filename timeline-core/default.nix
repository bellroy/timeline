{ mkDerivation, base, containers, indexed-traversable, lib
, semigroupoids, template-haskell, text, th-compat, time
}:
mkDerivation {
  pname = "timeline-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers indexed-traversable semigroupoids template-haskell
    text th-compat time
  ];
  description = "Core types and functions for use within other timeline-* packages";
  license = lib.licenses.bsd3;
}
