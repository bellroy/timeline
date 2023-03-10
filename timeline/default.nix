{ mkDerivation, base, containers, indexed-traversable, lib
, semigroupoids, template-haskell, text, th-compat, time
}:
mkDerivation {
  pname = "timeline";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers indexed-traversable semigroupoids template-haskell
    text th-compat time
  ];
  description = "A simple library for handling data that changes over time";
  license = lib.licenses.bsd3;
}
