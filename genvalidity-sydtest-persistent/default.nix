{ mkDerivation, base, genvalidity, genvalidity-sydtest, lib
, persistent, QuickCheck, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "genvalidity-sydtest-persistent";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-sydtest persistent QuickCheck sydtest
    text
  ];
  testHaskellDepends = [ base genvalidity sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for persistent-related instances for sydtest";
  license = lib.licenses.mit;
}
