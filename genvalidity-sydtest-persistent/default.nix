{ mkDerivation, base, genvalidity, genvalidity-sydtest
, genvalidity-text, lib, persistent, QuickCheck, sydtest
, sydtest-discover, text, validity
}:
mkDerivation {
  pname = "genvalidity-sydtest-persistent";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-sydtest persistent QuickCheck sydtest
    text
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest genvalidity-text persistent
    QuickCheck sydtest text validity
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for persistent-related instances for sydtest";
  license = lib.licenses.mit;
}
