{ mkDerivation, base, genvalidity, genvalidity-hspec, hspec, lib
, persistent, QuickCheck, text
}:
mkDerivation {
  pname = "genvalidity-hspec-persistent";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-hspec hspec persistent QuickCheck text
  ];
  testHaskellDepends = [ base genvalidity hspec ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for persistent-related instances";
  license = lib.licenses.mit;
}
