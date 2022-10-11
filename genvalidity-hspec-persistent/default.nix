{ mkDerivation, base, genvalidity, genvalidity-hspec
, genvalidity-property, genvalidity-text, hspec, lib, persistent
, QuickCheck, text, validity
}:
mkDerivation {
  pname = "genvalidity-hspec-persistent";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-hspec hspec persistent QuickCheck text
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-hspec genvalidity-property
    genvalidity-text hspec persistent QuickCheck text validity
  ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for persistent-related instances";
  license = lib.licenses.mit;
}
