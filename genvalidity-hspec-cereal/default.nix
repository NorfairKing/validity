{ mkDerivation, base, cereal, deepseq, genvalidity
, genvalidity-hspec, genvalidity-property, hspec, lib, QuickCheck
, validity
}:
mkDerivation {
  pname = "genvalidity-hspec-cereal";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cereal deepseq genvalidity genvalidity-hspec hspec QuickCheck
  ];
  testHaskellDepends = [
    base cereal genvalidity genvalidity-property hspec QuickCheck
    validity
  ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for cereal-related instances";
  license = lib.licenses.mit;
}
