{ mkDerivation, aeson, base, bytestring, deepseq, genvalidity
, genvalidity-aeson, genvalidity-hspec, genvalidity-property
, genvalidity-text, hspec, lib, QuickCheck, text, validity
}:
mkDerivation {
  pname = "genvalidity-hspec-aeson";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring deepseq genvalidity genvalidity-hspec hspec
    QuickCheck
  ];
  testHaskellDepends = [
    aeson base genvalidity genvalidity-aeson genvalidity-hspec
    genvalidity-property genvalidity-text hspec QuickCheck text
    validity
  ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for aeson-related instances";
  license = lib.licenses.mit;
}
