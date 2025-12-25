{ mkDerivation, aeson, base, deepseq, genvalidity
, genvalidity-aeson, genvalidity-hspec, genvalidity-text, hspec
, lib, QuickCheck, text
}:
mkDerivation {
  pname = "genvalidity-hspec-aeson";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base deepseq genvalidity genvalidity-hspec hspec QuickCheck
  ];
  testHaskellDepends = [
    aeson base genvalidity genvalidity-aeson genvalidity-text hspec
    text
  ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for aeson-related instances";
  license = lib.licenses.mit;
}
