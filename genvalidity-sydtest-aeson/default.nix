{ mkDerivation, aeson, base, deepseq, genvalidity
, genvalidity-aeson, genvalidity-sydtest, genvalidity-text, lib
, QuickCheck, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "genvalidity-sydtest-aeson";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base deepseq genvalidity genvalidity-sydtest QuickCheck
    sydtest
  ];
  testHaskellDepends = [
    aeson base genvalidity genvalidity-aeson genvalidity-text sydtest
    text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for aeson-related instances in sydtest";
  license = lib.licenses.mit;
}
