{ mkDerivation, aeson, base, criterion, deepseq, genvalidity
, genvalidity-criterion, genvalidity-hspec, genvalidity-scientific
, genvalidity-text, genvalidity-unordered-containers
, genvalidity-vector, hspec, lib, QuickCheck, validity
, validity-aeson
}:
mkDerivation {
  pname = "genvalidity-aeson";
  version = "1.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base genvalidity genvalidity-scientific genvalidity-text
    genvalidity-unordered-containers genvalidity-vector QuickCheck
    validity validity-aeson
  ];
  testHaskellDepends = [
    aeson base deepseq genvalidity genvalidity-hspec hspec
  ];
  benchmarkHaskellDepends = [
    aeson base criterion genvalidity-criterion
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for aeson";
  license = lib.licenses.mit;
}
