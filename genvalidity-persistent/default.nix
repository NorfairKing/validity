{ mkDerivation, base, criterion, deepseq, genvalidity
, genvalidity-containers, genvalidity-criterion, genvalidity-hspec
, hspec, lib, persistent, QuickCheck, validity-containers
, validity-persistent
}:
mkDerivation {
  pname = "genvalidity-persistent";
  version = "1.0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers persistent QuickCheck
    validity-containers validity-persistent
  ];
  testHaskellDepends = [
    base genvalidity-hspec hspec persistent QuickCheck
    validity-containers
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq genvalidity genvalidity-criterion persistent
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for Persistent";
  license = lib.licenses.mit;
}
