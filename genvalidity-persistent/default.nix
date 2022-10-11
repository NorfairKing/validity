{ mkDerivation, base, containers, criterion, deepseq, genvalidity
, genvalidity-containers, genvalidity-criterion, genvalidity-hspec
, hspec, lib, persistent, persistent-template, QuickCheck
, validity-containers, validity-persistent
}:
mkDerivation {
  pname = "genvalidity-persistent";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers genvalidity genvalidity-containers persistent
    QuickCheck validity-containers validity-persistent
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-hspec hspec persistent
    persistent-template QuickCheck validity-containers
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq genvalidity genvalidity-criterion persistent
    persistent-template
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for Persistent";
  license = lib.licenses.mit;
}
