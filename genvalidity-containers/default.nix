{ mkDerivation, base, containers, criterion, genvalidity
, genvalidity-criterion, genvalidity-hspec, genvalidity-property
, hspec, lib, QuickCheck, validity, validity-containers
}:
mkDerivation {
  pname = "genvalidity-containers";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers genvalidity QuickCheck validity validity-containers
  ];
  testHaskellDepends = [
    base containers genvalidity genvalidity-hspec genvalidity-property
    hspec QuickCheck validity validity-containers
  ];
  benchmarkHaskellDepends = [
    base containers criterion genvalidity genvalidity-criterion
    QuickCheck
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for containers";
  license = lib.licenses.mit;
}
