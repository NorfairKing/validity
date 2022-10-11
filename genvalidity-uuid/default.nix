{ mkDerivation, base, criterion, genvalidity, genvalidity-criterion
, genvalidity-hspec, hspec, lib, QuickCheck, uuid, validity
, validity-uuid
}:
mkDerivation {
  pname = "genvalidity-uuid";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity QuickCheck uuid validity validity-uuid
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-hspec hspec QuickCheck uuid
  ];
  benchmarkHaskellDepends = [
    base criterion genvalidity genvalidity-criterion QuickCheck uuid
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for UUID";
  license = lib.licenses.mit;
}
