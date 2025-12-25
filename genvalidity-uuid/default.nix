{ mkDerivation, base, criterion, genvalidity, genvalidity-criterion
, hspec, lib, QuickCheck, uuid, validity-uuid
}:
mkDerivation {
  pname = "genvalidity-uuid";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base genvalidity uuid validity-uuid ];
  testHaskellDepends = [ base genvalidity hspec QuickCheck uuid ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion uuid
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for UUID";
  license = lib.licenses.mit;
}
