{ mkDerivation, base, criterion, genvalidity, genvalidity-criterion
, genvalidity-hspec, hspec, lib, QuickCheck, time, validity-time
}:
mkDerivation {
  pname = "genvalidity-time";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity QuickCheck time validity-time
  ];
  testHaskellDepends = [ base genvalidity-hspec hspec time ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion time
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for time";
  license = lib.licenses.mit;
}
