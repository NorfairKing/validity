{ mkDerivation, base, criterion, genvalidity, genvalidity-criterion
, genvalidity-hspec, hspec, lib, path, QuickCheck, validity-path
}:
mkDerivation {
  pname = "genvalidity-path";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity path QuickCheck validity-path
  ];
  testHaskellDepends = [ base genvalidity-hspec hspec path ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion path
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for Path";
  license = lib.licenses.mit;
}
