{ mkDerivation, base, criterion, genvalidity, genvalidity-criterion
, genvalidity-hspec, hspec, lib, QuickCheck, random, text
, validity-text
}:
mkDerivation {
  pname = "genvalidity-text";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity QuickCheck random text validity-text
  ];
  testHaskellDepends = [
    base genvalidity-hspec hspec QuickCheck text
  ];
  benchmarkHaskellDepends = [
    base criterion genvalidity genvalidity-criterion text
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for Text";
  license = lib.licenses.mit;
}
