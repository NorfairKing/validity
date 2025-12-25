{ mkDerivation, base, case-insensitive, criterion, genvalidity
, genvalidity-criterion, genvalidity-hspec, hspec, lib
, validity-case-insensitive
}:
mkDerivation {
  pname = "genvalidity-case-insensitive";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base case-insensitive genvalidity validity-case-insensitive
  ];
  testHaskellDepends = [
    base case-insensitive genvalidity-hspec hspec
  ];
  benchmarkHaskellDepends = [
    base case-insensitive criterion genvalidity-criterion
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for case-insensitive";
  license = lib.licenses.mit;
}
