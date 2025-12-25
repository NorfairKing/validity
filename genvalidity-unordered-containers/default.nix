{ mkDerivation, base, genvalidity, genvalidity-hspec, hashable
, hspec, lib, unordered-containers, validity-unordered-containers
}:
mkDerivation {
  pname = "genvalidity-unordered-containers";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity hashable unordered-containers
    validity-unordered-containers
  ];
  testHaskellDepends = [
    base genvalidity-hspec hspec unordered-containers
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for unordered-containers";
  license = lib.licenses.mit;
}
