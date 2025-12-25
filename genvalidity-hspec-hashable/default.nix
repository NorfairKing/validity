{ mkDerivation, base, genvalidity, genvalidity-hspec
, genvalidity-property, hashable, hspec, lib, QuickCheck
}:
mkDerivation {
  pname = "genvalidity-hspec-hashable";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-hspec genvalidity-property hashable
    hspec QuickCheck
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-hspec hashable hspec
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Standard spec's for Hashable instances";
  license = lib.licenses.mit;
}
