{ mkDerivation, base, genvalidity, genvalidity-property, hspec
, hspec-core, lib, QuickCheck, transformers
}:
mkDerivation {
  pname = "genvalidity-hspec";
  version = "1.0.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-property hspec hspec-core QuickCheck
    transformers
  ];
  testHaskellDepends = [ base genvalidity hspec ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Standard spec's for GenValidity instances";
  license = lib.licenses.mit;
}
