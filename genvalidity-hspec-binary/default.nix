{ mkDerivation, base, binary, deepseq, genvalidity
, genvalidity-hspec, genvalidity-property, hspec, lib, QuickCheck
, validity
}:
mkDerivation {
  pname = "genvalidity-hspec-binary";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary deepseq genvalidity genvalidity-hspec hspec QuickCheck
  ];
  testHaskellDepends = [
    base binary genvalidity genvalidity-property hspec validity
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Standard spec's for binary-related Instances";
  license = lib.licenses.mit;
}
