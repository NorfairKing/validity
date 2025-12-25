{ mkDerivation, base, bytestring, criterion, deepseq, genvalidity
, genvalidity-criterion, hspec, lib, QuickCheck, random
, validity-bytestring
}:
mkDerivation {
  pname = "genvalidity-bytestring";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring genvalidity QuickCheck random validity-bytestring
  ];
  testHaskellDepends = [
    base bytestring deepseq genvalidity hspec QuickCheck
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion genvalidity-criterion
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for ByteString";
  license = lib.licenses.mit;
}
