{ mkDerivation, base, bytestring, criterion, deepseq, genvalidity
, genvalidity-criterion, genvalidity-hspec, hspec, lib, QuickCheck
, random, validity, validity-bytestring
}:
mkDerivation {
  pname = "genvalidity-bytestring";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring genvalidity QuickCheck random validity
    validity-bytestring
  ];
  testHaskellDepends = [
    base bytestring deepseq genvalidity genvalidity-hspec hspec
    QuickCheck validity
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion genvalidity genvalidity-criterion
    QuickCheck
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for ByteString";
  license = lib.licenses.mit;
}
