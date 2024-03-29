{ mkDerivation, base, genvalidity, hspec, lib, pretty-show
, QuickCheck, validity
}:
mkDerivation {
  pname = "genvalidity-property";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity hspec pretty-show QuickCheck validity
  ];
  testHaskellDepends = [ base genvalidity hspec QuickCheck ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Standard properties for functions on `Validity` types";
  license = lib.licenses.mit;
}
