{ mkDerivation, base, hspec, hspec-core, lib, QuickCheck, random
, validity
}:
mkDerivation {
  pname = "genvalidity";
  version = "1.1.1.0";
  src = ./.;
  libraryHaskellDepends = [ base QuickCheck random validity ];
  testHaskellDepends = [ base hspec hspec-core QuickCheck ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Testing utilities for the validity library";
  license = lib.licenses.mit;
}
