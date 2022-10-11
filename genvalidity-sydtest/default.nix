{ mkDerivation, base, genvalidity, lib, pretty-show, QuickCheck
, sydtest, sydtest-discover, validity
}:
mkDerivation {
  pname = "genvalidity-sydtest";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity pretty-show QuickCheck sydtest validity
  ];
  testHaskellDepends = [ base genvalidity QuickCheck sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Standard properties for functions on `Validity` types for the sydtest framework";
  license = lib.licenses.mit;
}
