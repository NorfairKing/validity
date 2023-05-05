{ mkDerivation, base, lib, pretty-show, QuickCheck, sydcheck
, sydtest, sydtest-discover, validity
}:
mkDerivation {
  pname = "sydcheck-sydtest";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pretty-show sydcheck sydtest validity
  ];
  testHaskellDepends = [ base QuickCheck sydcheck sydtest ];
  testToolDepends = [ sydtest-discover ];
  description = "Standard properties for functions on `Validity` types for the sydtest framework";
  license = "unknown";
}
