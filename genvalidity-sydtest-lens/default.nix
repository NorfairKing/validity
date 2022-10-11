{ mkDerivation, base, genvalidity, genvalidity-sydtest, lib
, microlens, QuickCheck, sydtest, sydtest-discover, validity
}:
mkDerivation {
  pname = "genvalidity-sydtest-lens";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-sydtest microlens QuickCheck sydtest
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest microlens sydtest validity
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for lens for sydtest";
  license = lib.licenses.mit;
}
