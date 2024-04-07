{ mkDerivation, base, genvalidity, genvalidity-sydtest, hashable
, lib, QuickCheck, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "genvalidity-sydtest-hashable";
  version = "1.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-sydtest hashable QuickCheck sydtest
  ];
  testHaskellDepends = [ base genvalidity hashable sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Standard spec's for Hashable instances for sydtest";
  license = lib.licenses.mit;
}
