{ mkDerivation, base, genvalidity, genvalidity-hspec, hspec, lib
, validity-vector, vector
}:
mkDerivation {
  pname = "genvalidity-vector";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity validity-vector vector
  ];
  testHaskellDepends = [ base genvalidity-hspec hspec vector ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for vector";
  license = lib.licenses.mit;
}
