{ mkDerivation, base, genvalidity, genvalidity-hspec, hspec, lib
, scientific, validity-scientific
}:
mkDerivation {
  pname = "genvalidity-scientific";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity scientific validity-scientific
  ];
  testHaskellDepends = [ base genvalidity-hspec hspec scientific ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for Scientific";
  license = lib.licenses.mit;
}
