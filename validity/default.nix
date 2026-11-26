{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "validity";
  version = "0.12.2.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity typeclass";
  license = lib.licenses.mit;
}
