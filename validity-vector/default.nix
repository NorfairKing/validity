{ mkDerivation, base, hashable, lib, validity, vector }:
mkDerivation {
  pname = "validity-vector";
  version = "0.2.0.3";
  src = ./.;
  libraryHaskellDepends = [ base hashable validity vector ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for vector";
  license = lib.licenses.mit;
}
