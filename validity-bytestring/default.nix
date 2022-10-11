{ mkDerivation, base, bytestring, lib, validity }:
mkDerivation {
  pname = "validity-bytestring";
  version = "0.4.1.1";
  src = ./.;
  libraryHaskellDepends = [ base bytestring validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for bytestring";
  license = lib.licenses.mit;
}
