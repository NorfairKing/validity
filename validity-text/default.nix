{ mkDerivation, base, bytestring, lib, text, validity }:
mkDerivation {
  pname = "validity-text";
  version = "0.3.1.3";
  src = ./.;
  libraryHaskellDepends = [ base bytestring text validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for text";
  license = lib.licenses.mit;
}
