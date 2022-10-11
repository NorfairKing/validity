{ mkDerivation, base, lib, uuid, validity }:
mkDerivation {
  pname = "validity-uuid";
  version = "0.1.0.3";
  src = ./.;
  libraryHaskellDepends = [ base uuid validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for uuid";
  license = lib.licenses.mit;
}
