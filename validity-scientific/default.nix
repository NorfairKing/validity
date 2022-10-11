{ mkDerivation, base, lib, scientific, validity }:
mkDerivation {
  pname = "validity-scientific";
  version = "0.2.0.3";
  src = ./.;
  libraryHaskellDepends = [ base scientific validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for scientific";
  license = lib.licenses.mit;
}
