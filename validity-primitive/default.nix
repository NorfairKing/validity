{ mkDerivation, base, lib, primitive, validity }:
mkDerivation {
  pname = "validity-primitive";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base primitive validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for primitive";
  license = lib.licenses.mit;
}
