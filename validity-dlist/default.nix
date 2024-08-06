{ mkDerivation, base, dlist, lib, validity }:
mkDerivation {
  pname = "validity-dlist";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [ base dlist validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for containers";
  license = lib.licenses.mit;
}
