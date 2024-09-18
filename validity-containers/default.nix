{ mkDerivation, base, containers, lib, validity }:
mkDerivation {
  pname = "validity-containers";
  version = "0.5.0.5";
  src = ./.;
  libraryHaskellDepends = [ base containers validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for containers";
  license = lib.licenses.mit;
}
