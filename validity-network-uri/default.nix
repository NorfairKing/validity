{ mkDerivation, base, lib, network-uri, validity }:
mkDerivation {
  pname = "validity-network-uri";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base network-uri validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for URI";
  license = lib.licenses.mit;
}
