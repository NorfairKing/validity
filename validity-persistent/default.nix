{ mkDerivation, base, hspec, lib, persistent, validity }:
mkDerivation {
  pname = "validity-persistent";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hspec persistent validity ];
  homepage = "http://cs-syd.eu";
  description = "Validity instances for persistent-related types";
  license = lib.licenses.mit;
}
