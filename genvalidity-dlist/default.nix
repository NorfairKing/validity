{ mkDerivation, base, dlist, genvalidity, lib, validity-dlist }:
mkDerivation {
  pname = "genvalidity-dlist";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base dlist genvalidity validity-dlist ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for dlist";
  license = lib.licenses.mit;
}
