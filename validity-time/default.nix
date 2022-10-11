{ mkDerivation, base, lib, time, validity }:
mkDerivation {
  pname = "validity-time";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [ base time validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for time";
  license = lib.licenses.mit;
}
