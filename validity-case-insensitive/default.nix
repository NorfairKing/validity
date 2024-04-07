{ mkDerivation, base, case-insensitive, genvalidity-hspec, hspec
, lib, validity
}:
mkDerivation {
  pname = "validity-case-insensitive";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base case-insensitive validity ];
  testHaskellDepends = [
    base case-insensitive genvalidity-hspec hspec
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for case-insensitive";
  license = lib.licenses.mit;
}
