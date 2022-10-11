{ mkDerivation, aeson, base, hspec, lib, validity
, validity-scientific, validity-text, validity-unordered-containers
, validity-vector
}:
mkDerivation {
  pname = "validity-aeson";
  version = "0.2.0.5";
  src = ./.;
  libraryHaskellDepends = [
    aeson base validity validity-scientific validity-text
    validity-unordered-containers validity-vector
  ];
  testHaskellDepends = [ aeson base hspec validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for aeson";
  license = lib.licenses.mit;
}
