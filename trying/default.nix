{ mkDerivation, base, lib, selective, splitmix, sydtest, validity
, vector
}:
mkDerivation {
  pname = "trying";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base selective splitmix validity vector
  ];
  testHaskellDepends = [ base splitmix sydtest validity vector ];
  license = "unknown";
}
