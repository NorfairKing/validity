{ mkDerivation, base, containers, lib, selective, splitmix, sydtest
, validity, validity-containers, validity-vector, vector
}:
mkDerivation {
  pname = "sydcheck";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers selective splitmix validity validity-containers
    validity-vector vector
  ];
  testHaskellDepends = [ base splitmix sydtest validity vector ];
  license = "unknown";
}
