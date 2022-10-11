{ mkDerivation, base, criterion, genvalidity, genvalidity-criterion
, genvalidity-sydtest, iproute, lib, network-uri, QuickCheck
, sydtest, sydtest-discover, validity, validity-network-uri
}:
mkDerivation {
  pname = "genvalidity-network-uri";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity iproute network-uri QuickCheck validity
    validity-network-uri
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest network-uri QuickCheck sydtest
    validity-network-uri
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity genvalidity-criterion network-uri
    QuickCheck
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "GenValidity support for URI";
  license = lib.licenses.mit;
}
