{ mkDerivation, base, genvalidity, genvalidity-hspec, hspec, lib
, microlens, QuickCheck
}:
mkDerivation {
  pname = "genvalidity-hspec-optics";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-hspec hspec microlens QuickCheck
  ];
  testHaskellDepends = [ base genvalidity-hspec hspec microlens ];
  homepage = "http://cs-syd.eu";
  description = "Standard spec's for lens";
  license = lib.licenses.mit;
}
