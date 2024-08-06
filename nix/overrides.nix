{ lib, haskell, symlinkJoin, ... }:
with lib;
with haskell.lib;
self: super:
let
  validityPkg = name:
    doBenchmark (buildStrictly (self.callPackage (../${name}/default.nix) { }));
  validityPackages =
    lib.genAttrs [
      "genvalidity"
      "genvalidity-aeson"
      "genvalidity-bytestring"
      "genvalidity-case-insensitive"
      "genvalidity-containers"
      "genvalidity-criterion"
      "genvalidity-dlist"
      "genvalidity-hspec"
      "genvalidity-hspec-aeson"
      "genvalidity-hspec-binary"
      "genvalidity-hspec-cereal"
      "genvalidity-hspec-hashable"
      "genvalidity-hspec-optics"
      "genvalidity-hspec-persistent"
      "genvalidity-network-uri"
      "genvalidity-path"
      "genvalidity-persistent"
      "genvalidity-property"
      "genvalidity-scientific"
      "genvalidity-sydtest"
      "genvalidity-sydtest-aeson"
      "genvalidity-sydtest-lens"
      "genvalidity-sydtest-persistent"
      "genvalidity-text"
      "genvalidity-time"
      "genvalidity-unordered-containers"
      "genvalidity-uuid"
      "genvalidity-vector"
      "validity"
      "validity-aeson"
      "validity-bytestring"
      "validity-case-insensitive"
      "validity-containers"
      "validity-dlist"
      "validity-network-uri"
      "validity-path"
      "validity-persistent"
      "validity-primitive"
      "validity-scientific"
      "validity-text"
      "validity-time"
      "validity-unordered-containers"
      "validity-uuid"
      "validity-vector"
    ]
      validityPkg;
in
{
  inherit validityPackages;

  validityRelease = symlinkJoin {
    name = "validity-release";
    paths = attrValues self.validityPackages;
    passthru = self.validityPackages;
  };
} // validityPackages
