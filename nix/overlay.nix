final: prev:
with final.lib;
with final.haskell.lib;
{

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super:
          let
            validityPkg = name:
              doBenchmark (buildStrictly (self.callPackage (../${name}/default.nix) { }));
            validityPackages =
              final.lib.genAttrs [
                "genvalidity"
                "genvalidity-aeson"
                "genvalidity-bytestring"
                "genvalidity-case-insensitive"
                "genvalidity-containers"
                "genvalidity-criterion"
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

            validityRelease =
              final.symlinkJoin {
                name = "validity-release";
                paths = final.lib.attrValues self.validityPackages;
              };
          } // validityPackages
      );
  });
}
