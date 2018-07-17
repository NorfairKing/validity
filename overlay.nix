final:
  previous:
    with final.haskell.lib;
    {
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (

          self: super:
            let validityPkg = name:
                (self.callCabal2nix name (./. + "/${name}") {});
            in final.lib.genAttrs [
              "genvalidity"
              "genvalidity-aeson"
              "genvalidity-bytestring"
              "genvalidity-containers"
              "genvalidity-hspec"
              "genvalidity-hspec-aeson"
              "genvalidity-hspec-binary"
              "genvalidity-hspec-cereal"
              "genvalidity-hspec-hashable"
              "genvalidity-path"
              "genvalidity-property"
              "genvalidity-scientific"
              "genvalidity-text"
              "genvalidity-time"
              "genvalidity-unordered-containers"
              "genvalidity-uuid"
              "genvalidity-vector"
              "validity"
              "validity-aeson"
              "validity-bytestring"
              "validity-containers"
              "validity-path"
              "validity-scientific"
              "validity-text"
              "validity-time"
              "validity-unordered-containers"
              "validity-uuid"
              "validity-vector"
            ] validityPkg
        );
      });
    }
