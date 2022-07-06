final:
previous:
with final.haskell.lib;
{
  validityPackages =
    let validityPkg = name:
      doBenchmark (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }));
    in
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

  validityRelease =
    final.symlinkJoin {
      name = "validity-release";
      paths = final.lib.attrValues final.validityPackages;
    };

  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        final.validityPackages
    );
  });
}
