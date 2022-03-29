final:
previous:
let
  defCompiler = "ghc${previous.lib.strings.replaceStrings ["."] [""] previous.haskellPackages.ghc.version}";
  gitignoreSrc = final.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    # put the latest commit sha of gitignore Nix library here:
    rev = "5b9e0ff9d3b551234b4f3eb3983744fa354b17f1";
    # use what nix suggests in the mismatch message here:
    sha256 = "01l4phiqgw9xgaxr6jr456qmww6kzghqrnbc7aiiww3h6db5vw53";
  };
  inherit (import gitignoreSrc { inherit (final) lib; }) gitignoreSource;
in with final.haskell.lib;
{
  validityPackages =
    compiler:
    let validityPkg = name:
      doBenchmark (buildStrictly (final.haskell.packages.${compiler}.callCabal2nixWithOptions name (gitignoreSource (../. + "/${name}")) "--no-hpack" { }));
    in
    final.lib.genAttrs [
      "genvalidity"
      "genvalidity-aeson"
      "genvalidity-bytestring"
      "genvalidity-containers"
      "genvalidity-criterion"
      "genvalidity-hspec"
      "genvalidity-hspec-aeson"
      "genvalidity-hspec-binary"
      "genvalidity-hspec-cereal"
      "genvalidity-hspec-hashable"
      "genvalidity-hspec-optics"
      "genvalidity-hspec-persistent"
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
      "validity-containers"
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
      paths = final.lib.attrValues (final.validityPackages defCompiler);
    };

  haskell = previous.haskell // {
    packages = final.lib.mapAttrs
      (compiler: haskellPackages:
        haskellPackages.override (
          old:
          {
            overrides =
              final.lib.composeExtensions
                (
                  old.overrides or (
                    _:
                    _:
                    { }
                  )
                )
                (
                  _:
                  _:
                    final.validityPackages compiler
                    // {
                      autoexporter = previous.haskell.lib.doJailbreak haskellPackages.autoexporter;
                    }
                );
          }
        )
      )
      previous.haskell.packages;
  };

  haskellPackages = final.haskell.packages.${defCompiler};

}
