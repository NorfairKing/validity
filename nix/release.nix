let pkgs = import (../default.nix);
in {
  release = 
    with pkgs.haskellPackages;
    pkgs.stdenv.mkDerivation {
      name = "validity-release";
      buildInputs = pkgs.lib.attrsets.attrValues pkgs.validityPackages;
      buildCommand= ''
        cp -r ${genvalidity-hspec} $out
      '';
    };
}
