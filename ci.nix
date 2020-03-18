let
  pkgs = import ./nix/pkgs.nix {};
  versions = [
    "lts-13_11"
    "lts-13_19"
    "lts-14_23"
  ];
  copyPackage = pkgName: pkgPath: ''
    cp -r ${pkgPath} $out/${pkgName}
  '';
  mkTarget = name: ps:
      pkgs.stdenv.mkDerivation {
        name = name;
        buildCommand = ''
          mkdir $out

        '' + pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList copyPackage ps);
      };
  mkCi = version:
    let
      nixpkgsVersion = import (./ci + "/${version}.nix");
      pkgsf = import ./nix/nixpkgs.nix { inherit nixpkgsVersion; };
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in mkTarget version p.validityPackages;
in pkgs.lib.genAttrs versions mkCi // { current = mkTarget "current" pkgs.validityPackages; }
