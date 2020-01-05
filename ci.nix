let
  pkgs = import ./nix/pkgs.nix {};
  versions = [
    "lts-11_16"
    "lts-12_14"
    "lts-12_23"
    "lts-13_11"
    "lts-13_19"
  ];
  copyPackage = pkgName: pkgPath: ''
    cp ${pkgPath} $out/${pkgName}
  '';
  mkTarget = name: ps:
      pkgs.stdenv.mkDerivation {
        name = name;
        buildCommand = pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList copyPackage ps);
      };
  mkCi = version:
    let
      nixpkgsVersion = import (./ci + "/${version}.nix");
      pkgsf = import ./nix/nixpkgs.nix { inherit nixpkgsVersion; };
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in mkTarget version p.validityPackages;
in pkgs.lib.genAttrs versions mkCi // { current = mkTarget "current" pkgs.validityPackages; }
