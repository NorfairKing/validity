{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, system ? builtins.currentSystem
}:
let
  versions = {
    "nixos-21_05" = sources.nixpkgs-21_05;
    "nixos-21_11" = sources.nixpkgs-21_11;
    "nixos-22_05" = sources.nixpkgs-22_05;
  };

  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs system;
      };

    in
    p.validityRelease.overrideAttrs (old: { name = "validity-release-${version}"; });

in
{
  release = pkgs.validityRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { inherit sources; }).run;
} // builtins.mapAttrs mkReleaseForVersion versions
