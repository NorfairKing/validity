let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };

  versions = {
    "lts-16_11" = "89db531aea80df58584c9a9e3504ffd9617e6b48";
  };

  mkReleaseForVersion = version: rev:
    let
      pkgsf = builtins.fetchGit {
        url = "https://github.com/NixOS/nixpkgs";
        inherit rev;
      };
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in
    p.validityRelease.overrideAttrs (old: { name = "validity-release-${version}"; });

in
{
  release = pkgs.validityRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { inherit sources; }).run;
} // builtins.mapAttrs mkReleaseForVersion versions
