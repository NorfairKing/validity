let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };

  versions = {
    "nixos-21_05" = "ad425b5cfb214f6d94c57638e3fc371d5806562c";
    "nixos-21_11" = "5a2e2471e8163da8e6f2c1dfd50ef9063199c08b";
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
