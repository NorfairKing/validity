{ sources ? import ./sources.nix
, pkgsf ? sources.nixpkgs
, system ? builtins.currentSystem
}:
let
  pkgs =
    import pkgsf {
      inherit system;
      overlays = [
        (import ./overlay.nix)
        (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
        (import (sources.safe-coloured-text + "/nix/overlay.nix"))
        (import (sources.sydtest + "/nix/overlay.nix"))
        (import (sources.yamlparse-applicative + "/nix/overlay.nix"))
      ];
      config.allowUnfree = true;
    };
in
pkgs
