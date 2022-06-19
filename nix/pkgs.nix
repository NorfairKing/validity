{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
}:
let
  pkgs =
    import nixpkgs {
      inherit system;
      overlays = [
        (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
        (import (sources.autodocodec + "/nix/overlay.nix"))
        (import (sources.safe-coloured-text + "/nix/overlay.nix"))
        (import (sources.sydtest + "/nix/overlay.nix"))
        (import ./overlay.nix)
      ];
      config.allowUnfree = true;
    };
in
pkgs
