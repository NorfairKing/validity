{ pkgsf ? import ./nixpkgs.nix {}
}:
let pkgs =
      import pkgsf {
      overlays = [
        ( import ./gitignore-src.nix )  
        ( import ./overlay.nix )
      ];
    };
in pkgs
