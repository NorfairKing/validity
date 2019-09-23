let pkgs =
      import (import ./nixpkgs.nix) {
      overlays = [
        ( import ./gitignore-src.nix )  
        ( import ./overlay.nix )
      ];
    };
in pkgs
