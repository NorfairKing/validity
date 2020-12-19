{ pkgsf ? import ./nixpkgs.nix {}
}:
let 
    yamlparse-applicative-overlay =    import (      builtins.fetchGit (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"    );
    sydtest-overlay = import (builtins.fetchGit (import ./sydtest-version.nix) + "/nix/overlay.nix");
    pkgs =
      import pkgsf {
      overlays = [
        ( import ./gitignore-src.nix )  
        ( import ./overlay.nix )
        yamlparse-applicative-overlay
        sydtest-overlay
      ];
      config.allowUnfree = true;
    };
in pkgs
