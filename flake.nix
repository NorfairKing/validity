{
  description = "validity";
  nixConfig = {
    extra-substituters = "https://validity.cachix.org";
    extra-trusted-public-keys = "validity.cachix.org-1:CqZp6vt9ir3yB5f8GAtfkJxPZG8hKC5fhIdaQsf7eZE=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.05";
    nixpkgs-22_11.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=forward-compatibility";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_11
    , nixpkgs-22_05
    , nixpkgs-21_11
    , pre-commit-hooks
    , autodocodec
    , safe-coloured-text
    , sydtest
    }:
    let
      system = "x86_64-linux";
      overlays = [
        self.overlays.${system}
        (import (autodocodec + "/nix/overlay.nix"))
        (import (safe-coloured-text + "/nix/overlay.nix"))
        (import (sydtest + "/nix/overlay.nix"))
      ];
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        inherit overlays;
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.haskellPackages.validityRelease;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.haskellPackages.validityRelease;
          allNixpkgs = {
            inherit
              nixpkgs-22_11
              nixpkgs-22_05
              nixpkgs-21_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs // {
            ghcHEAD = pkgs.haskell.packages.ghcHEAD.validity;
          };
        in
        backwardCompatibilityChecks // {
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "validity-shell";
        packages = p: builtins.attrValues p.validityPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
