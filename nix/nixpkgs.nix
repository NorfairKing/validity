{ nixpkgsVersion ? import ./nixpkgs-version.nix
}:
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
  inherit (nixpkgsVersion) sha256;
}
