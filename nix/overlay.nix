final: prev:
let
  overrides = (final.callPackage ./overrides.nix { });
  addOverrides = old: { overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) overrides; };
in
{

  haskell = prev.haskell // {
    packages = builtins.mapAttrs
      (compiler: haskellPackages: haskellPackages.override addOverrides)
      prev.haskell.packages;
  };
  haskellPackages = prev.haskellPackages.override addOverrides;
}
