final: prev:
let overrides = (final.callPackage ./overrides.nix { });
in {

  haskell = prev.haskell // {
    packages = builtins.mapAttrs
      (compiler: haskellPackages:
        haskellPackages.override (
          old: { overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) overrides; }

        ))
      prev.haskell.packages;
  };
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) overrides;
  });
}
