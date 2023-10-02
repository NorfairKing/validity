final: prev:
{

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (final.callPackage ./overrides.nix { });
  });
}
