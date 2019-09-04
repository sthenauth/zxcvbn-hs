{ pkgs ? import <nixpkgs> { }
}:

let
  # Fix some packages that are marked as broken:
  unBreak = drv: with pkgs.haskell.lib;
    overrideCabal drv (_: {
      broken  = false;
      patches = [ ];
    });

  # Helpful if you want to override any Haskell packages:
  overrides = self: super: with pkgs.haskell.lib; {
    # pipes-text has gone a bit stale.
    pipes-text = unBreak (dontCheck (doJailbreak super.pipes-text));
  };

  # Apply the overrides from above:
  haskell = pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: {})) overrides; });

in pkgs.haskell.lib.doBenchmark
     (haskell.callPackage ./zxcvbn-hs.nix { })
