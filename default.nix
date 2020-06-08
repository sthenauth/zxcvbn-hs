{ sources ? import ./nix/sources.nix, nixpkgs ? "nixpkgs"
, pkgs ? import sources.${nixpkgs} { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, ghc ? "default" }:

nix-hs {
  cabal = ./zxcvbn-hs.cabal;
  flags = [ "tools" ];
  compiler = ghc;

  overrides = lib: self: super:
    with lib; {
      pipes-text = unBreak (dontCheck (doJailbreak super.pipes-text));
    };
}
