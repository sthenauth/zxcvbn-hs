{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghcide ? sources.ghcide-nix
, ormolu ? sources.ormolu
, ghc ? "default"
}:

nix-hs {
  cabal = ./zxcvbn-hs.cabal;
  flags = [ "tools" ];
  compiler = ghc;

  overrides = lib: self: super: with lib; {
    pipes-text = unBreak (dontCheck (doJailbreak super.pipes-text));

    ghcide = import ghcide {};

    ormolu = (import ormolu {
      inherit (lib) pkgs;
      ormoluCompiler = lib.compilerName;
    }).ormolu;
  };
}
