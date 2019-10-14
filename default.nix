{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "2003332a1e8e518b54e6143f9a9467a8a05abca4";
  };

  nix-hs = import "${nix-hs-src}/default.nix" { inherit pkgs; };

in nix-hs {
  cabal = ./zxcvbn-hs.cabal;
  flags = [ "tools" ];

  overrides = lib: self: super: with lib; {
    pipes-text = unBreak (dontCheck (doJailbreak super.pipes-text));

    optparse-applicative =
      if super ? optparse-applicative_0_15_0_0
        then super.optparse-applicative_0_15_0_0
        else optparse-applicative;

    lens =
      if super ? lens_4_18
        then super.lens_4_18
        else super.lens;
  };
}
