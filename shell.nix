{ pkgs ? (import <nixpkgs> {}).pkgs
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/pjones/nix-hs.git";
    rev = "2cc803c62cdc74a9a0696cc8fa8ffa3fcb420b97";
  };

  nix-hs = (import "${nix-hs-src}/default.nix" {inherit pkgs;});
  zxcvbn-hs = import ./default.nix {inherit pkgs;};

in

pkgs.mkShell {
  buildInputs = with pkgs; [
    nix-hs
    haskellPackages.hlint
    haskellPackages.hasktags
    # cabal-dependency-licenses

    (haskellPackages.ghcWithPackages (hs: [
      zxcvbn-hs
      hs.cabal-install
    ]))
  ];
}
