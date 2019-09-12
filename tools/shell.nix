{ pkgs ? import <nixpkgs> { }
}:

let
  drv = import ../default.nix { inherit pkgs; };

  zxcvbn-hs = pkgs.haskell.lib.overrideCabal drv
    (_: {configureFlags = [ "-ftools" ];});

in pkgs.mkShell {
  buildInputs = with pkgs; [
    zxcvbn-hs
  ];
}
