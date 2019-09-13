{ pkgs ? import <nixpkgs> { }
}:

let
  zxcvbn-hs = import ../default.nix { inherit pkgs; };

in pkgs.mkShell {
  buildInputs = with pkgs; [
    zxcvbn-hs
  ];
}
