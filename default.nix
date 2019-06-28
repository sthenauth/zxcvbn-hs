{ pkgs ? (import <nixpkgs> {}).pkgs }:
pkgs.haskellPackages.callPackage ./zxcvbn-hs.nix { }
