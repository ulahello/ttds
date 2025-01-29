{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./ttds-web.nix { }
