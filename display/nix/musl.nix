{ pkgs ? (import <nixpkgs> {}).pkgsMusl }:

pkgs.callPackage ./ttds-display.nix {}
