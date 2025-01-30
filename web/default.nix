{ pkgs ? import <nixpkgs> {} }:

let drv =  pkgs.haskellPackages.callPackage ./ttds-web.nix { }; in

if pkgs.lib.inNixShell then drv.env else drv
