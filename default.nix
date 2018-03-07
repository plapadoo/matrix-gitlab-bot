{ pkgs ? import <nixpkgs> {} }:
let
  drv = pkgs.haskellPackages.callCabal2nix "matrix-gitlab" ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv
