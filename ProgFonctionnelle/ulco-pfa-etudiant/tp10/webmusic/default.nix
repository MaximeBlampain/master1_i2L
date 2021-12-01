{ pkgs ? import <nixpkgs> {} }:
let drv = pkgs.haskellPackages.callCabal2nix "webmusic" ./. {};
in if pkgs.lib.inNixShell then drv.env else drv

