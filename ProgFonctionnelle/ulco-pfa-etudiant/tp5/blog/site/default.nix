{ pkgs ? import <nixpkgs> {} } :
let drv = pkgs.haskellPackages.callCabal2nix "site" ./. {};
in if pkgs.lib.inNixShell then drv.env else drv
