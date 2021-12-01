let

  pkgs = import <nixpkgs> {};
  app-src = ../. ;
  app = pkgs.haskellPackages.callCabal2nix "ptivelo" ../. {};

in

  pkgs.runCommand "app" { inherit app; } ''
    mkdir -p $out/{bin,static}
    cp ${app}/bin/* $out/bin/
    cp ${app-src}/static/* $out/static/
  ''

