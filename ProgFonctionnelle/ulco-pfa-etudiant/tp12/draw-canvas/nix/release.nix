with import ./nixpkgs.nix {};

let

  app-src = ../. ;
  server = pkgs.haskellPackages.callCabal2nix "app" ../. {};
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ../. {};

in

  pkgs.runCommand "app" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin/

    cp ${client}/bin/client.jsexe/all.js $out/static/all.js
    #${pkgs.closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js

    cp ${app-src}/static/* $out/static/
  ''

