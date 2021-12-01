with import ./nixpkgs.nix {};

let

  app-src = ../. ;
  server = pkgs.haskellPackages.callCabal2nix "app" ../. {};
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ../. {};

in

  pkgs.runCommand "app" { inherit client server; } ''
    mkdir -p $out/{bin,static,data}

    cp ${server}/bin/* $out/bin/
    ${pkgs.closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js

    cp ${app-src}/static/* $out/static/

    ${pkgs.sqlite}/bin/sqlite3 $out/data/database.sqlite < ${app-src}/data/database.sql

  ''

