{ pkgs ? import <nixpkgs> {} }:

let

  app = import ./release.nix;

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
    $@
  '';

in

  pkgs.dockerTools.buildLayeredImage {
    name = "draw-canvas";
    tag = "latest";
    config = {
      WorkingDir = "${app}";
      Entrypoint = [ entrypoint ];
      Cmd = [ "${app}/bin/server" ];
    };
  }

