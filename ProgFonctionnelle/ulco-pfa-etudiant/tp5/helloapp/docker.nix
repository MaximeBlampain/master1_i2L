{ pkgs ? import <nixpkgs> {} }:
let
  app = pkgs.callPackage ./default.nix {};
  entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
    $@
  '';
in
  pkgs.dockerTools.buildLayeredImage {
    name = "helloapp";
    tag = "latest";
    config = {
      WorkingDir = "${app}";
      Entrypoint = [ entrypoint ];
      Cmd = [ "${app}/bin/helloapp" ];
    };
  }
