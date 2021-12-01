with import ./nix/nixpkgs.nix {};

let

  server = pkgs.haskellPackages.callCabal2nix "app" ./. {};
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {};

in

  {
    server = server.env;
    client = client.env;
  }
