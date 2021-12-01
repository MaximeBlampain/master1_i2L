with import ./nix/nixpkgs.nix {};
{
  server = (pkgs.haskellPackages.callCabal2nix "app" ./. {}).env;
  client = (pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {}).env;
}
