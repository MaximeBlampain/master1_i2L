{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "blog";
  src = ./.;

  buildInputs = with pkgs; [
    (haskellPackages.callCabal2nix "site" ./site {})
    (texlive.combine { inherit (texlive) scheme-small; })
    gnumake
    gzip
    librsvg
    pandoc
    pdf2svg
  ];

  installPhase = ''
    mkdir -p $out
    cp -R public $out/
  '';

}
