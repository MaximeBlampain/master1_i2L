with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      aeson
      bytestring
      hakyll
      lucid
      optparse-applicative
      pandoc
      scalpel
      text
      text-show
    ]))
  ];
}

