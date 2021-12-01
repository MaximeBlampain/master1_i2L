with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      aeson
      bytestring
      optparse-applicative
      pandoc
      scalpel
      text
    ]))
  ];
}

