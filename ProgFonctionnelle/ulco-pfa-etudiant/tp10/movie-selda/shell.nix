with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      directory
      lucid
      selda
      selda-sqlite
      scotty
      text
      text-show
    ]))
  ];
}

