with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      aeson
      lucid
      sqlite-simple
      postgresql-simple
      selda
      selda-sqlite
      selda-postgresql
      scotty
      text
      text-show
    ]))
  ];
}

