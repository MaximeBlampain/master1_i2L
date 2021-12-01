with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      lucid
      sqlite-simple
      scotty
      text
      text-show
    ]))
  ];
}

