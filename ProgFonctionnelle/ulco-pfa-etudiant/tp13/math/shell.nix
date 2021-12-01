with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [

      aeson
      http-client-tls
      servant
      servant-client
      servant-server
      text
      text-show
      warp
    ]))
  ];
}

