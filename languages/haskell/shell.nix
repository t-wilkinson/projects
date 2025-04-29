with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    (haskell.packages.ghc884.ghcWithPackages (p: with p; [

      HTTP
      aeson
      ansi-terminal
      async
      criterion
      http-client
      http-conduit
      lens
      lens-aeson
      megaparsec
      neat-interpolation
      optparse-applicative
      optparse-generic
      optparse-simple
      raw-strings-qq
      split

    ]))
  ];
}
