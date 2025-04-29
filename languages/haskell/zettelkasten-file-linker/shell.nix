let
  pkgs = import <nixpkgs> {};
  haskell = pkgs.haskell.packages.ghc884.ghcWithPackages (p: with p; [
    directory
    case-insensitive
    Glob
    megaparsec
    titlecase
  ]);
in

  pkgs.mkShell {
    buildInputs = [
      haskell
    ];
  }
