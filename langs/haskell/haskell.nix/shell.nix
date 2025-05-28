# shell.nix
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs-1909 haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;
in
  # haskell.haskellPackages.ghcWithPackages (ps: with ps;
  haskell.haskellPackages.ghcWithHoogle (ps: with ps;
    [ ])
