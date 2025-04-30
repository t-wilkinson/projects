# example.nix
{ nixpkgs ? <nixpkgs> }:
rec {
  haskell = import nixpkgs (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs;
  pkgNames = haskell.pkgs.lib.attrNames haskell.haskell-nix.snapshots."lts-13.18";
}
