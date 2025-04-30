{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [

    HUnit
    QuickCheck
    aeson
    async
    base
    base-compat-batteries
    bytestring
    cabal-install
    conduit
    constraints
    containers
    criterion
    hspec
    hspec-wai
    http-client
    http-conduit
    http-types
    lens
    lens-aeson
    megaparsec
    mtl
    profunctors
    random
    servant
    servant-client
    stm
    streamly
    time
    unordered-containers
    vector
    wai
    warp

        ]);
in
with pkgs;
stdenv.mkDerivation {
  name = "haskell-funtime-env";
  buildInputs = [ ghc ghcid ];
  # shellHook = ''
  #   eval $(egrep ^export ${ghc}/bin/ghc)
  #   '';
} //   { allowBroken = true; }

